load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_attr_exported_linker_flags",
    "cxx_platform_supported",
)
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkInfo",
    "LinkInfos",
    "LinkStyle",
    "Linkage",
    "create_merged_link_info",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "create_linkable_graph",
    "create_linkable_graph_node",
    "create_linkable_node",
)
load(":apple_bundle_types.bzl", "AppleBundleInfo")
load(":apple_frameworks.bzl", "to_framework_name")

def prebuilt_apple_framework_impl(ctx: "context") -> ["provider"]:
    providers = []

    framework_directory_artifact = ctx.attrs.framework

    # Check this rule's `supported_platforms_regex` with the current platform.
    if cxx_platform_supported(ctx):
        # Sandbox the framework, to avoid leaking other frameworks via search paths.
        framework_name = to_framework_name(framework_directory_artifact.basename)
        framework_dir = ctx.actions.symlinked_dir(
            "Frameworks",
            {framework_name + ".framework": framework_directory_artifact},
        )

        # Add framework & pp info from deps.
        inherited_pp_info = cxx_inherited_preprocessor_infos(ctx.attrs.deps)
        providers.append(cxx_merge_cpreprocessors(
            ctx,
            [CPreprocessor(args = ["-F", framework_dir])],
            inherited_pp_info,
        ))

        # Add framework to link args.
        # TODO(T110378120): Support shared linking for mac targets:
        # https://fburl.com/code/pqrtt1qr.
        args = []
        args.extend(cxx_attr_exported_linker_flags(ctx))
        args.extend(["-F", framework_dir])
        args.extend(["-framework", framework_name])
        link = LinkInfo(
            name = framework_name,
            pre_flags = args,
        )
        providers.append(create_merged_link_info(
            ctx,
            {link_style: LinkInfos(default = link) for link_style in LinkStyle},
        ))

        # Create, augment and provide the linkable graph.
        linkable_graph = create_linkable_graph(
            ctx,
            node = create_linkable_graph_node(
                ctx,
                linkable_node = create_linkable_node(
                    ctx,
                    preferred_linkage = Linkage("shared"),
                    link_infos = {LinkStyle("shared"): LinkInfos(default = link)},
                ),
                excluded = {ctx.label: None},
            ),
        )
        providers.append(linkable_graph)

    # The default output is the provided framework.
    providers.append(DefaultInfo(default_outputs = [framework_directory_artifact]))
    providers.append(AppleBundleInfo(bundle = framework_directory_artifact, is_watchos = None))

    return providers
