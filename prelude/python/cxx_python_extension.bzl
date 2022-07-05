load(
    "@fbcode//buck2/prelude/cxx:comp_db.bzl",
    "create_compilation_database",
)
load(
    "@fbcode//buck2/prelude/cxx:compile.bzl",
    "compile_cxx",
    "create_compile_cmds",
)
load(
    "@fbcode//buck2/prelude/cxx:cxx.bzl",
    "cxx_link_into_shared_library",
    "get_srcs_with_flags",
)
load(
    "@fbcode//buck2/prelude/cxx:cxx_library_utility.bzl",
    "cxx_attr_deps",
    "cxx_attr_linker_flags",
    "cxx_inherited_link_info",
)
load("@fbcode//buck2/prelude/cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo", "CxxToolchainInfo")
load(
    "@fbcode//buck2/prelude/cxx:cxx_types.bzl",
    "CxxRuleConstructorParams",
)
load("@fbcode//buck2/prelude/cxx:headers.bzl", "cxx_get_regular_cxx_headers_layout")
load(
    "@fbcode//buck2/prelude/cxx:omnibus.bzl",
    "add_omnibus_roots",
    "create_native_link_target",
)
load(
    "@fbcode//buck2/prelude/cxx:preprocessor.bzl",
    "cxx_inherited_preprocessor_infos",
    "cxx_private_preprocessor_info",
)
load(
    "@fbcode//buck2/prelude/cxx:resources.bzl",
    "CxxResourceInfo",
    "gather_cxx_resources",
)
load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "LinkArgs",
    "LinkInfo",
    "LinkStyle",
    "MergedLinkInfo",
    "ObjectsLinkable",
    "get_link_args",
)
load(
    "@fbcode//buck2/prelude/linking:linkable_graph.bzl",
    "ForceConsiderationOfOmnibusRoots",
    "LinkableGraph",
    "create_merged_linkable_graph",
)
load("@fbcode//buck2/prelude/python:toolchain.bzl", "PythonPlatformInfo", "get_platform_attr")
load("@fbcode//buck2/prelude/utils:utils.bzl", "flatten", "value_or")
load(":manifest.bzl", "create_manifest_for_source_map")
load(":python_library.bzl", "create_python_library_info", "gather_dep_libraries", "qualify_srcs")

# This extension is basically cxx_library, plus base_module.
# So we augment with default attributes so it has everything cxx_library has, and then call cxx_library_impl and work from that.
def cxx_python_extension_impl(ctx: "context") -> ["provider"]:
    providers = []
    sub_targets = {}

    impl_params = CxxRuleConstructorParams(
        rule_type = "cxx_python_extension",
        headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
        srcs = get_srcs_with_flags(ctx),
    )

    cxx_deps = cxx_attr_deps(ctx)
    link_flags = cxx_attr_linker_flags(ctx)

    # Gather preprocessor inputs.
    (own_pre, _) = cxx_private_preprocessor_info(
        ctx,
        impl_params.headers_layout,
        raw_headers = ctx.attr.raw_headers,
    )
    inherited_pre = cxx_inherited_preprocessor_infos(cxx_deps)

    # Compile objects.
    compile_cmd_output = create_compile_cmds(ctx, impl_params, [own_pre], inherited_pre)
    objects = compile_cxx(ctx, compile_cmd_output.source_commands.src_compile_cmds, pic = True)

    # The name of the extension.
    name = value_or(ctx.attr.module_name, ctx.label.name) + ".so"

    # Compilation DB.
    comp_db = create_compilation_database(ctx, compile_cmd_output.source_commands.src_compile_cmds)
    sub_targets["compilation-database"] = [comp_db]

    # Link extension.
    args = []

    # TODO(agallagher): Support post link flags properly.
    args.extend(link_flags)
    args.extend(objects)
    inherited_link = cxx_inherited_link_info(ctx, cxx_deps)
    extension = cxx_link_into_shared_library(
        ctx,
        name,
        [
            LinkArgs(flags = args),
            # As per v1, we always link against "shared" dependencies when building
            # a shared library.
            get_link_args(inherited_link, LinkStyle("shared")),
        ],
        # Python extensions don't use SONAMEs.
        soname = False,
        category_suffix = "python_extension",
    )
    providers.append(DefaultInfo(
        default_outputs = [extension.output],
        other_outputs = extension.external_debug_paths,
        sub_targets = sub_targets,
    ))

    # If a type stub was specified, create a manifest for export.
    src_type_manifest = None
    if ctx.attr.type_stub != None:
        src_type_manifest = create_manifest_for_source_map(
            ctx,
            "type_stub",
            qualify_srcs(
                ctx.label,
                ctx.attr.base_module,
                {value_or(ctx.attr.module_name, ctx.label.name) + ".pyi": ctx.attr.type_stub},
            ),
        )

    # Export library info.
    python_platform = ctx.attr._python_toolchain[PythonPlatformInfo]
    cxx_platform = ctx.attr._cxx_toolchain[CxxPlatformInfo]
    cxx_toolchain = ctx.attr._cxx_toolchain[CxxToolchainInfo]
    raw_deps = (
        [ctx.attr.deps] +
        get_platform_attr(python_platform, cxx_platform, ctx.attr.platform_deps)
    )
    deps, shared_deps = gather_dep_libraries(raw_deps)
    providers.append(create_python_library_info(
        ctx.actions,
        ctx.label,
        extensions = qualify_srcs(ctx.label, ctx.attr.base_module, {name: extension}),
        deps = deps,
        shared_libraries = shared_deps,
        src_types = src_type_manifest,
    ))

    # Omnibus providers
    linkable_graph = create_merged_linkable_graph(ctx.label, flatten(raw_deps))

    # Handle the case where C++ Python extensions depend on other C++ Python
    # extensions, which should also be treated as roots.
    add_omnibus_roots(
        linkable_graph,
        [
            dep
            for dep in flatten(raw_deps)
            # We only want to handle C++ Python extension deps, but not other native
            # linkable deps like C++ libraries.
            if dep[ForceConsiderationOfOmnibusRoots] != None or (dep[LinkableGraph] != None and dep[MergedLinkInfo] == None)
        ],
    )
    providers.append(linkable_graph)
    providers.append(create_native_link_target(
        link_info = LinkInfo(linkables = [
            ObjectsLinkable(
                objects = objects,
                linker_type = cxx_toolchain.linker_info.type,
                link_whole = True,
            ),
        ]),
        deps = cxx_deps,
    ))

    # C++ resources.
    providers.append(CxxResourceInfo(resources = gather_cxx_resources(
        label = ctx.label,
        deps = flatten(raw_deps),
    )))

    return providers
