# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:artifact_tset.bzl", "ArtifactTSet")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_attr_exported_linker_flags",
    "cxx_attr_preferred_linkage",
    "cxx_platform_supported",
)
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
    "CPreprocessorArgs",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
)
load(
    "@prelude//linking:link_groups.bzl",
    "merge_link_group_lib_info",
)
load(
    "@prelude//linking:link_info.bzl",
    "LibOutputStyle",
    "LinkInfo",
    "LinkInfos",
    "create_merged_link_info",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "create_linkable_graph",
    "create_linkable_graph_node",
    "create_linkable_node",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
    "merge_shared_libraries",
)
load("@prelude//linking:strip.bzl", "strip_object")
load("@prelude//utils:utils.bzl", "filter_and_map_idx")
load(":apple_bundle_types.bzl", "AppleBundleInfo", "AppleBundleTypeDefault")
load(":apple_dsym.bzl", "DSYM_SUBTARGET")
load(":apple_frameworks.bzl", "to_framework_name")
load(":apple_toolchain_types.bzl", "AppleToolchainInfo", "AppleToolsInfo")
load(":apple_utility.bzl", "get_apple_stripped_attr_value_with_default_fallback")
load(":debug.bzl", "AppleDebuggableInfo")

def prebuilt_apple_framework_impl(ctx: AnalysisContext) -> list[Provider]:
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
            [CPreprocessor(args = CPreprocessorArgs(args = ["-F", framework_dir]))],
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
        link_info = LinkInfos(default = link)

        providers.append(create_merged_link_info(
            ctx,
            get_cxx_toolchain_info(ctx).pic_behavior,
            {output_style: link_info for output_style in LibOutputStyle},
        ))

        # Create, augment and provide the linkable graph.
        linkable_graph = create_linkable_graph(
            ctx,
            node = create_linkable_graph_node(
                ctx,
                linkable_node = create_linkable_node(
                    ctx,
                    preferred_linkage = cxx_attr_preferred_linkage(ctx),
                    link_infos = {output_style: link_info for output_style in LibOutputStyle},
                    # TODO(cjhopman): this should be set to non-None
                    default_soname = None,
                ),
                excluded = {ctx.label: None},
            ),
        )
        providers.append(linkable_graph)

    providers.append(merge_link_group_lib_info(deps = ctx.attrs.deps))
    providers.append(merge_shared_libraries(ctx.actions, deps = filter_and_map_idx(SharedLibraryInfo, ctx.attrs.deps)))

    # The default output is the provided framework.
    sub_targets = {
        "distribution": _sanitize_framework_for_app_distribution(ctx, framework_directory_artifact) + providers,
    }

    if ctx.attrs.dsyms:
        sub_targets[DSYM_SUBTARGET] = [DefaultInfo(default_outputs = ctx.attrs.dsyms)]
        providers.append(AppleDebuggableInfo(dsyms = ctx.attrs.dsyms, debug_info_tset = ArtifactTSet()))

    providers.append(DefaultInfo(default_output = framework_directory_artifact, sub_targets = sub_targets))
    providers.append(AppleBundleInfo(
        bundle = framework_directory_artifact,
        bundle_type = AppleBundleTypeDefault,
        skip_copying_swift_stdlib = True,
        contains_watchapp = None,
    ))

    return providers

def _sanitize_framework_for_app_distribution(ctx: AnalysisContext, framework_directory_artifact: Artifact) -> list[Provider]:
    framework_name = to_framework_name(framework_directory_artifact.basename)
    bundle_for_app_distribution = ctx.actions.declare_output(framework_name + ".framework", dir = True)

    apple_tools = ctx.attrs._apple_tools[AppleToolsInfo]
    framework_sanitize_command = cmd_args([
        apple_tools.framework_sanitizer,
        "--input",
        framework_directory_artifact,
        "--output",
        bundle_for_app_distribution.as_output(),
    ])

    if get_apple_stripped_attr_value_with_default_fallback(ctx):
        strip_args = cmd_args("-x")
        stripped = strip_object(ctx, ctx.attrs._apple_toolchain[AppleToolchainInfo].cxx_toolchain_info, framework_directory_artifact.project(framework_name), strip_args, "framework_distribution")
        framework_sanitize_command.add("--replacement-binary", stripped)

    ctx.actions.run(framework_sanitize_command, category = "sanitize_prebuilt_apple_framework")
    providers = [DefaultInfo(default_output = bundle_for_app_distribution)]
    providers.append(AppleBundleInfo(
        bundle = bundle_for_app_distribution,
        bundle_type = AppleBundleTypeDefault,
        skip_copying_swift_stdlib = True,
        contains_watchapp = None,
    ))
    return providers
