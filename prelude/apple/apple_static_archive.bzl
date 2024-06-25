# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:artifact_tset.bzl", "make_artifact_tset")
load("@prelude//:validation_deps.bzl", "VALIDATION_DEPS_ATTR_NAME", "VALIDATION_DEPS_ATTR_TYPE", "get_validation_deps_outputs")
load("@prelude//apple:apple_library.bzl", "AppleLibraryForDistributionInfo", "AppleLibraryInfo")
load("@prelude//apple:apple_rules_impl_utility.bzl", "get_apple_toolchain_attr")
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//linking:link_info.bzl", "LinkStrategy", "get_link_args_for_strategy", "unpack_link_args")
load("@prelude//linking:linkables.bzl", "linkables")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")
load("@prelude//utils:arglike.bzl", "ArgLike")

def _impl(ctx: AnalysisContext) -> list[Provider]:
    libtool = ctx.attrs._apple_toolchain[AppleToolchainInfo].libtool
    archive_name = ctx.attrs.name if ctx.attrs.archive_name == None else ctx.attrs.archive_name
    output = ctx.actions.declare_output(archive_name)

    artifacts = _get_static_link_args(ctx)
    validation_deps_outputs = get_validation_deps_outputs(ctx)

    #TODO(T193127271): Support thin archives
    cmd = cmd_args([libtool, "-static", "-o", output.as_output(), artifacts], hidden = validation_deps_outputs or [])
    ctx.actions.run(cmd, category = "libtool", identifier = output.short_path)

    providers = [DefaultInfo(default_output = output), _get_apple_library_info(ctx)] + _get_apple_library_for_distribution_info(ctx)

    return providers

def _get_apple_library_for_distribution_info(ctx: AnalysisContext) -> list[AppleLibraryForDistributionInfo]:
    if ctx.attrs.distribution_flat_dep != None:
        apple_library_for_distribution = ctx.attrs.distribution_flat_dep.get(AppleLibraryForDistributionInfo)
        if apple_library_for_distribution != None:
            return [apple_library_for_distribution]
    return []

def _get_apple_library_info(ctx: AnalysisContext) -> AppleLibraryInfo:
    all_flat_deps = filter(None, ctx.attrs.flat_deps + [ctx.attrs.distribution_flat_dep])
    flat_apple_library_infos = filter(None, [dep.get(AppleLibraryInfo) for dep in all_flat_deps])
    flat_public_framework_headers = []
    for apple_library_info in flat_apple_library_infos:
        tset = apple_library_info.public_framework_headers._tset
        if tset != None:
            for headers in tset.value:
                flat_public_framework_headers += headers.artifacts

    flat_header_tset = make_artifact_tset(
        actions = ctx.actions,
        label = ctx.label,
        artifacts = flat_public_framework_headers,
    )

    apple_library_infos = filter(None, [dep.get(AppleLibraryInfo) for dep in ctx.attrs.deps])
    public_framework_header_tset = make_artifact_tset(
        actions = ctx.actions,
        label = ctx.label,
        children = [apple_library.public_framework_headers for apple_library in apple_library_infos] + [flat_header_tset],
    )

    swift_header = None
    if ctx.attrs.distribution_flat_dep != None:
        distribution_flat_dep_apple_library_info = ctx.attrs.distribution_flat_dep.get(AppleLibraryInfo)
        if distribution_flat_dep_apple_library_info:
            swift_header = distribution_flat_dep_apple_library_info.swift_header

    return AppleLibraryInfo(public_framework_headers = public_framework_header_tset, swift_header = swift_header)

def _get_static_link_args(ctx: AnalysisContext) -> list[ArgLike]:
    args = []

    for dep in ctx.attrs.flat_deps:
        default_info = dep.get(DefaultInfo)
        if default_info == None:
            continue
        default_outputs = default_info.default_outputs
        if len(default_outputs) > 0:
            args.append(default_outputs[0])

    if ctx.attrs.distribution_flat_dep:
        default_info = ctx.attrs.distribution_flat_dep.get(DefaultInfo)
        if default_info != None:
            default_outputs = default_info.default_outputs
            if len(default_outputs) > 0:
                args.append(default_outputs[0])

    args = dedupe(args)

    transitive_link_args = get_link_args_for_strategy(
        ctx,
        [x.merged_link_info for x in linkables(ctx.attrs.deps)],
        LinkStrategy("static"),
    )
    args.append(unpack_link_args(transitive_link_args))

    return args

registration_spec = RuleRegistrationSpec(
    name = "apple_static_archive",
    impl = _impl,
    attrs = {
        "archive_name": attrs.option(attrs.string(), default = None),
        "deps": attrs.list(attrs.dep(), default = []),
        "distribution_flat_dep": attrs.option(attrs.dep(), default = None),
        "flat_deps": attrs.list(attrs.dep(), default = []),
        VALIDATION_DEPS_ATTR_NAME: VALIDATION_DEPS_ATTR_TYPE,
        "_apple_toolchain": get_apple_toolchain_attr(),
    },
)
