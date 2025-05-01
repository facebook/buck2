# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:artifacts.bzl", "single_artifact")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//dist:dist_info.bzl", "DistInfo")
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
)
load(
    "@prelude//utils:utils.bzl",
    "map_val",
    "value_or",
)
load(":link.bzl", "link")
load(":package_builder.bzl", "build_package")
load(":packages.bzl", "go_attr_pkg_name")
load(":toolchain.bzl", "evaluate_cgo_enabled")

def go_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    cxx_toolchain_available = CxxToolchainInfo in ctx.attrs._cxx_toolchain
    pkg_name = go_attr_pkg_name(ctx)

    lib, pkg_info = build_package(
        ctx = ctx,
        pkg_name = pkg_name,
        main = True,
        srcs = ctx.attrs.srcs,
        package_root = ctx.attrs.package_root,
        deps = ctx.attrs.deps,
        compiler_flags = ctx.attrs.compiler_flags,
        build_tags = ctx.attrs._build_tags,
        race = ctx.attrs._race,
        asan = ctx.attrs._asan,
        embedcfg = ctx.attrs.embedcfg,
        cgo_enabled = evaluate_cgo_enabled(cxx_toolchain_available, ctx.attrs.cgo_enabled),
    )
    (bin, runtime_files, external_debug_info) = link(
        ctx,
        lib,
        deps = ctx.attrs.deps,
        link_style = value_or(map_val(LinkStyle, ctx.attrs.link_style), LinkStyle("static")),
        linker_flags = ctx.attrs.linker_flags,
        link_mode = ctx.attrs.link_mode,
        race = ctx.attrs._race,
        asan = ctx.attrs._asan,
        external_linker_flags = ctx.attrs.external_linker_flags,
    )

    # runtime_files are all the artifacts that must be present in order for this
    # binary to be runnable. Notably, all of its shared library dependencies.
    # This is materialized when a Go binary is executed as a genrule.
    #
    # other_outputs is a superset of runtime_files, adding external debuginfo
    # which is necessary for a user to run this binary in a debugger. This is
    # materialized when a Go binary is the end result of a build.
    runtime_files = list(runtime_files)
    other_outputs = runtime_files + external_debug_info

    for resource in ctx.attrs.resources:
        resource = single_artifact(resource)

        runtime_files.append(resource.default_output)
        runtime_files.extend(resource.nondebug_runtime_files)

        other_outputs.append(resource.default_output)
        other_outputs.extend(resource.other_outputs)

    return [
        DefaultInfo(
            default_output = bin,
            other_outputs = other_outputs,
        ),
        RunInfo(args = cmd_args(bin, hidden = other_outputs)),
        DistInfo(nondebug_runtime_files = runtime_files),
        pkg_info,
    ]
