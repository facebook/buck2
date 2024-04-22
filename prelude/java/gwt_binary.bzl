# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//java:java_toolchain.bzl", "JavaToolchainInfo")
load("@prelude//java/utils:java_more_utils.bzl", "get_path_separator_for_exec_os")
load("@prelude//utils:expect.bzl", "expect")
load(
    ":java_providers.bzl",
    "derive_compiling_deps",
    "get_all_java_packaging_deps",
)

_GWT_COMPILER_CLASS = "com.google.gwt.dev.Compiler"

def gwt_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    expect(ctx.attrs.local_workers > 0, "local workers must be greater than zero")

    output = ctx.actions.declare_output("{}.zip".format(ctx.label.name))

    # Write deploy files to separate directory so that the generated .zip is smaller
    deploy_output = ctx.actions.declare_output("deploy")

    module_deps_classpath = [dep.gwt_module for dep in get_all_java_packaging_deps(ctx, ctx.attrs.module_deps) if dep.gwt_module]
    compiling_deps_tset = derive_compiling_deps(ctx.actions, None, ctx.attrs.deps)
    deps_classpath = [dep.full_library for dep in (list(compiling_deps_tset.traverse()) if compiling_deps_tset else [])]

    java_toolchain = ctx.attrs._java_toolchain[JavaToolchainInfo]
    gwt_args = cmd_args([
        java_toolchain.java[RunInfo],
        "-Dgwt.normalizeTimestamps=true",
        ctx.attrs.vm_args,
        "-classpath",
        cmd_args(module_deps_classpath + deps_classpath, delimiter = get_path_separator_for_exec_os(ctx)),
        _GWT_COMPILER_CLASS,
        "-war",
        output.as_output(),
        "-style",
        ctx.attrs.style,
        "-optimize",
        str(ctx.attrs.optimize),
        "-localWorkers",
        str(ctx.attrs.local_workers),
        "-deploy",
        deploy_output.as_output(),
    ])

    if ctx.attrs.draft_compile:
        gwt_args.add("-draftCompile")
    if ctx.attrs.strict:
        gwt_args.add("-strict")
    gwt_args.add(ctx.attrs.experimental_args)
    gwt_args.add(ctx.attrs.modules)

    ctx.actions.run(gwt_args, category = "gwt_binary")

    sub_targets = {"deploy": [DefaultInfo(default_output = deploy_output)]}

    return [
        DefaultInfo(default_output = output, sub_targets = sub_targets),
    ]
