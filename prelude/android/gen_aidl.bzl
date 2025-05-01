# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//java:java_toolchain.bzl", "JavaToolchainInfo")
load("@prelude//os_lookup:defs.bzl", "Os", "OsLookup")
load(":android_toolchain.bzl", "AndroidToolchainInfo")

_AidlSourceInfo = provider(fields = {
    "srcs": provider_field(typing.Any, default = None),
})

def gen_aidl_impl(ctx: AnalysisContext) -> list[Provider]:
    android_toolchain = ctx.attrs._android_toolchain[AndroidToolchainInfo]
    aidl_cmd = cmd_args(
        [android_toolchain.aidl] +
        ["-p", android_toolchain.framework_aidl_file] +
        ["-I", ctx.attrs.import_path] +
        [a for path in ctx.attrs.import_paths for a in ["-I", path]],
        # We need the `aidl_srcs` files - otherwise the search on the `import_path` won't find anything.
        hidden = ctx.attrs.aidl_srcs,
    )

    # Allow gen_aidl rules to depend on other gen_aidl rules, and make the source files from the
    # deps accessible in this context. This is an alternative to adding dependent files in
    # aidl_srcs.
    dep_srcs = []
    for dep in ctx.attrs.deps:
        source_info = dep.get(_AidlSourceInfo)
        if source_info != None:
            dep_srcs += source_info.srcs
        else:
            warning("`{}` dependency `{}` is not a `gen_aidl` rule and will be ignored".format(ctx.label, dep.label))

    aidl_cmd.add(cmd_args(hidden = dep_srcs))

    aidl_out = ctx.actions.declare_output("aidl_output", dir = True)
    aidl_cmd.add("-o", aidl_out.as_output())
    aidl_cmd.add(ctx.attrs.aidl)

    # Aidl does not create any output for parcelables. Therefore, we always initialize the output
    # directory so that we don't get an "Action failed to produce outputs" error.
    if ctx.attrs._exec_os_type[OsLookup].os == Os("windows"):
        sh_cmd = cmd_args([
            cmd_args(["cmd.exe", "/c", cmd_args([aidl_out.as_output()], format = "if not exist {} md {}")]),
            "&&",
            aidl_cmd,
        ])
    else:
        sh_cmd = cmd_args([
            "sh",
            "-c",
            "mkdir -p $1 && $2",
            "--",
            aidl_out.as_output(),
            cmd_args(aidl_cmd, delimiter = " "),
        ])

    ctx.actions.run(sh_cmd, category = "aidl")

    # Put the generated Java files into a zip file to be used as srcs to other rules.
    java_toolchain = ctx.attrs._java_toolchain[JavaToolchainInfo]
    jar_cmd = cmd_args(java_toolchain.jar)
    jar_cmd.add("-cfM")
    out = ctx.actions.declare_output("{}_aidl_java_output.src.zip".format(ctx.attrs.name))
    jar_cmd.add(out.as_output())
    jar_cmd.add(aidl_out)

    ctx.actions.run(jar_cmd, category = "aidl_jar")

    return [
        DefaultInfo(default_output = out),
        _AidlSourceInfo(srcs = [ctx.attrs.aidl] + ctx.attrs.aidl_srcs + dep_srcs),
    ]
