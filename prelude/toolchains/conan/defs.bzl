# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""Conan C/C++ Package Manager Toolchain."""

ConanToolchainInfo = provider(fields = ["conan"])

def _system_conan_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        ConanToolchainInfo(
            conan = RunInfo(args = [ctx.attrs.conan_path]),
        ),
    ]

system_conan_toolchain = rule(
    impl = _system_conan_toolchain_impl,
    attrs = {
        "conan_path": attrs.string(),
    },
    is_toolchain_rule = True,
)

def _conan_lock_update_impl(ctx):
    conan_toolchain = ctx.attrs._conan_toolchain[ConanToolchainInfo]

    # TODO[AH] This assumes that buck2 is in PATH when executing the script via `buck2 run`.
    #   Consider making the name/path `buck2` configurable via an environment variable.
    root = cmd_args(["ROOT=`buck2 root`"])
    conanfile = cmd_args([ctx.attrs.conanfile], format = 'CONANFILE="$ROOT/{}"')
    lockfile_out = cmd_args([ctx.attrs.lockfile_name], format = 'LOCKFILE_OUT="`dirname $CONANFILE`/{}"')
    conan_cmd = cmd_args([conan_toolchain.conan, "lock", "create"], delimiter = " ")
    if ctx.attrs.lockfile:
        conan_cmd.add(["--lockfile", ctx.attrs.lockfile])
    conan_cmd.add(["--lockfile-out", "$LOCKFILE_OUT"])
    conan_cmd.add(["$CONANFILE"])

    output = ctx.actions.declare_output(ctx.label.name)
    script, inputs = ctx.actions.write(
        output,
        cmd_args([
            "#!/bin/sh",
            root,
            conanfile,
            lockfile_out,
            conan_cmd,
        ]),
        is_executable = True,
        allow_args = True,
    )

    run = cmd_args("/bin/sh", script)
    run.hidden(inputs)

    return [
        DefaultInfo(default_outputs = [output]),
        RunInfo(args = [run]),
    ]

conan_lock_update = rule(
    impl = _conan_lock_update_impl,
    attrs = {
        "conanfile": attrs.source(),
        "lockfile_name": attrs.string(),
        "lockfile": attrs.option(attrs.source(), default = None),
        "_conan_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:conan", providers = [ConanToolchainInfo])),
    },
)
