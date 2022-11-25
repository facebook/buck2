# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""Conan C/C++ Package Manager Toolchain.

Provides a toolchain and rules to use the [Conan package manager][conan] to
manage and install third-party C/C++ dependencies.
"""

ConanToolchainInfo = provider(fields = ["conan"])

def _system_conan_toolchain_impl(ctx: "context") -> ["provider"]:
    return [
        DefaultInfo(),
        ConanToolchainInfo(
            conan = RunInfo(args = [ctx.attrs.conan_path]),
        ),
    ]

system_conan_toolchain = rule(
    impl = _system_conan_toolchain_impl,
    attrs = {
        "conan_path": attrs.string(doc = "Path to the Conan executable."),
    },
    is_toolchain_rule = True,
    doc = "Uses a globally installed Conan executable.",
)

def _conan_lock_update_impl(ctx: "context") -> ["provider"]:
    conan_toolchain = ctx.attrs._conan_toolchain[ConanToolchainInfo]
    conan_update = ctx.attrs._conan_update[RunInfo]

    cmd = cmd_args([conan_update])
    cmd.add(["--conan", conan_toolchain.conan])
    cmd.add(["--conanfile", ctx.attrs.conanfile])
    cmd.add(["--lockfile-out", ctx.attrs.lockfile_name])
    if ctx.attrs.lockfile:
        cmd.add(["--lockfile", ctx.attrs.lockfile])
    cmd.add(["--bzl-out", ctx.attrs.bzl_name])

    return [
        DefaultInfo(default_outputs = []),
        RunInfo(args = [cmd]),
    ]

conan_lock_update = rule(
    impl = _conan_lock_update_impl,
    attrs = {
        "conanfile": attrs.source(doc = "The conanfile defining the project dependencies."),
        "lockfile_name": attrs.string(doc = "Generate a lockfile with this name next to the conanfile."),
        "lockfile": attrs.option(attrs.source(doc = "A pre-existing lockfile to base the dependency resolution on."), default = None),
        "bzl_name": attrs.string(doc = "Generate a .bzl file with this name next to the conanfile to define the Conan package targets."),
        "_conan_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:conan", providers = [ConanToolchainInfo])),
        "_conan_update": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_update"),
    },
    doc = "Defines a runnable target that will invoke Conan to generate a lock-file based on the given conanfile.",
)
