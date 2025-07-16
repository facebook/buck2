# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")

GoBootstrapDistrInfo = provider(
    # @unsorted-dict-items
    fields = {
        "bin_go": provider_field(RunInfo),
        "go_root": provider_field(Artifact | None),
    },
)
GoBootstrapToolchainInfo = provider(
    fields = {
        "env_go_arch": provider_field(str),
        "env_go_os": provider_field(str),
        "env_go_root": provider_field(Artifact | None, default = None),
        "go": provider_field(RunInfo),
        "go_wrapper": provider_field(RunInfo),
    },
)

def go_bootstrap_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    """
    Produces a Go binary for use in prelude. Similar to `python_bootstrap_binary`
    It doesn't depend on other Go rules and uses `go build` under the hood.
    CGo is disabled minimise dependencies.
    """
    go_toolchain = ctx.attrs._go_bootstrap_toolchain[GoBootstrapToolchainInfo]

    target_is_win = go_toolchain.env_go_os == "windows"
    exe_suffix = ".exe" if target_is_win else ""
    output = ctx.actions.declare_output(ctx.label.name + exe_suffix)

    # Copy files, because go:embed doesn't work with symlinks
    srcs_dir = ctx.actions.copied_dir(
        "__srcs_dir__",
        {paths.relativize(src.short_path, ctx.attrs.workdir): src for src in ctx.attrs.srcs},
    )

    cmd = cmd_args([
        go_toolchain.go_wrapper,
        go_toolchain.go,
        ["--workdir", srcs_dir],
        "build",
        ["-o", cmd_args(output.as_output(), relative_to = srcs_dir)],
        ctx.attrs.build_args,
    ])

    env = {
        "CGO_ENABLED": "0",
        "GO111MODULE": "",
        "GOARCH": go_toolchain.env_go_arch,
        "GOOS": go_toolchain.env_go_os,
        "GOTOOLCHAIN": "local",
    }

    if go_toolchain.env_go_root != None:
        env["GOROOT"] = go_toolchain.env_go_root

    ctx.actions.run(cmd, env = env, category = "go_bootstrap_binary")

    return [
        DefaultInfo(default_output = output),
        RunInfo(args = [output]),
    ]
