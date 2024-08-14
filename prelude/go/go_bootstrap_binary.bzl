# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_env_vars")

def go_bootstrap_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    """
    Produces a Go binary for use in prelude. Similar to `python_bootstrap_binary`
    It doesn't depend on other Go rules and uses `go build` under the hood.
    CGo is disabled minimise dependencies.
    """
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]

    target_is_win = go_toolchain.env_go_os == "windows"
    exe_suffix = ".exe" if target_is_win else ""
    output = ctx.actions.declare_output(ctx.label.name + exe_suffix)

    # Copy files, becode go:embed doesn't work with symlinks
    srcs_dir = ctx.actions.copied_dir(
        "__srcs_dir__",
        {src.short_path: src for src in ctx.attrs.srcs},
    )

    cmd = cmd_args([
        go_toolchain.go_wrapper,
        go_toolchain.go,
        ["--workdir", srcs_dir],
        "build",
        ["-o", cmd_args(output.as_output(), relative_to = srcs_dir)],
        ctx.attrs.entrypoints,
    ])

    env = get_toolchain_env_vars(go_toolchain)
    env["CGO_ENABLED"] = "0"

    ctx.actions.run(cmd, env = env, category = "go_bootstrap_binary")

    return [
        DefaultInfo(default_output = output),
        RunInfo(args = [output]),
    ]
