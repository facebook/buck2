# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//rust:rust_toolchain.bzl", "RustToolchainInfo")
load("@prelude//decls/toolchains_common.bzl", "toolchains_common")

def _get_rustc_cfg_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchain_info = ctx.attrs._rust_toolchain[RustToolchainInfo]

    out = ctx.actions.declare_output("rustc.cfg")

    cmd = [
        toolchain_info.compiler,
        cmd_args("--print=cfg=", out.as_output(), delimiter = ""),
        cmd_args("--target=", toolchain_info.rustc_target_triple, delimiter = ""),
        cmd_args("--sysroot="),  # We do not need a sysroot here, and not all platforms we support have one available (e.g. mips64-unknown-linux-gnuabi64)
    ]

    ctx.actions.run(cmd, category = "rustc_cfg")

    return [DefaultInfo(default_output = out)]

get_rustc_cfg = rule(
    impl = _get_rustc_cfg_impl,
    attrs = {
        "_rust_toolchain": toolchains_common.rust(),
    },
)
