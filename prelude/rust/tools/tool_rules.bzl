# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//decls:toolchains_common.bzl", "toolchains_common")
load("@prelude//rust:rust_toolchain.bzl", "RustToolchainInfo")

def _get_rustc_cfg_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchain_info = ctx.attrs._rust_toolchain[RustToolchainInfo]

    out = ctx.actions.declare_output("rustc.cfg")

    cmd = [
        toolchain_info.compiler,
        cmd_args("--print=cfg=", out.as_output(), delimiter = ""),
        cmd_args("--sysroot="),  # We do not need a sysroot here, and not all platforms we support have one available (e.g. mips64-unknown-linux-gnuabi64)
    ]

    if toolchain_info.rustc_target_triple:
        cmd.append(cmd_args("--target=", toolchain_info.rustc_target_triple, delimiter = ""))

    env = {}
    if toolchain_info.rust_target_path != None:
        env["RUST_TARGET_PATH"] = toolchain_info.rust_target_path[DefaultInfo].default_outputs[0]

    ctx.actions.run(cmd, category = "rustc_cfg", env = env)

    return [DefaultInfo(default_output = out)]

get_rustc_cfg = rule(
    impl = _get_rustc_cfg_impl,
    attrs = {
        "_rust_toolchain": toolchains_common.rust(),
    },
)

def _get_rustc_host_tuple_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchain_info = ctx.attrs._rust_toolchain[RustToolchainInfo]

    out = ctx.actions.declare_output("rustc.host_tuple")

    cmd = [
        toolchain_info.compiler,
        cmd_args("--print=host-tuple=", out.as_output(), delimiter = ""),
    ]

    ctx.actions.run(cmd, category = "rustc_host_tuple")

    return [DefaultInfo(default_output = out)]

get_rustc_host_tuple = rule(
    impl = _get_rustc_host_tuple_impl,
    attrs = {
        "_rust_toolchain": toolchains_common.rust(),
    },
)

def _linkable_symbol_supports_no_std_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchain_info = ctx.attrs._rust_toolchain[RustToolchainInfo]

    # `#[no_std]` requires use of `advanced_unstable_linking` on the toolchain,
    # as otherwise the panic handler is missing.
    cfg = "--cfg=set_nostd\n" if toolchain_info.advanced_unstable_linking else ""

    flagfile = ctx.actions.write("cfg", cfg)
    return [DefaultInfo(default_output = flagfile)]

linkable_symbol_supports_no_std = rule(
    impl = _linkable_symbol_supports_no_std_impl,
    attrs = {
        "_rust_toolchain": toolchains_common.rust(),
    },
)
