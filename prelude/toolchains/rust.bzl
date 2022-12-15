# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//rust:rust_toolchain.bzl", "RustPlatformInfo", "RustToolchainInfo")

def _system_rust_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        RustToolchainInfo(
            clippy_driver = "clippy-driver",
            compiler = "rustc",
            extern_html_root_url_prefix = "",
            failure_filter = False,
            failure_filter_action = None,
            pipelined = False,
            report_unused_deps = False,
            rustc_action = ctx.attrs.rustc_action[RunInfo],
            default_edition = "2021",
            rustc_target_triple = "x86_64-pc-windows-gnu" if host_info().os.is_windows else "x86_64-unknown-linux-gnu",
            rustc_test_flags = "",
            rustdoc = "rustdoc",
        ),
        RustPlatformInfo(
            name = "x86_64",
        ),
    ]

system_rust_toolchain = rule(
    impl = _system_rust_toolchain_impl,
    attrs = {
        "rustc_action": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//rust/tools:rustc_action")),
    },
    is_toolchain_rule = True,
)
