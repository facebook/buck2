# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//rust:rust_toolchain.bzl", "PanicRuntime", "RustToolchainInfo")

# almost identical to the system_rust_toolchain implementation, with the only
# the difference being the ability to specify rustc

_DEFAULT_TRIPLE = select({
    "config//os:linux": select({
        "config//cpu:arm64": "aarch64-unknown-linux-gnu",
        "config//cpu:x86_64": "x86_64-unknown-linux-gnu",
        "config//cpu:riscv64": "riscv64gc-unknown-linux-gnu",
    }),
    "config//os:macos": select({
        "config//cpu:arm64": "aarch64-apple-darwin",
        "config//cpu:x86_64": "x86_64-apple-darwin",
    }),
    "config//os:windows": select({
        "config//cpu:arm64": "aarch64-pc-windows-msvc",
        "config//cpu:x86_64": "x86_64-pc-windows-msvc",
    }),
})

def _rust_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        RustToolchainInfo(
            clippy_driver = RunInfo(args = ["clippy-driver"]),
            compiler = ctx.attrs.compiler[RunInfo],
            default_edition = ctx.attrs.default_edition,
            panic_runtime = PanicRuntime("unwind"),
            rustc_flags = ctx.attrs.rustc_flags,
            rustc_target_triple = ctx.attrs.rustc_target_triple,
            rustdoc = RunInfo(args = ["rustdoc"]),
            rustdoc_flags = ctx.attrs.rustdoc_flags,
        ),
    ]

rust_toolchain = rule(
    impl = _rust_toolchain_impl,
    attrs = {
        "compiler": attrs.exec_dep(providers = [RunInfo]),
        "default_edition": attrs.option(attrs.string(), default = None),
        "failure_filter_action": attrs.default_only(attrs.exec_dep(providers = [RunInfo], default = "prelude//rust/tools:failure_filter_action")),
        "rustc_action": attrs.default_only(attrs.exec_dep(providers = [RunInfo], default = "prelude//rust/tools:rustc_action")),
        "rustc_flags": attrs.list(attrs.string(), default = []),
        "rustc_target_triple": attrs.string(default = _DEFAULT_TRIPLE),
        "rustdoc_flags": attrs.list(attrs.string(), default = []),
        "rustdoc_test_with_resources": attrs.default_only(attrs.exec_dep(providers = [RunInfo], default = "prelude//rust/tools:rustdoc_test_with_resources")),
    },
    is_toolchain_rule = True,
)
