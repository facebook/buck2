# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//decls:toolchains_common.bzl", "toolchains_common")
load("@prelude//rust:rust_toolchain.bzl", "RustToolchainInfo")

# Configurations for Clippy runs.
ClippyConfiguration = provider(
    fields = {
        "clippy_toml": provider_field(Artifact),
    },
)

def _clippy_configuration_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchain_ctx = ctx.attrs._rust_toolchain[RustToolchainInfo]
    toolchain_clippy_toml = toolchain_ctx.clippy_toml

    if not toolchain_clippy_toml:
        clippy_toml = ctx.attrs.clippy_toml_src
    else:
        toml_merge_tool = ctx.attrs.toml_merge_tool

        clippy_toml = ctx.actions.declare_output("clippy.toml")
        ctx.actions.run([
            toml_merge_tool[RunInfo],
            cmd_args(clippy_toml.as_output(), format = "--output={}"),
            cmd_args(toolchain_clippy_toml, format = "--file={}"),
            cmd_args(ctx.attrs.clippy_toml_src, format = "--file={}"),
        ], category = "clippy_toml_merge")

    return [
        DefaultInfo(
            default_output = clippy_toml,
        ),
        ClippyConfiguration(
            clippy_toml = clippy_toml,
        ),
    ]

# Generate a Clippy configuration that is merged with the toolchain specified
# Clippy configuration (if defined).
clippy_configuration = rule(impl = _clippy_configuration_impl, attrs = {
    "clippy_toml_src": attrs.source(),
    # TODO(emersonford): figure out how to store this in `_rust_toolchain`
    # without causing a circular dependency on the toolchain target when
    # `toml_merge_tool` is a `rust_binary`.
    #
    # Tool used to recursively merge multiple TOML files, e.g. for merging
    # clippy.toml files. Must support taking multiple `--file <FILENAME>` flags
    # as source files to merge and `--output <FILENAME>` flag to write the
    # merged TOML table to.
    "toml_merge_tool": attrs.exec_dep(providers = [RunInfo]),
    "_rust_toolchain": toolchains_common.rust(),
})
