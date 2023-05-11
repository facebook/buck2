# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//linking:execution_preference.bzl", "LinkExecutionPreference", "get_link_execution_preference")

def link_cxx_binary_locally(ctx: "context", cxx_toolchain: ["CxxToolchainInfo", None] = None) -> bool.type:
    # Core tools are linked on RE because they are
    # a) small enough to do so and
    # b) don't get build stamping so they do cache correctly.
    if _is_core_tool(ctx):
        return False

    return _cxx_toolchain_sets_link_binaries_locally(ctx, cxx_toolchain)

def get_resolved_cxx_binary_link_execution_preference(
        ctx: "context",
        force_full_hybrid_if_capable: bool.type,
        cxx_toolchain: ["CxxToolchainInfo", None] = None) -> LinkExecutionPreference.type:
    if force_full_hybrid_if_capable:
        return LinkExecutionPreference("full_hybrid")

    # Core tools can be linked on RE because they are
    # a) small enough to do so and
    # b) don't get build stamping so they do cache correctly.
    if _is_core_tool(ctx):
        return LinkExecutionPreference("any")

    # Check if the toolchain has a preference.
    if _cxx_toolchain_sets_link_binaries_locally(ctx, cxx_toolchain):
        return LinkExecutionPreference("local")

    # Else use the preference on the target.
    return get_link_execution_preference(ctx)

def package_python_locally(ctx: "context", python_toolchain: "PythonToolchainInfo") -> bool.type:
    if _is_core_tool(ctx) or getattr(ctx.attrs, "_package_remotely", False):
        return False

    return python_toolchain.build_standalone_binaries_locally

def _is_core_tool(ctx: "context") -> bool.type:
    return "is_core_tool" in ctx.attrs.labels

def _cxx_toolchain_sets_link_binaries_locally(ctx: "context", cxx_toolchain: ["CxxToolchainInfo", None]) -> bool.type:
    if not cxx_toolchain:
        cxx_toolchain = get_cxx_toolchain_info(ctx)
    return cxx_toolchain.linker_info.link_binaries_locally
