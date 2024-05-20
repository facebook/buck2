# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//cxx:headers.bzl", "CxxHeadersLayout", "CxxHeadersNaming")
load("@prelude//utils:utils.bzl", "value_or")

def get_apple_cxx_headers_layout(ctx: AnalysisContext) -> CxxHeadersLayout:
    namespace = value_or(ctx.attrs.header_path_prefix, ctx.attrs.name)
    return CxxHeadersLayout(namespace = namespace, naming = CxxHeadersNaming("apple"))

def get_module_name(ctx: AnalysisContext) -> str:
    return ctx.attrs.module_name or ctx.attrs.header_path_prefix or ctx.attrs.name

def has_apple_toolchain(ctx: AnalysisContext) -> bool:
    return hasattr(ctx.attrs, "_apple_toolchain")

def get_apple_architecture(ctx: AnalysisContext) -> str:
    return ctx.attrs._apple_toolchain[AppleToolchainInfo].architecture

def get_apple_stripped_attr_value_with_default_fallback(ctx: AnalysisContext) -> bool:
    stripped = ctx.attrs.stripped
    if stripped != None:
        # `stripped` present on a target takes priority
        return stripped

    # Fallback to the default stripped override which is driven by buckconfig
    return ctx.attrs._stripped_default

def expand_relative_prefixed_sdk_path(
        sdk_path: cmd_args,
        swift_resource_dir: cmd_args,
        platform_path: cmd_args,
        path_to_expand: str) -> cmd_args:
    path_expansion_map = {
        "$PLATFORM_DIR": platform_path,
        "$RESOURCEDIR": swift_resource_dir,
        "$SDKROOT": sdk_path,
    }
    expanded_cmd = []
    for (path_variable, path_value) in path_expansion_map.items():
        if path_to_expand.startswith(path_variable):
            path = path_to_expand[len(path_variable):]
            if path.find("$") == 0:
                fail("Failed to expand framework path: {}".format(path))
            expanded_cmd.append(cmd_args([path_value, path], delimiter = ""))

    return cmd_args(expanded_cmd)

def get_disable_pch_validation_flags() -> list[str]:
    """
    We need to disable PCH validation for some actions like Swift compilation and Swift PCM generation.
    Currently, we don't have a mechanism to compile with enabled pch validation and Swift explicit modules,
    which we need to be able to do while we are waiting for Anonymous targets which will allow us to solve this problem properly.
    """
    return [
        "-Xcc",
        "-Xclang",
        "-Xcc",
        "-fno-validate-pch",
    ]
