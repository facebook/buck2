# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:build_mode.bzl", "BuildModeInfo")
load("@prelude//tests:remote_test_execution_toolchain.bzl", "RemoteTestExecutionToolchainInfo")
load("@prelude//utils:expect.bzl", "expect_non_none")

def _get_re_arg(ctx: AnalysisContext):
    if not hasattr(ctx.attrs, "remote_execution"):
        return None

    if ctx.attrs.remote_execution != None:
        # If this is a string, look up the profile on the RE toolchain.
        if type(ctx.attrs.remote_execution) == type(""):
            expect_non_none(ctx.attrs._remote_test_execution_toolchain)
            return ctx.attrs._remote_test_execution_toolchain[RemoteTestExecutionToolchainInfo].profiles[ctx.attrs.remote_execution]

        return ctx.attrs.remote_execution

    # Check for a default RE option on the toolchain.
    re_toolchain = ctx.attrs._remote_test_execution_toolchain
    if re_toolchain != None and re_toolchain[RemoteTestExecutionToolchainInfo].default_profile != None:
        return re_toolchain[RemoteTestExecutionToolchainInfo].default_profile

    return None

def get_re_executors_from_props(ctx: AnalysisContext) -> ([CommandExecutorConfig, None], dict[str, CommandExecutorConfig]):
    """
    Convert the `remote_execution` properties param into `CommandExecutorConfig` objects to use with test providers.

    Returns (default_executor, executor_overrides).
    """

    re_props = _get_re_arg(ctx)
    if re_props == None:
        return None, {}

    re_props_copy = dict(re_props)
    capabilities = re_props_copy.pop("capabilities")
    use_case = re_props_copy.pop("use_case")
    listing_capabilities = re_props_copy.pop("listing_capabilities", None)
    remote_cache_enabled = re_props_copy.pop("remote_cache_enabled", None)
    re_dependencies = re_props_copy.pop("dependencies", [])
    local_enabled = re_props_copy.pop("local_enabled", False)
    re_resource_units = re_props_copy.pop("resource_units", None)
    if re_props_copy:
        unexpected_props = ", ".join(re_props_copy.keys())
        fail("found unexpected re props: " + unexpected_props)

    remote_execution_action_key = None
    build_mode_info = ctx.attrs.remote_execution_action_key_providers[BuildModeInfo]
    if build_mode_info != None:
        remote_execution_action_key = "{}={}".format(build_mode_info.cell, build_mode_info.mode)

    default_executor = CommandExecutorConfig(
        local_enabled = local_enabled,
        remote_enabled = True,
        remote_execution_properties = capabilities,
        remote_execution_use_case = use_case or "tpx-default",
        remote_cache_enabled = remote_cache_enabled,
        remote_execution_action_key = remote_execution_action_key,
        remote_execution_dependencies = re_dependencies,
        remote_execution_resource_units = re_resource_units,
    )
    listing_executor = default_executor
    if listing_capabilities:
        listing_executor = CommandExecutorConfig(
            local_enabled = local_enabled,
            remote_enabled = True,
            remote_execution_properties = listing_capabilities,
            remote_execution_use_case = use_case or "tpx-default",
            remote_cache_enabled = remote_cache_enabled,
            remote_execution_action_key = remote_execution_action_key,
            remote_execution_resource_units = re_resource_units,
        )
    return default_executor, {"listing": listing_executor}
