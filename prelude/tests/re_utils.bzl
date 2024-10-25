# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:build_mode.bzl", "BuildModeInfo")
load("@prelude//tests:remote_test_execution_toolchain.bzl", "RemoteTestExecutionToolchainInfo")
load("@prelude//utils:expect.bzl", "expect_non_none")

ReArg = record(
    re_props = field(dict | None),
    default_run_as_bundle = field(bool | None),
)

def _get_re_arg(ctx: AnalysisContext) -> ReArg:
    if not hasattr(ctx.attrs, "remote_execution"):
        return ReArg(re_props = None, default_run_as_bundle = False)

    if ctx.attrs.remote_execution != None:
        # If this is a string, look up the re_props on the RE toolchain.
        if type(ctx.attrs.remote_execution) == type(""):
            expect_non_none(ctx.attrs._remote_test_execution_toolchain)
            return ReArg(
                re_props =
                    ctx.attrs._remote_test_execution_toolchain[RemoteTestExecutionToolchainInfo].profiles[ctx.attrs.remote_execution],
                default_run_as_bundle =
                    ctx.attrs._remote_test_execution_toolchain[RemoteTestExecutionToolchainInfo].default_run_as_bundle,
            )

        return ReArg(re_props = ctx.attrs.remote_execution, default_run_as_bundle = False)

    # Check for a default RE option on the toolchain.
    re_toolchain = ctx.attrs._remote_test_execution_toolchain
    if re_toolchain != None and re_toolchain[RemoteTestExecutionToolchainInfo].default_profile != None:
        return ReArg(
            re_props = re_toolchain[RemoteTestExecutionToolchainInfo].default_profile,
            default_run_as_bundle = re_toolchain[RemoteTestExecutionToolchainInfo].default_run_as_bundle,
        )

    return ReArg(re_props = None, default_run_as_bundle = False)

def maybe_add_run_as_bundle_label(ctx: AnalysisContext, labels: list[str]) -> None:
    if "re_ignore_force_run_as_bundle" in labels:
        return
    re_arg = _get_re_arg(ctx)
    if re_arg.default_run_as_bundle or read_config("tpx", "force_run_as_bundle") == "True":
        labels.extend(["run_as_bundle"])

def get_re_executors_from_props(ctx: AnalysisContext) -> ([CommandExecutorConfig, None], dict[str, CommandExecutorConfig]):
    """
    Convert the `remote_execution` properties param into `CommandExecutorConfig` objects to use with test providers.

    Returns (default_executor, executor_overrides).
    """

    re_props = _get_re_arg(ctx).re_props
    if re_props == None:
        # If no RE args are set and an RE config is specified
        if bool(read_config("tpx", "force_re_props")):
            re_props = {
                "capabilities": {
                    "platform": read_config("remoteexecution", "platform"),
                    "subplatform": read_config("remoteexecution", "subplatform"),
                },
                "use_case": read_config("remoteexecution", "use_case"),
            }

        else:
            return None, {}

    re_props_copy = dict(re_props)
    capabilities = re_props_copy.pop("capabilities")
    use_case = re_props_copy.pop("use_case")
    listing_capabilities = re_props_copy.pop("listing_capabilities", None)
    remote_cache_enabled = re_props_copy.pop("remote_cache_enabled", None)
    re_dependencies = re_props_copy.pop("dependencies", [])
    local_enabled = re_props_copy.pop("local_enabled", False)
    local_listing_enabled = re_props_copy.pop("local_listing_enabled", False)
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
            local_enabled = local_listing_enabled or False,
            remote_enabled = True,
            remote_execution_properties = listing_capabilities,
            remote_execution_use_case = use_case or "tpx-default",
            remote_cache_enabled = remote_cache_enabled,
            remote_execution_action_key = remote_execution_action_key,
            remote_execution_resource_units = re_resource_units,
        )
    return default_executor, {"listing": listing_executor}
