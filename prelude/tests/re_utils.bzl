# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//tests:remote_test_execution_toolchain.bzl", "RemoteTestExecutionToolchainInfo")
load("@prelude//utils:expect.bzl", "expect_non_none")
load("@prelude//utils:type_defs.bzl", "type_utils")

ReArg = record(
    disabled = field(bool | None, default = None),
    re_props = field(dict | None, default = None),
    default_run_as_bundle = field(bool | None, default = None),
)

def _get_re_arg(ctx: AnalysisContext) -> ReArg:
    force_local = read_config("fbcode", "disable_re_tests", default = False)
    if force_local or not hasattr(ctx.attrs, "remote_execution"):
        # NOTE: this is kinda weird, we take this path if the attr is missing completely
        # Even if the value is None we still follow. Adding force.local to give users
        # some means of bypassing.
        # Example usecase: SGW wants to run kotlin_test targets on MBP/OSX locally
        # eg: buck2 test --local-only -c fbcode.disable_re_tests=True //signals/cloudbridge/v2/libs/cb-meters:test
        return ReArg(re_props = None, default_run_as_bundle = False)

    if ctx.attrs.remote_execution != None:
        if ctx.attrs.remote_execution == "disabled":
            return ReArg(disabled = True)
        elif type_utils.is_string(ctx.attrs.remote_execution):
            # If this is a string, look up the re_props on the RE toolchain.
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

    re_arg = _get_re_arg(ctx)

    if re_arg.disabled:
        executor = CommandExecutorConfig(local_enabled = True, remote_enabled = False)
        return executor, {}

    re_props = re_arg.re_props
    if re_props == None:
        return None, {}

    re_props_copy = dict(re_props)
    capabilities = re_props_copy.pop("capabilities")
    use_case = re_props_copy.pop("use_case")
    listing_capabilities = re_props_copy.pop("listing_capabilities", None)
    remote_cache_enabled = re_props_copy.pop("remote_cache_enabled", None)
    re_dependencies = re_props_copy.pop("dependencies", [])
    re_gang_workers = re_props_copy.pop("gang_workers", [])
    local_enabled = re_props_copy.pop("local_enabled", False)
    local_listing_enabled = re_props_copy.pop("local_listing_enabled", None)
    re_resource_units = re_props_copy.pop("resource_units", None)
    re_listing_resource_units = re_props_copy.pop("listing_resource_units", re_resource_units)
    re_dynamic_image = re_props_copy.pop("remote_execution_dynamic_image", None)
    if re_props_copy:
        unexpected_props = ", ".join(re_props_copy.keys())
        fail("found unexpected re props: " + unexpected_props)

    default_executor = CommandExecutorConfig(
        local_enabled = local_enabled,
        remote_enabled = True,
        remote_execution_properties = capabilities,
        remote_execution_use_case = use_case or "tpx-default",
        remote_cache_enabled = remote_cache_enabled,
        remote_execution_dependencies = re_dependencies,
        remote_execution_gang_workers = re_gang_workers,
        remote_execution_resource_units = re_resource_units,
        remote_execution_dynamic_image = re_dynamic_image,
    )

    listing_executor = default_executor
    if listing_capabilities != None or local_listing_enabled != None:
        listing_executor = CommandExecutorConfig(
            local_enabled = local_listing_enabled if local_listing_enabled != None else local_enabled,
            remote_enabled = True,
            remote_execution_properties = listing_capabilities if listing_capabilities != None else capabilities,
            remote_execution_use_case = use_case or "tpx-default",
            remote_cache_enabled = remote_cache_enabled,
            remote_execution_resource_units = re_listing_resource_units,
            remote_execution_dynamic_image = re_dynamic_image,
        )
    return default_executor, {"listing": listing_executor}
