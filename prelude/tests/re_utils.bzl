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

# Result of `get_re_executors_from_props`.
#
# `run_from_project_root` and `use_project_relative_paths` report whether the
# returned executor is remote-execution eligible and therefore the test must run
# from the project root with project-relative paths. They are currently the same
# value, but keep them separate from each other and from "is there an executor at
# all?" so callers can evolve cwd and path behavior independently.
RemoteTestExecutorConfig = record(
    default_executor = field([CommandExecutorConfig, None], default = None),
    executor_overrides = field(dict[str, CommandExecutorConfig], default = {}),
    run_from_project_root = field(bool, default = False),
    use_project_relative_paths = field(bool, default = False),
)

def _network_access_kwargs(network_access: str | None) -> dict[str, str]:
    if network_access == None:
        return {}
    return {"network_access": network_access}

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
                re_props = ctx.attrs._remote_test_execution_toolchain[RemoteTestExecutionToolchainInfo].profiles[ctx.attrs.remote_execution],
                default_run_as_bundle = ctx.attrs._remote_test_execution_toolchain[RemoteTestExecutionToolchainInfo].default_run_as_bundle,
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

def get_re_executors_from_props(ctx: AnalysisContext, dynamic_image_override: [dict, None] = None) -> RemoteTestExecutorConfig:
    """
    Convert the `remote_execution` properties param into `CommandExecutorConfig` objects to use with test providers.

    The target's `network_access` policy (if any) is attached to the returned
    `CommandExecutorConfig`(s) so it is enforced for both local and remote test
    execution. When a target has no RE profile but does request a `network_access`
    policy, a local-only executor is synthesized solely to carry that policy; it is
    reported with `run_from_project_root = False` so the test keeps running in-place
    rather than being switched to project-root/RE-style execution.

    Args:
        ctx: The analysis context.
        dynamic_image_override: If provided, overrides the `remote_execution_dynamic_image`
            from `remote_execution` props. Use this to inject a resolved snapshotted fbpkg
            image (with pinned uuid) at analysis time.

    Returns a `RemoteTestExecutorConfig`.
    """

    re_arg = _get_re_arg(ctx)
    network_access = getattr(ctx.attrs, "network_access", None)

    if re_arg.disabled:
        executor = CommandExecutorConfig(local_enabled = True, remote_enabled = False, **_network_access_kwargs(network_access))
        # A `remote_execution = "disabled"` target has always produced an executor
        # and therefore run from the project root; preserve that behavior.
        return RemoteTestExecutorConfig(default_executor = executor, run_from_project_root = True, use_project_relative_paths = True)

    re_props = re_arg.re_props
    if re_props == None:
        if network_access != None:
            # No RE profile, but the target requests a network policy. Synthesize a
            # local-only executor purely to carry it. Crucially this executor is NOT
            # marked as needing the project root, so the test still runs in-place
            # (from the cell root) just as it would with no executor at all.
            #
            # `remote_cache_enabled = False` keeps this a plain `Executor::Local`
            # (parity: the unset default is True in fbcode but False in OSS), and
            # nothing is uploaded from here anyway (`allow_cache_uploads` is False).
            executor = CommandExecutorConfig(local_enabled = True, remote_enabled = False, remote_cache_enabled = False, **_network_access_kwargs(network_access))
            return RemoteTestExecutorConfig(default_executor = executor)
        return RemoteTestExecutorConfig()

    re_props_copy = dict(re_props)
    capabilities = re_props_copy.pop("capabilities")
    use_case = re_props_copy.pop("use_case")
    listing_capabilities = re_props_copy.pop("listing_capabilities", None)
    remote_cache_enabled = re_props_copy.pop("remote_cache_enabled", None)
    re_dependencies = re_props_copy.pop("dependencies", [])
    re_gang_workers = re_props_copy.pop("gang_workers", [])
    re_gang = re_props_copy.pop("gang", None)
    local_enabled = re_props_copy.pop("local_enabled", False)
    local_listing_enabled = re_props_copy.pop("local_listing_enabled", None)
    re_resource_units = re_props_copy.pop("resource_units", None)
    re_listing_resource_units = re_props_copy.pop("listing_resource_units", re_resource_units)
    re_dynamic_image = re_props_copy.pop("remote_execution_dynamic_image", None)
    meta_internal_extra_params = re_props_copy.pop("meta_internal_extra_params", None)
    if dynamic_image_override != None:
        re_dynamic_image = dynamic_image_override
    if re_props_copy:
        unexpected_props = ", ".join(re_props_copy.keys())
        fail("found unexpected re props: " + unexpected_props)

    if re_gang != None:
        meta_internal_extra_params = dict(meta_internal_extra_params or {})
        meta_internal_extra_params["remote_execution_gang"] = re_gang

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
        meta_internal_extra_params = meta_internal_extra_params,
        **_network_access_kwargs(network_access),
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
            meta_internal_extra_params = meta_internal_extra_params,
            **_network_access_kwargs(network_access),
        )
    return RemoteTestExecutorConfig(
        default_executor = default_executor,
        executor_overrides = {"listing": listing_executor},
        run_from_project_root = True,
        use_project_relative_paths = True,
    )
