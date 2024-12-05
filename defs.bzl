# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode_macros//build_defs:platform_utils.bzl", "platform_utils")
load("@prelude//decls:common.bzl", "buck")
load("@prelude//os_lookup:defs.bzl", "OsLookup")

def _buck2_bundle_impl(ctx: AnalysisContext) -> list[Provider]:
    """
    Produce a directory layout that is similar to the one our release binary
    uses, this allows setting a path for Tpx relative to BUCK2_BINARY_DIR.
    """
    target_is_windows = ctx.attrs._target_os_type[OsLookup].platform == "windows"

    materialisations = []

    buck2 = ctx.attrs.buck2[DefaultInfo].default_outputs[0]
    materialisations.extend(ctx.attrs.buck2[DefaultInfo].other_outputs)

    buck2_client = ctx.attrs.buck2_client[DefaultInfo].default_outputs[0]
    materialisations.extend(ctx.attrs.buck2_client[DefaultInfo].other_outputs)

    tpx = ctx.attrs.tpx[DefaultInfo].default_outputs[0]
    materialisations.extend(ctx.attrs.tpx[DefaultInfo].other_outputs)

    binary_extension = ".exe" if target_is_windows else ""
    buck2_binary = "buck2" + binary_extension
    buck2_tpx_binary = "buck2-tpx" + binary_extension
    buck2_daemon_binary = "buck2-daemon" + binary_extension
    out = ctx.actions.copied_dir("out", {buck2_binary: buck2_client, buck2_tpx_binary: ctx.actions.symlink_file(buck2_tpx_binary, tpx), buck2_daemon_binary: buck2})

    return [DefaultInfo(out, other_outputs = materialisations), RunInfo(cmd_args(out.project("buck2" + binary_extension), hidden = materialisations))]

_buck2_bundle = rule(
    impl = _buck2_bundle_impl,
    attrs = {
        "buck2": attrs.dep(),
        "buck2_client": attrs.dep(),
        "labels": attrs.list(attrs.string(), default = []),
        "tpx": attrs.dep(),
        "_target_os_type": buck.target_os_type_arg(),
    },
)

def buck2_bundle(**kwargs):
    cxx_platform = platform_utils.get_cxx_platform_for_base_path(native.package_name())
    kwargs["default_target_platform"] = cxx_platform.target_platform
    _buck2_bundle(**kwargs)
