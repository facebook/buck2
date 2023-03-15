# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode_macros//build_defs:platform_utils.bzl", "platform_utils")

def _symlinked_buck2_and_tpx_impl(ctx: "context") -> ["provider"]:
    """
    Produce a directory layout that is similar to the one our release binary
    uses, this allows setting a path for Tpx relative to BUCK2_BINARY_DIR.

    We do the whole BUCK2_BINARY_DIR_RELATIVE_TO dance to support doing this
    using just symlinks. If we're willing to do a copy we can just
    `out.project("buck2")` and we're done.
    """
    buck2 = ctx.attrs.buck2[DefaultInfo].default_outputs[0]
    tpx = ctx.attrs.tpx[DefaultInfo].default_outputs[0]
    out = ctx.actions.symlinked_dir("out", {"buck2": buck2, "buck2-tpx": tpx})

    cmd = cmd_args(
        "/usr/bin/env",
        cmd_args(out, format = "BUCK2_BINARY_DIR_RELATIVE_TO={}").relative_to(buck2, parent = 1),
        out.project("buck2"),
    ).hidden(out)

    return [DefaultInfo(out), RunInfo(cmd)]

_symlinked_buck2_and_tpx = rule(impl = _symlinked_buck2_and_tpx_impl, attrs = {"buck2": attrs.dep(), "tpx": attrs.dep()})

def symlinked_buck2_and_tpx(**kwargs):
    cxx_platform = platform_utils.get_cxx_platform_for_base_path(native.package_name())
    kwargs["default_target_platform"] = cxx_platform.target_platform
    _symlinked_buck2_and_tpx(**kwargs)
