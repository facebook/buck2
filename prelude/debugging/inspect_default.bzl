# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @starlark-rust: allow_string_literals_in_type_expr

load("@prelude//debugging/common.bzl", "create_target_info", "target_name")
load("@prelude//debugging/types.bzl", "ExecInfo", "ScriptSettings")

# "inspect_default" is reused across "fdb.bxl" to provide a fallback default information
# in case special handling for the rule type isn't implemented yet
def inspect_default(_ctx: bxl.Context, _actions: AnalysisActions, _target: "target_node", settings: ScriptSettings) -> ExecInfo:
    return ExecInfo(
        target_name = target_name(settings.target),
        target_info = create_target_info(settings.target),
        data = None,
    )
