# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//debugging:common.bzl", "create_target_info", "target_name")
load("@prelude//debugging:configure_env.bzl", "configure_env")
load("@prelude//debugging:ensure_dwp.bzl", "ensure_dwp")
load("@prelude//debugging:types.bzl", "ExecInfo", "ScriptSettings", "TargetExtraInfo")

# "inspect_default" is reused across "fdb.bxl" to provide a fallback default information
# in case special handling for the rule type isn't implemented yet
def inspect_default(ctx: bxl.Context, _actions: AnalysisActions, target: bxl.ConfiguredTargetNode, settings: ScriptSettings) -> ExecInfo:
    ensure_dwp(ctx, target)
    attrs = target.attrs_lazy()
    labels = attrs.get("labels").value() if attrs.get("labels") else []
    env = attrs.get("env").value() if attrs.get("env") else {}
    configured_env = configure_env(env, labels)

    return ExecInfo(
        target_name = target_name(settings.target),
        target_info = create_target_info(settings.target),
        data = TargetExtraInfo(
            exec_info_version = 1,
            debugger = "fdb:debugger:default",
            env = configured_env.env,
        ),
        messages = configured_env.messages,
    )
