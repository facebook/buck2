# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//debugging/common.bzl", "create_target_info", "target_name")
load("@prelude//debugging/types.bzl", "ScriptSettings")

def inspect_dbg_exec(ctx: "bxl_ctx", actions: AnalysisActions, target: "target_node", settings: ScriptSettings):
    pointer_name = target_name(target)
    if not pointer_name.endswith("_fdb"):
        pointer_name = "{}_fdb".format(pointer_name)

    fbsource_alias_target = ctx.configured_targets(pointer_name)

    fdb_helper = ctx.analysis(fbsource_alias_target).providers()[RunInfo]
    fdb_helper_out = actions.declare_output("fdb_helper.json")
    cmd = cmd_args(fdb_helper)
    cmd.add(settings.args)
    actions.run(cmd, category = "fdb_helper", env = {"FDB_OUTPUT_FILE": fdb_helper_out.as_output()}, local_only = True)
    result = actions.declare_output("final_out.json")

    # java is supported via [JavaClassToSourceMapInfo] provider
    def build_exec_info(ctx, artifacts, outputs):
        exec_info = artifacts[fdb_helper_out].read_json()
        exec_info["java"] = None  # TODO: merge java info here

        # read_json can't create a record of type ExecInfo
        # can alternativelly create ExecInfo by enumerating every single primitive nested field in there
        ctx.bxl_actions().actions.write_json(outputs[result], {
            "data": exec_info,
            "target_info": create_target_info(settings.target),
            "target_name": target_name(settings.target),
        })

    actions.dynamic_output(
        dynamic = [fdb_helper_out],
        inputs = [],
        outputs = [result],
        f = build_exec_info,
    )
    return result
