# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//debugging:common.bzl", "create_target_info", "target_name")
load("@prelude//debugging:types.bzl", "ExecInfo", "JavaInfo", "ScriptSettings", "TargetExtraInfo")
load("@prelude//java:class_to_srcs.bzl", "JavaClassToSourceMapInfo")

def inspect_java_rule(ctx: bxl.Context, actions: AnalysisActions, target: bxl.ConfiguredTargetNode, settings: ScriptSettings) -> ExecInfo:
    providers = ctx.analysis(target).providers()
    debuginfo = providers[JavaClassToSourceMapInfo].debuginfo if JavaClassToSourceMapInfo in providers else None
    if debuginfo:
        ctx.output.ensure(debuginfo)

    program = None
    if ExternalRunnerTestInfo in providers:
        cmd = cmd_args()

        for key, value in providers[ExternalRunnerTestInfo].env.items():
            cmd.add(cmd_args([key, "=", cmd_args(value, quote = "shell")], delimiter = ""))

        cmd.add(providers[ExternalRunnerTestInfo].command)

        # TODO: Maybe figure out how to make this a runnable thing
        program_lines = actions.write(
            "run_runner.argsfile",
            cmd,
            allow_args = True,
            with_inputs = True,
        )

        ctx.output.ensure(program_lines[0])
        ctx.output.ensure_multiple(program_lines[1])
        program = get_path_without_materialization(program_lines[0], ctx, abs = True)

    return ExecInfo(
        target_name = target_name(settings.target),
        target_info = create_target_info(settings.target),
        data = TargetExtraInfo(
            exec_info_version = 1,
            debugger = "fdb:debugger:jdwp",
            java = JavaInfo(
                classmap_file = debuginfo,
            ),
            program = program,
        ),
    )
