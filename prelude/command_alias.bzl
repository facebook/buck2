# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//os_lookup:defs.bzl", "Os", "OsLookup", "ScriptLanguage")
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused: Used as a type

def command_alias_impl(ctx: AnalysisContext):
    target_os = ctx.attrs._target_os_type[OsLookup]

    if target_os.os == Os("fat_mac_linux") and len(ctx.attrs.platform_exe) > 0:
        base = {
            "Darwin": _get_os_base(ctx, Os("macos")),
            "Linux": _get_os_base(ctx, Os("linux")),
        }
    else:
        base = _get_os_base(ctx, target_os.os)

    output = _command_alias_impl(ctx, target_os, base, cmd_args(ctx.attrs.args), ctx.attrs.env)

    default_info = DefaultInfo(
        default_output = output.output.default_outputs[0],
        other_outputs = list(output.output.other_outputs) + ctx.attrs.resources,
    )
    run_info = RunInfo(
        args = cmd_args(output.run_info.args, hidden = ctx.attrs.resources),
    )

    return [default_info, run_info]

def _get_os_base(ctx: AnalysisContext, os: Os) -> RunInfo:
    exe = ctx.attrs.platform_exe.get(os.value)
    if exe == None:
        exe = ctx.attrs.exe

    if exe == None:
        return RunInfo()

    if isinstance(exe, Artifact):
        return RunInfo(args = cmd_args(exe))

    run_info = exe.get(RunInfo)
    if run_info == None:
        run_info = RunInfo(
            args = exe[DefaultInfo].default_outputs,
        )

    return run_info

CommandAliasOutput = record(
    # A batch or sh script representing the command alias
    #
    # `DefaultInfo` instead of `Artifact` because the `other_outputs` will usually need to be
    # included as hidden somewhere
    output = DefaultInfo,
    run_info = RunInfo,
)

def _command_alias_impl(
        ctx: AnalysisContext,
        target_os: OsLookup,
        base: RunInfo | dict[str, RunInfo],
        args: cmd_args,
        env: dict[str, ArgLike]) -> CommandAliasOutput:
    if target_os.script == ScriptLanguage("sh"):
        trampoline, hidden = _command_alias_write_trampoline_unix(ctx, base, args, env)
    elif target_os.script == ScriptLanguage("bat"):
        trampoline, hidden = _command_alias_write_trampoline_windows(ctx, base, args, env)
    else:
        fail("Unsupported script language: {}".format(target_os.script))

    run_info_args_args = []

    # FIXME(JakobDegen): We should not accept `platform_exe` as meaning `run_using_single_arg`, but
    # there are things that depend on that
    if ctx.attrs.run_using_single_arg or \
       len(env) > 0 or \
       isinstance(base, dict) or \
       len(ctx.attrs.platform_exe) > 0:
        run_info_args_args.append(trampoline)
    else:
        run_info_args_args.append(base.args)
        run_info_args_args.append(args)

    run_info_args = cmd_args(run_info_args_args, hidden = hidden)

    return CommandAliasOutput(
        output = DefaultInfo(
            default_output = trampoline,
            other_outputs = [hidden],
        ),
        run_info = RunInfo(args = run_info_args),
    )

def _command_alias_write_trampoline_unix(
        ctx: AnalysisContext,
        # Either the `RunInfo` to use, or in the case of a fat platform, the choice of `RunInfo`
        # depending on `uname`
        base: RunInfo | dict[str, RunInfo],
        args: cmd_args,
        env: dict[str, ArgLike]) -> (Artifact, cmd_args):
    trampoline_args = cmd_args()
    trampoline_args.add("#!/usr/bin/env bash")
    trampoline_args.add("set -euo pipefail")

    if isinstance(base, dict):
        trampoline_args.add('case "$(uname)" in')
        for uname, run_info in base.items():
            trampoline_args.add("    {})".format(uname))
            _add_args_declaration_to_trampoline_args(trampoline_args, run_info, args)
            trampoline_args.add("        ;;")
        trampoline_args.add("esac")
    else:
        _add_args_declaration_to_trampoline_args(trampoline_args, base, args)

    # We can't use cwd relative paths (since we don't know the cwd when this script is run) and so
    # we instead use paths relative to the script itself. However, we can't just naively stick a
    # `$(...)` into `absolute_prefix`, since we must also shell quote paths, and that expression
    # would be shell quoted.
    #
    # Instead, we use `BUCK_COMMAND_ALIAS_ABSOLUTE_PREFIX/`, verbatim, as an absolute prefix on the
    # cmd_args, and then replace that with the actual path of the script at runtime
    trampoline_args.add(
        """
BASE=$(cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P)
R_ARGS=()
for arg in "${ARGS[@]}"; do
    R_ARGS+=("${arg//BUCK_COMMAND_ALIAS_ABSOLUTE_PREFIX/$BASE}")
done
""",
    )

    for (k, v) in env.items():
        # TODO(akozhevnikov): maybe check environment variable is not conflicting with pre-existing one
        trampoline_args.add(cmd_args("export ", k, "=", cmd_args(v, quote = "shell"), delimiter = ""))
        trampoline_args.add(cmd_args("export ", k, '="${', k, '//BUCK_COMMAND_ALIAS_ABSOLUTE_PREFIX/$BASE}"', delimiter = ""))

    trampoline_args.add('exec "${R_ARGS[@]}" "$@"')

    trampoline = ctx.actions.declare_output("__command_alias_trampoline.sh")
    trampoline_args = cmd_args(
        trampoline_args,
        relative_to = (trampoline, 1),
        absolute_prefix = "BUCK_COMMAND_ALIAS_ABSOLUTE_PREFIX/",
    )
    ctx.actions.write(
        trampoline.as_output(),
        trampoline_args,
        allow_args = True,
        is_executable = True,
    )

    return trampoline, trampoline_args

def _command_alias_write_trampoline_windows(
        ctx: AnalysisContext,
        base: RunInfo,
        args: cmd_args,
        env: dict[str, ArgLike]) -> (Artifact, cmd_args):
    trampoline_args = cmd_args()
    trampoline_args.add("@echo off")

    if "close_stdin" in ctx.attrs.labels:
        # Avoids waiting for input on the "Terminate batch job (Y/N)?" prompt.
        # The prompt itself is unavoidable, but we can avoid having to wait for input.
        # This will call the same trampoline batch file with stdin disabled
        trampoline_args.add("if not defined STDIN_CLOSED (set STDIN_CLOSED=1 & CALL <NUL %0 %* & GOTO :EOF)")

    # Set BUCK_COMMAND_ALIAS_ABSOLUTE to the drive and full path of the script being created here
    # We use this below to prefix any artifacts being referenced in the script
    trampoline_args.add("set BUCK_COMMAND_ALIAS_ABSOLUTE=%~dp0")

    # Handle envs
    for (k, v) in env.items():
        # TODO(akozhevnikov): maybe check environment variable is not conflicting with pre-existing one
        trampoline_args.add(cmd_args(["set ", k, "=", v], delimiter = ""))

    # FIXME(JakobDegen): This should be batch quoting, not shell quoting
    cmd = cmd_args(cmd_args(base.args, args, quote = "shell"), "%*", delimiter = " ")

    trampoline_args.add(cmd)

    trampoline = ctx.actions.declare_output("__command_alias_trampoline.bat")
    trampoline_args = cmd_args(
        trampoline_args,
        relative_to = (trampoline, 1),
        absolute_prefix = "%BUCK_COMMAND_ALIAS_ABSOLUTE%/",
    )
    ctx.actions.write(
        trampoline.as_output(),
        trampoline_args,
        allow_args = True,
        is_executable = True,
    )

    return trampoline, trampoline_args

def _add_args_declaration_to_trampoline_args(trampoline_args: cmd_args, base: RunInfo, args: cmd_args):
    trampoline_args.add("ARGS=(")
    trampoline_args.add(cmd_args(base.args, args, quote = "shell"))
    trampoline_args.add(")")
