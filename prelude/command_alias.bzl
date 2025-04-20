# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//os_lookup:defs.bzl", "Os", "OsLookup", "ScriptLanguage")
load("@prelude//utils:arglike.bzl", "ArgLike")

def command_alias_impl(ctx: AnalysisContext):
    target_os = ctx.attrs._target_os_type[OsLookup]

    if target_os.os == Os("fat_mac_linux") and len(ctx.attrs.platform_exe) > 0:
        variants = {
            "Darwin": _get_os_base(ctx, Os("macos")),
            "Linux": _get_os_base(ctx, Os("linux")),
        }
        return _command_alias_impl_target_unix(ctx, variants)

    base = _get_os_base(ctx, target_os.os)
    if target_os.script == ScriptLanguage("sh"):
        return _command_alias_impl_target_unix(ctx, base)
    elif target_os.script == ScriptLanguage("bat"):
        return _command_alias_impl_target_windows(ctx, base)
    else:
        fail("Unsupported script language: {}".format(target_os.script))

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

def _command_alias_impl_target_unix(
        ctx: AnalysisContext,
        # Either the `RunInfo` to use, or in the case of a fat platform, the choice of `RunInfo`
        # depending on `uname`
        base: RunInfo | dict[str, RunInfo]) -> list[Provider]:
    trampoline_args = cmd_args()
    trampoline_args.add("#!/usr/bin/env bash")
    trampoline_args.add("set -euo pipefail")

    if isinstance(base, dict):
        trampoline_args.add('case "$(uname)" in')
        for uname, run_info in base.items():
            trampoline_args.add("    {})".format(uname))
            _add_args_declaration_to_trampoline_args(trampoline_args, run_info, ctx.attrs.args)
            trampoline_args.add("        ;;")
        trampoline_args.add("esac")
    else:
        _add_args_declaration_to_trampoline_args(trampoline_args, base, ctx.attrs.args)

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

    for (k, v) in ctx.attrs.env.items():
        # TODO(akozhevnikov): maybe check environment variable is not conflicting with pre-existing one
        trampoline_args.add(cmd_args("export ", k, "=", cmd_args(v, quote = "shell"), delimiter = ""))
        trampoline_args.add(cmd_args("export ", k, '="${', k, '//BUCK_COMMAND_ALIAS_ABSOLUTE_PREFIX/$BASE}"', delimiter = ""))

    trampoline_args.add('exec "${R_ARGS[@]}"')

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

    run_info_args_args = []
    run_info_args_hidden = []

    # FIXME(JakobDegen): We should not accept `platform_exe` as meaning `run_using_single_arg`, but
    # there are things that depend on that
    if ctx.attrs.run_using_single_arg or \
       len(ctx.attrs.env) > 0 or \
       isinstance(base, dict) or \
       len(ctx.attrs.platform_exe) > 0:
        run_info_args_args.append(trampoline)
        run_info_args_hidden.append(trampoline_args)
    else:
        run_info_args_args.append(base.args)
        run_info_args_args.append(ctx.attrs.args)

    run_info_args_hidden.append(ctx.attrs.resources)

    run_info_args = cmd_args(run_info_args_args, hidden = run_info_args_hidden)

    return [
        DefaultInfo(default_output = trampoline, other_outputs = [trampoline_args] + ctx.attrs.resources),
        RunInfo(args = run_info_args),
    ]

def _command_alias_impl_target_windows(ctx: AnalysisContext, base: RunInfo):
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
    for (k, v) in ctx.attrs.env.items():
        # TODO(akozhevnikov): maybe check environment variable is not conflicting with pre-existing one
        trampoline_args.add(cmd_args(["set ", k, "=", v], delimiter = ""))

    # Handle args
    # We shell quote the args but not the base. This is due to the same limitation detailed below with T111687922
    cmd = cmd_args([base.args], delimiter = " ")
    for arg in ctx.attrs.args:
        cmd.add(cmd_args(arg, quote = "shell"))

    # Add on %* to handle any other args passed through the command
    cmd.add("%*")

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

    run_info_args_args = []
    run_info_args_hidden = []
    if ctx.attrs.run_using_single_arg or len(ctx.attrs.env) > 0:
        run_info_args_args.append(trampoline)
        run_info_args_hidden.append(trampoline_args)
    else:
        run_info_args_args.append(base.args)
        run_info_args_args.append(ctx.attrs.args)

    run_info_args_hidden.append(ctx.attrs.resources)

    run_info_args = cmd_args(run_info_args_args, hidden = run_info_args_hidden)

    return [
        DefaultInfo(default_output = trampoline, other_outputs = [trampoline_args] + ctx.attrs.resources),
        RunInfo(args = run_info_args),
    ]

def _add_args_declaration_to_trampoline_args(trampoline_args: cmd_args, base: RunInfo, args: list[ArgLike]):
    trampoline_args.add("ARGS=(")

    # FIXME (T111687922): We cannot preserve BUCK_COMMAND_ALIAS_ABSOLUTE *and*
    # quote here...  So we don't quote the exe's RunInfo (which usually has a
    # path and hopefully no args that need quoting), but we quote the args
    # themselves (which usually are literals that might need quoting and
    # hopefully doesn't contain relative paths).
    # FIXME (T111687922): Note that we have no shot at quoting base.args anyway
    # at this time, since we need to quote the individual words, but using
    # `quote = "shell"` would just quote the whole thing into one big word.
    trampoline_args.add(base.args)
    for arg in args:
        trampoline_args.add(cmd_args(arg, quote = "shell"))

    # Add the args passed to the command_alias itself.
    trampoline_args.add('"$@"')

    trampoline_args.add(")")
