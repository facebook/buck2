# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

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

    output = command_alias(
        actions = ctx.actions,
        path = ctx.attrs.executable_name,
        target_os = target_os,
        base = base,
        args = cmd_args(ctx.attrs.args),
        env = ctx.attrs.env,
        labels = ctx.attrs.labels,
    )

    default_info = DefaultInfo(
        default_output = output.output.default_outputs[0],
        other_outputs = list(output.output.other_outputs) + ctx.attrs.resources,
    )

    # FIXME(JakobDegen): We should not accept `platform_exe` as meaning `run_using_single_arg`, but
    # there are things that depend on that
    #
    # FIXME(JakobDegen): It's easy to end up depending on either one of these behaviors. Life would
    # probably be easier if we just always went the `output.cmd` route
    if output.maybe_directly_runnable == None or \
       ctx.attrs.run_using_single_arg or \
       ctx.attrs.executable_name != None or \
       (len(ctx.attrs.platform_exe) > 0 and target_os.script == ScriptLanguage("sh")):
        run_info = RunInfo(args = output.cmd)
    else:
        run_info = RunInfo(args = output.maybe_directly_runnable)

    run_info_with_resources = RunInfo(
        args = cmd_args(run_info, hidden = ctx.attrs.resources),
    )

    return [default_info, run_info_with_resources]

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
    # The output wrapped into a `cmd_args` in the obvious way
    cmd = cmd_args,
    # For some values of the arguments to `command_alias` it is possible to represent the
    # command_alias entirely within a `cmd_args`, without needing to write out a script. If that is
    # the case, those args are made available here.
    maybe_directly_runnable = cmd_args | None,
)

def command_alias(
        *,
        actions: AnalysisActions,
        # The path at which to write the output to, without an extension - that will be added
        path: str | None,
        # The target where this script should be able to run (this may actually be your exec platform)
        target_os: OsLookup,
        # Either the `RunInfo` to use, or in the case of a fat platform, the choice of `RunInfo`
        # depending on `uname`
        base: RunInfo | dict[str, RunInfo],
        args: cmd_args,
        env: dict[str, ArgLike],
        labels: list[str]) -> CommandAliasOutput:
    if path == "":
        fail("Path cannot be empty string")

    if path != None:
        # Custom paths are usually provided for binaries that are searched for with a particular
        # name. On Windows, the .bat extension is required, and well-behaved tools will respect the
        # PATHEXT environment variable (which contains that extension). On Unix, no extension is
        # expected for executables, and searches would likely fail if we added one.
        unix_trampoline_path = path
        windows_trampoline_path = path + ".bat"
    else:
        # Preserve the `.sh` extension in the default path, since many places have it hard-coded.
        unix_trampoline_path = "__command_alias_trampoline.sh"
        windows_trampoline_path = "__command_alias_trampoline.bat"

    if target_os.script == ScriptLanguage("sh"):
        trampoline, hidden = _command_alias_write_trampoline_unix(actions, unix_trampoline_path, base, args, env)
    elif target_os.script == ScriptLanguage("bat"):
        trampoline, hidden = _command_alias_write_trampoline_windows(actions, windows_trampoline_path, base, args, env, labels)
    else:
        fail("Unsupported script language: {}".format(target_os.script))

    if len(env) > 0 or isinstance(base, dict):
        maybe_directly_runnable = None
    else:
        maybe_directly_runnable = cmd_args(base.args, args)

    return CommandAliasOutput(
        output = DefaultInfo(
            default_output = trampoline,
            other_outputs = [hidden],
        ),
        cmd = cmd_args(trampoline, hidden = hidden),
        maybe_directly_runnable = maybe_directly_runnable,
    )

def _command_alias_write_trampoline_unix(
        actions: AnalysisActions,
        path: str,
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
    #
    # Resolve symlinks first to handle execution via symlinks (e.g., in link-trees)
    trampoline_args.add(
        """
SCRIPT_PATH="$0"
if [ -L "$SCRIPT_PATH" ]; then
    TARGET="$(readlink "$SCRIPT_PATH")"
    SCRIPT_PATH="$(dirname "$SCRIPT_PATH")/$TARGET"
fi
""",
    )

    # Calculate the base path and process arguments with the resolved path
    trampoline_args.add(
        """
BASE=$(cd -- "$(dirname "$SCRIPT_PATH")" >/dev/null 2>&1 ; pwd -P)
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

    trampoline = actions.declare_output(path)
    trampoline_args = cmd_args(
        trampoline_args,
        relative_to = (trampoline, 1),
        absolute_prefix = "BUCK_COMMAND_ALIAS_ABSOLUTE_PREFIX/",
    )
    actions.write(
        trampoline.as_output(),
        trampoline_args,
        allow_args = True,
        is_executable = True,
    )

    return trampoline, trampoline_args

def _command_alias_write_trampoline_windows(
        actions: AnalysisActions,
        path: str,
        base: RunInfo,
        args: cmd_args,
        env: dict[str, ArgLike],
        labels: list[str]) -> (Artifact, cmd_args):
    trampoline_args = cmd_args()
    trampoline_args.add("@echo off")

    if "close_stdin" in labels:
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

    trampoline = actions.declare_output(path)
    trampoline_args = cmd_args(
        trampoline_args,
        relative_to = (trampoline, 1),
        absolute_prefix = "%BUCK_COMMAND_ALIAS_ABSOLUTE%/",
    )
    actions.write(
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
