# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//os_lookup:defs.bzl", "ScriptLanguage")

# Takes a cmd_args containing an executable and zero or more arguments to that
# executable, and bundles it together into a script that is callable as a single
# argument.
#
# For example in the Rust rules we have a `linker` + `linker_flags` that we want
# to pass to rustc as a single "-Clinker={}" argument.
#
#     linker_cmd = cmd_args(linker_info.linker, ctx.attrs.linker_flags)
#     linker_wrapper = cmd_script(
#         ctx = ctx,
#         name = "linker_wrapper",
#         cmd = linker_cmd,
#         language = ctx.attrs._exec_os_type[OsLookup].script,
#     )
#     return cmd_args(linker_wrapper, format = "-Clinker={}")
#
def cmd_script(
        ctx: AnalysisContext,
        name: str,
        cmd: cmd_args,
        language: ScriptLanguage = ScriptLanguage("sh"),
        quote: str | None = "shell") -> cmd_args:
    cmd_kwargs = {} if quote == None else {"quote": quote}
    shell_quoted = cmd_args(cmd, **cmd_kwargs)

    if language == ScriptLanguage("sh"):
        wrapper, _ = ctx.actions.write(
            ctx.actions.declare_output("{}.sh".format(name)),
            [
                "#!/usr/bin/env bash",
                cmd_args(cmd_args(shell_quoted, delimiter = " \\\n"), format = "{} \"$@\"\n"),
            ],
            is_executable = True,
            allow_args = True,
        )
    elif language == ScriptLanguage("bat"):
        wrapper, _ = ctx.actions.write(
            ctx.actions.declare_output("{}.bat".format(name)),
            [
                "@echo off",
                cmd_args(cmd_args(shell_quoted, delimiter = "^\n "), format = "{} %*\n"),
            ],
            allow_args = True,
        )
    else:
        fail(language)

    return cmd_args(wrapper, hidden = cmd)
