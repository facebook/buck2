def _printing_binary_impl(ctx):
    if host_info().os.is_windows:
        script_contents = """\
@echo off
setlocal EnableExtensions EnableDelayedExpansion
for /F %%a in ('echo prompt $E ^| cmd') do set "ESC=%%a"
(echo warning on stderr no color)1>&2
(echo !ESC![33mwarning on stderr with color!ESC![0m)1>&2
for %%F in (%1) do set dirname=%%~dpF
if not exist %dirname% md %dirname%
type nul > %1
"""
        script = ctx.actions.write("cmd.bat", script_contents)
        script_args = ["cmd.exe", "/c", script]
    else:
        script_contents = """\
#!/bin/bash
echo "warning on stderr no color" >&2
echo -en "\\x1b[33mwarning on stderr with color" >&2
mkdir -p "$(dirname "$1")"
touch "$1"
"""
        script = ctx.actions.write("cmd.sh", script_contents, is_executable = True)
        script_args = [script]

    out = ctx.actions.declare_output("out.txt")
    script_args.append(out.as_output())
    ctx.actions.run(
        script_args,
        always_print_stderr = ctx.attr.always_print,
        category = "printer",
        identifier = "writing_stderr",
    )
    return [DefaultInfo(default_outputs = [out])]

printing_binary = rule(implementation = _printing_binary_impl, attrs = {"always_print": attr.bool()})
