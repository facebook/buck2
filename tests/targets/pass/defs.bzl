def _printing_binary_impl(ctx):
    script_contents = """\
#!/bin/bash
echo "warning on stderr no color" >&2
echo -en "\\x1b[33mwarning on stderr with color" >&2
mkdir -p "$(dirname "$1")"
touch "$1"
"""
    script = ctx.actions.write("cmd.sh", script_contents, is_executable = True)
    out = ctx.actions.declare_output("out.txt")
    ctx.actions.run(
        [script, out.as_output()],
        always_print_stderr = ctx.attr.always_print,
        category = "printer",
        identifier = "writing_stderr",
    )
    return [DefaultInfo(default_outputs = [out])]

printing_binary = rule(implementation = _printing_binary_impl, attrs = {"always_print": attr.bool()})
