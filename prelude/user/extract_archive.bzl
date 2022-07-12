load("@fbcode//buck2/prelude/utils:utils.bzl", "value_or")
load(":rule_spec.bzl", "RuleRegistrationSpec")

# Buck v2 doesn't support directories as source inputs, while v1 allows that.
# This rule fills that gap and allows to produce a directory from archive,
# which then can be used as an input for other rules.

def _impl(ctx: "context") -> ["provider"]:
    output = ctx.actions.declare_output(value_or(ctx.attr.directory_name, ctx.label.name))
    archive = ctx.attr.contents_archive
    script, hidden = ctx.actions.write(
        "unpack.sh",
        [
            cmd_args(output, format = "mkdir -p {}"),
            cmd_args(output, format = "cd {}"),
            cmd_args(archive, format = "tar -xzf {}").relative_to(output),
        ],
        is_executable = True,
        allow_args = True,
    )
    ctx.actions.run(cmd_args(["/bin/sh", script])
        .hidden(hidden + [archive, output.as_output()]), category = "extract_archive")

    return [DefaultInfo(default_outputs = [output])]

registration_spec = RuleRegistrationSpec(
    name = "extract_archive",
    impl = _impl,
    attrs = {
        # .tar.gz archive with the contents of the result directory
        "contents_archive": attr.source(),
        # name of the result directory, if omitted, `name` attribute will be used instead
        "directory_name": attr.option(attr.string()),
    },
)
