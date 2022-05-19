def _nested_subtargets(ctx):
    out = ctx.actions.write("foo", content = "foo_content")

    nested_info = [DefaultInfo(
        sub_targets = {"nested_sub": [
            DefaultInfo(default_outputs = [out]),
        ]},
    )]

    return [DefaultInfo(
        sub_targets = {"sub": nested_info},
    )]

nested_subtargets = rule(
    implementation = _nested_subtargets,
    attrs = {},
)
