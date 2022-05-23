def _test_in_subtarget_impl(ctx):
    test_info = ExternalRunnerTestInfo(
        type = "custom",
        command = [ctx.attr.test],
        env = {},
        labels = [],
        contacts = ["buck2"],
    )
    return [DefaultInfo(
        sub_targets = {"sub": [test_info]},
    )]

test_in_subtarget = rule(
    implementation = _test_in_subtarget_impl,
    attrs = {
        "test": attr.arg(),
    },
)
