# Foo message rule that accepts one string attribute

def _unified_foo_impl(ctx):
    output = ctx.actions.declare_output("output.txt")
    ctx.actions.write(output, "hello world!")

    return [DefaultInfo(default_output = output)]

unified_foo = rule(
    impl = _unified_foo_impl,
    attrs = {
        "os": attrs.string(),
    },
)
