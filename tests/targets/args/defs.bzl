def _toolchain_impl(_ctx):
    return [
        DefaultInfo(),
        TemplatePlaceholderInfo(
            unkeyed_variables = {
                "FOO": "<foo_compiler>",
                "FOO_FLAGS": "<foo_compiler_flags>",
            },
        ),
    ]

def _library_impl(ctx):
    return [
        DefaultInfo(),
        TemplatePlaceholderInfo(
            keyed_variables = {
                "LIB_FLAGS": cmd_args(ctx.attrs.flags),
                "NAME": ctx.label.name,
            },
        ),
    ]

def _binary_impl(ctx):
    output, _ = ctx.actions.write("out", ctx.attrs.flags, allow_args = True)
    return [
        DefaultInfo(
            default_outputs = [output],
        ),
    ]

foo_toolchain = rule(
    impl = _toolchain_impl,
    attrs = {},
)

foo_library = rule(
    impl = _library_impl,
    attrs = {
        "flags": attrs.list(attrs.arg()),
    },
)

foo_binary = rule(
    impl = _binary_impl,
    attrs = {
        "flags": attrs.list(attrs.arg()),
        "_toolchains": attrs.list(attrs.dep(), default = ["//:toolchain"]),
    },
)
