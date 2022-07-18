FooInfo = provider(fields = [
    "foo",
])

def _platform_impl(ctx):
    return [
        DefaultInfo(),
        PlatformInfo(
            label = str(ctx.label.raw_target()),
            configuration = ConfigurationInfo(
                constraints = {},
                values = {},
            ),
        ),
    ]

foo_platform = rule(
    impl = _platform_impl,
    attrs = {},
)

def _config_setting_impl(_ctx):
    return [DefaultInfo(), ConfigurationInfo(constraints = {}, values = {})]

foo_config_setting = rule(
    impl = _config_setting_impl,
    attrs = {},
)

def _impl(ctx):
    return [DefaultInfo(), FooInfo(foo = ctx.attrs.name + "_foo")]

def _binary_impl(ctx):
    return [DefaultInfo(), RunInfo(args = []), FooInfo(foo = ctx.attrs.name + "_foo")]

def _buildable_impl(ctx):
    out = ctx.actions.write(ctx.attrs.out, ctx.attrs.content)
    return [DefaultInfo(default_outputs = [out])]

_foo_library = rule(
    impl = _impl,
    attrs = {
        "cmd": attrs.list(attrs.arg(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "description": attrs.string(default = ""),
        "mapped_srcs": attrs.dict(attrs.string(), attrs.source(), default = {}),
        "srcs": attrs.list(attrs.source(), default = []),
        "tuple_srcs": attrs.option(attrs.tuple(attrs.source(), attrs.source(), attrs.source())),
    },
)

_foo_binary = rule(
    impl = _binary_impl,
    attrs = {
        "cmd": attrs.list(attrs.arg(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "description": attrs.string(default = ""),
        "srcs": attrs.list(attrs.source(), default = []),
        "_foo_toolchain": attrs.exec_dep(default = "root//:foo_toolchain"),
    },
)

_foo_genrule = rule(
    impl = _binary_impl,
    attrs = {
        "cmd": attrs.arg(),
        "description": attrs.string(default = ""),
        "out": attrs.string(default = ""),
    },
)

_foo_buildable = rule(
    impl = _buildable_impl,
    attrs = {
        "content": attr.string(default = ""),
        "out": attr.string(),
    },
)

_default_platform = "root//platforms:platform1"

def foo_library(**kwargs):
    _foo_library(default_target_platform = _default_platform, **kwargs)

def foo_binary(**kwargs):
    _foo_binary(default_target_platform = _default_platform, **kwargs)

def foo_genrule(**kwargs):
    _foo_genrule(default_target_platform = _default_platform, **kwargs)

def foo_buildable(**kwargs):
    _foo_buildable(default_target_platform = _default_platform, **kwargs)
