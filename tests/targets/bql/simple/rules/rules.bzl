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

_foo_library = rule(
    impl = _impl,
    attrs = {
        "cmd": attr.list(attr.arg(), default = []),
        "deps": attr.list(attr.dep(), default = []),
        "description": attr.string(default = ""),
        "mapped_srcs": attr.dict(attr.string(), attr.source(), default = {}),
        "srcs": attr.list(attr.source(), default = []),
        "tuple_srcs": attr.option(attr.tuple(attr.source(), attr.source(), attr.source())),
    },
)

_foo_binary = rule(
    impl = _binary_impl,
    attrs = {
        "cmd": attr.list(attr.arg(), default = []),
        "deps": attr.list(attr.dep(), default = []),
        "description": attr.string(default = ""),
        "srcs": attr.list(attr.source(), default = []),
        "_foo_toolchain": attr.exec_dep(default = "root//:foo_toolchain"),
    },
)

_foo_genrule = rule(
    impl = _binary_impl,
    attrs = {
        "cmd": attr.arg(),
        "description": attr.string(default = ""),
        "out": attr.string(default = ""),
    },
)

_default_platform = "root//platforms:platform1"

def foo_library(**kwargs):
    _foo_library(default_target_platform = _default_platform, **kwargs)

def foo_binary(**kwargs):
    _foo_binary(default_target_platform = _default_platform, **kwargs)

def foo_genrule(**kwargs):
    _foo_genrule(default_target_platform = _default_platform, **kwargs)
