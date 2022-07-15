load(":tr.bzl", "iphone_to_watch_transition")

def _my_little_iphone_binary_impl(ctx):
    [_watchos_resource, _default_resource] = ctx.attrs.resources
    return [
        DefaultInfo(),
    ]

my_little_iphone_binary = rule(
    impl = _my_little_iphone_binary_impl,
    attrs = {
        "resources": attrs.list(attrs.dep()),
    },
)

def _my_watchos_resource_impl(ctx):
    # Assert that configuration is indeed transitioned, and select is resolved
    # to the correct configuration.
    if ctx.attrs.resource_value != "watchos":
        fail("Expected resource value to be watchos, got: {}".format(ctx.attrs.resource_value))
    return [
        DefaultInfo(),
    ]

my_watchos_resource = rule(
    impl = _my_watchos_resource_impl,
    # Resource with transition to watchOS.
    cfg = iphone_to_watch_transition,
    attrs = {
        "resource_value": attrs.string(),
    },
)

def _my_default_resource_impl(ctx):
    if ctx.attrs.resource_value != "DEFAULT":
        fail("Expected resource value to be DEFAULT, got: {}".format(ctx.attrs.resource_value))
    return [
        DefaultInfo(),
    ]

my_default_resource = rule(
    impl = _my_default_resource_impl,
    # No transition here.
    attrs = {
        "resource_value": attrs.string(),
    },
)
