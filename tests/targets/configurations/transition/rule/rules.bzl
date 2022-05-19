load(":tr.bzl", "iphone_to_watch_transition")

def _my_little_iphone_binary_impl(ctx):
    [_watchos_resource, _default_resource] = ctx.attr.resources
    return [
        DefaultInfo(),
    ]

my_little_iphone_binary = rule(
    implementation = _my_little_iphone_binary_impl,
    attrs = {
        "resources": attr.list(attr.dep()),
    },
)

def _my_watchos_resource_impl(ctx):
    # Assert that configuration is indeed transitioned, and select is resolved
    # to the correct configuration.
    if ctx.attr.resource_value != "watchos":
        fail("Expected resource value to be watchos, got: {}".format(ctx.attr.resource_value))
    return [
        DefaultInfo(),
    ]

my_watchos_resource = rule(
    implementation = _my_watchos_resource_impl,
    # Resource with transition to watchOS.
    cfg = iphone_to_watch_transition,
    attrs = {
        "resource_value": attr.string(),
    },
)

def _my_default_resource_impl(ctx):
    if ctx.attr.resource_value != "DEFAULT":
        fail("Expected resource value to be DEFAULT, got: {}".format(ctx.attr.resource_value))
    return [
        DefaultInfo(),
    ]

my_default_resource = rule(
    implementation = _my_default_resource_impl,
    # No transition here.
    attrs = {
        "resource_value": attr.string(),
    },
)
