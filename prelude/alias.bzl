# Implementation of aliases build rules.

def alias_impl(ctx: "context") -> ["provider"]:
    return ctx.attr.actual.providers

def configured_alias_impl(ctx: "context") -> ["provider"]:
    return ctx.attr.configured_actual.providers

def versioned_alias_impl(_ctx: "context") -> ["provider"]:
    # Should be intercepted in macro stub and converted to `alias`.
    fail("unsupported")
