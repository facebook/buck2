# Implementation of aliases build rules.

def alias_impl(ctx: "context") -> ["provider"]:
    return ctx.attrs.actual.providers

def configured_alias_impl(ctx: "context") -> ["provider"]:
    if ctx.attrs.configured_actual != None:
        return ctx.attrs.configured_actual.providers
    if ctx.attrs.fallback_actual != None:
        return ctx.attrs.fallback_actual.providers
    fail("must set one of `configured_actual` or `fallback_actual`")

def versioned_alias_impl(_ctx: "context") -> ["provider"]:
    # Should be intercepted in macro stub and converted to `alias`.
    fail("unsupported")
