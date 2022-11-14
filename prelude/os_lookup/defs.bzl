load("@prelude//:attributes.bzl", "Platform")

OsLookup = provider(fields = ["platform"])

def _os_lookup_impl(ctx: "context"):
    return [DefaultInfo(), OsLookup(platform = ctx.attrs.platform)]

os_lookup = rule(impl = _os_lookup_impl, attrs = {
    "platform": attrs.enum(Platform),
})
