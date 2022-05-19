def assert_eq(a, b):
    if a != b:
        fail("Expected equal, but got", a, b)

def providers_assert_impl(ctx: "context") -> ["provider"]:
    info = ctx.attr.dep[DefaultInfo]
    assert_eq(info.default_outputs, [])
    assert_eq(info.sub_targets.keys(), ["multiple", "single"])
    assert_eq(len(info.sub_targets["multiple"][DefaultInfo].default_outputs), 2)
    assert_eq(len(info.sub_targets["single"][DefaultInfo].default_outputs), 1)
    return [DefaultInfo()]

providers_assert = rule(
    implementation = providers_assert_impl,
    attrs = {"dep": attr.dep()},
)
