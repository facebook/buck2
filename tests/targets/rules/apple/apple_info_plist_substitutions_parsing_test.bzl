load("@fbcode//buck2/prelude/apple:apple_info_plist_substitutions_parsing.bzl", "parse_codesign_entitlements")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

def _test_parse_codesign_entitlements(ctx):
    result = parse_codesign_entitlements(ctx.attrs.info_plist_substitutions)
    expected = ctx.attrs.expected_code_sign_entitlements
    expect(result == expected, "Expected `{}`, got `{}`".format(expected, result))
    return [DefaultInfo()]

test_parse_code_sign_entitlements = rule(
    impl = _test_parse_codesign_entitlements,
    attrs = {
        "expected_code_sign_entitlements": attrs.option(attrs.string()),
        "info_plist_substitutions": attrs.option(attrs.dict(key = attrs.string(), value = attrs.string())),
    },
)
