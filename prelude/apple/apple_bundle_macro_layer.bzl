load(":apple_bundle_config.bzl", "apple_bundle_config")
load(":apple_info_plist_substitutions_parsing.bzl", "parse_codesign_entitlements")

def apple_bundle_macro_impl(apple_bundle_rule = None, info_plist_substitutions = None, **kwargs):
    kwargs.update(apple_bundle_config())
    apple_bundle_rule(
        info_plist_substitutions = info_plist_substitutions,
        _codesign_entitlements = parse_codesign_entitlements(info_plist_substitutions),
        **kwargs
    )
