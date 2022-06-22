load(":apple_bundle_config.bzl", "apple_bundle_config")

def apple_test_macro_impl(apple_test_rule = None, **kwargs):
    kwargs.update(apple_bundle_config())
    apple_test_rule(
        **kwargs
    )
