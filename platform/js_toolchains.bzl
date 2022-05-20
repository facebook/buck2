load("@fbcode//buck2/prelude/js:js_providers.bzl", "JsToolchainInfo")

def _js_toolchain_rule_impl(_ctx):
    return [
        DefaultInfo(),
        JsToolchainInfo(),
    ]

js_toolchain = rule(
    attrs = {},
    implementation = _js_toolchain_rule_impl,
)
