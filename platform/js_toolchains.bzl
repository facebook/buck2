load("@fbcode//buck2/prelude/js:js_providers.bzl", "JsToolchainInfo")

def _js_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        JsToolchainInfo(
            command_args_fixup = ctx.attr.command_args_fixup,
        ),
    ]

js_toolchain = rule(
    attrs = {
        "command_args_fixup": attr.dep(),
    },
    implementation = _js_toolchain_rule_impl,
)
