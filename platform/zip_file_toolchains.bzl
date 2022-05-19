load("@fbcode//buck2/prelude/zip_file:zip_file_toolchain.bzl", "ZipFileToolchainInfo")

def config_backed_zip_file_toolchain(name, **kwargs):
    kwargs["create_zip"] = "buck//src/com/facebook/buck/features/zip/rules/utils:zip_binary"

    _config_backed_zip_file_toolchain_rule(
        name = name,
        **kwargs
    )

def _config_backed_zip_file_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        ZipFileToolchainInfo(
            create_zip = ctx.attr.create_zip,
        ),
    ]

_config_backed_zip_file_toolchain_rule = rule(
    attrs = {
        "create_zip": attr.dep(providers = [RunInfo]),
    },
    implementation = _config_backed_zip_file_toolchain_rule_impl,
)
