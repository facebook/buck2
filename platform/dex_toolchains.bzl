load("@fbcode//buck2/prelude/java:dex_toolchain.bzl", "DexToolchainInfo")

def empty_dex_toolchain(
        name,
        **kwargs):
    _config_backed_dex_toolchain_rule(
        name = name,
        **kwargs
    )

def config_backed_dex_toolchain(
        name,
        d8_command = "buck//src/com/facebook/buck/android/dex:run_d8_binary",
        **kwargs):
    # TODO(T107163344) These don't belong here! Move out to Android toolchain once we have overlays.
    kwargs["android_jar"] = "fbsource//third-party/toolchains/android-sdk:android.jar"
    kwargs["d8_command_binary"] = d8_command

    _config_backed_dex_toolchain_rule(
        name = name,
        **kwargs
    )

def _config_backed_dex_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        DexToolchainInfo(
            android_jar = ctx.attr.android_jar,
            d8_command = ctx.attr.d8_command_binary,
        ),
    ]

_config_backed_dex_toolchain_rule = rule(
    attrs = {
        "android_jar": attr.option(attr.source()),
        "d8_command_binary": attr.option(attr.dep(providers = [RunInfo])),
    },
    implementation = _config_backed_dex_toolchain_rule_impl,
)
