load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidPlatformInfo", "AndroidToolchainInfo")
load("@fbsource//third-party/toolchains/android-sdk:defs.bzl", "OPTIONAL_JAR_NAMES")
load("@fbsource//tools/build_defs:buckconfig.bzl", "read", "read_int")

def config_backed_android_toolchain(
        name,
        apk_builder,
        d8_command,
        multi_dex_command,
        copy_string_resources,
        filter_resources,
        generate_build_config,
        generate_manifest,
        manifest_utils,
        merge_android_resources,
        merge_assets,
        mini_aapt,
        unpack_aar,
        **kwargs):
    kwargs["aidl"] = "fbsource//third-party/toolchains/android-sdk:aidl_and_deps"
    kwargs["framework_aidl_file"] = "fbsource//third-party/toolchains/android-sdk:framework_aidl_file"

    aapt2_config = read("tools", "aapt2")
    kwargs["aapt2"] = aapt2_config if aapt2_config else "fbsource//third-party/toolchains/android-sdk:aapt2"
    kwargs["adb"] = "fbsource//third-party/toolchains/android-sdk:adb"
    kwargs["zipalign"] = "fbsource//third-party/toolchains/android-sdk:zipalign_and_deps"

    kwargs["proguard_config"] = "fbsource//third-party/toolchains/android-sdk:proguard_config"
    kwargs["optimized_proguard_config"] = "fbsource//third-party/toolchains/android-sdk:optimized_proguard_config"
    kwargs["proguard_max_heap_size"] = read("tools", "proguard-max-heap-size", "1024M")
    kwargs["proguard_jar"] = read("tools", "proguard", "fbsource//third-party/toolchains/android-sdk:proguard.jar")

    kwargs["android_jar"] = "fbsource//third-party/toolchains/android-sdk:android.jar"
    kwargs["android_optional_jars"] = [
        "fbsource//third-party/toolchains/android-sdk:{}".format(jar_name)
        for jar_name in OPTIONAL_JAR_NAMES
    ]

    kwargs["apk_builder"] = apk_builder
    kwargs["d8_command"] = d8_command
    kwargs["multi_dex_command"] = multi_dex_command
    kwargs["secondary_dex_weight_limit"] = read_int("android", "secondary_dex_weight_limit", 12 * 1024 * 1024)
    kwargs["copy_string_resources"] = copy_string_resources
    kwargs["filter_resources"] = filter_resources
    kwargs["generate_build_config"] = generate_build_config
    kwargs["generate_manifest"] = generate_manifest
    kwargs["manifest_utils"] = manifest_utils
    kwargs["merge_android_resources"] = merge_android_resources
    kwargs["merge_assets"] = merge_assets
    kwargs["mini_aapt"] = mini_aapt
    kwargs["unpack_aar"] = unpack_aar

    kwargs["instrumentation_test_runner_classpath"] = [
        "buck//src/com/facebook/buck/testrunner:testrunner-bin-fixed",
        "buck//third-party/java/android:common",
        "buck//third-party/java/android:ddmlib",
        "buck//third-party/java/guava:shaded-guava-20",
        "buck//third-party/java/kxml2:kxml2",
    ]
    kwargs["instrumentation_test_runner_main_class"] = "com.facebook.buck.testrunner.InstrumentationMain"

    _config_backed_android_toolchain_rule(
        name = name,
        **kwargs
    )

def _config_backed_android_toolchain_rule_impl(ctx):
    bootclasspath = [ctx.attr.android_jar] + ctx.attr.android_optional_jars
    return [
        DefaultInfo(),
        AndroidPlatformInfo(
            name = ctx.attr.name,
        ),
        AndroidToolchainInfo(
            aapt2 = ctx.attr.aapt2[RunInfo],
            adb = ctx.attr.adb[RunInfo],
            aidl = ctx.attr.aidl[RunInfo],
            android_bootclasspath = bootclasspath,
            android_jar = ctx.attr.android_jar,
            apk_builder = ctx.attr.apk_builder,
            d8_command = ctx.attr.d8_command,
            multi_dex_command = ctx.attr.multi_dex_command,
            copy_string_resources = ctx.attr.copy_string_resources,
            filter_resources = ctx.attr.filter_resources,
            framework_aidl_file = ctx.attr.framework_aidl_file,
            generate_build_config = ctx.attr.generate_build_config,
            generate_manifest = ctx.attr.generate_manifest,
            instrumentation_test_runner_classpath = ctx.attr.instrumentation_test_runner_classpath,
            instrumentation_test_runner_main_class = ctx.attr.instrumentation_test_runner_main_class,
            manifest_utils = ctx.attr.manifest_utils,
            merge_android_resources = ctx.attr.merge_android_resources,
            merge_assets = ctx.attr.merge_assets,
            mini_aapt = ctx.attr.mini_aapt,
            optimized_proguard_config = ctx.attr.optimized_proguard_config,
            proguard_config = ctx.attr.proguard_config,
            proguard_jar = ctx.attr.proguard_jar,
            proguard_max_heap_size = ctx.attr.proguard_max_heap_size,
            secondary_dex_weight_limit = ctx.attr.secondary_dex_weight_limit,
            unpack_aar = ctx.attr.unpack_aar,
            zipalign = ctx.attr.zipalign,
        ),
    ]

_config_backed_android_toolchain_rule = rule(
    attrs = {
        "aapt2": attr.dep(providers = [RunInfo]),
        "adb": attr.dep(providers = [RunInfo]),
        "aidl": attr.dep(providers = [RunInfo]),
        "android_jar": attr.source(),
        "android_optional_jars": attr.list(attr.source()),
        "apk_builder": attr.dep(providers = [RunInfo]),
        "copy_string_resources": attr.dep(providers = [RunInfo]),
        "d8_command": attr.dep(providers = [RunInfo]),
        "filter_resources": attr.dep(providers = [RunInfo]),
        "framework_aidl_file": attr.source(),
        "generate_build_config": attr.dep(providers = [RunInfo]),
        "generate_manifest": attr.dep(providers = [RunInfo]),
        "instrumentation_test_runner_classpath": attr.list(attr.source()),
        "instrumentation_test_runner_main_class": attr.string(),
        "manifest_utils": attr.dep(providers = [RunInfo]),
        "merge_android_resources": attr.dep(providers = [RunInfo]),
        "merge_assets": attr.dep(providers = [RunInfo]),
        "mini_aapt": attr.dep(providers = [RunInfo]),
        "multi_dex_command": attr.dep(providers = [RunInfo]),
        "optimized_proguard_config": attr.source(),
        "proguard_config": attr.source(),
        "proguard_jar": attr.source(),
        "proguard_max_heap_size": attr.string(),
        "secondary_dex_weight_limit": attr.int(),
        "unpack_aar": attr.dep(providers = [RunInfo]),
        "zipalign": attr.dep(providers = [RunInfo]),
    },
    implementation = _config_backed_android_toolchain_rule_impl,
)
