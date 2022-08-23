load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidPlatformInfo", "AndroidToolchainInfo")
load("@fbsource//third-party/toolchains/android-sdk:defs.bzl", "OPTIONAL_JAR_NAMES")
load("@fbsource//tools/build_defs:buckconfig.bzl", "read", "read_int")

def config_backed_android_toolchain(
        name,
        apk_builder,
        apk_module_graph,
        compress_libraries,
        d8_command,
        multi_dex_command,
        copy_string_resources,
        filter_dex_class_names,
        filter_prebuilt_native_library_dir,
        filter_resources,
        generate_build_config,
        generate_manifest,
        manifest_utils,
        merge_android_resources,
        merge_assets,
        mini_aapt,
        native_libs_as_assets_metadata,
        package_strings_as_assets,
        unpack_aar,
        **kwargs):
    kwargs["aidl"] = "fbsource//third-party/toolchains/android-sdk:aidl"
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
    kwargs["apk_module_graph"] = apk_module_graph
    kwargs["compress_libraries"] = compress_libraries
    kwargs["d8_command"] = d8_command
    kwargs["multi_dex_command"] = multi_dex_command
    kwargs["filter_dex_class_names"] = filter_dex_class_names
    kwargs["filter_prebuilt_native_library_dir"] = filter_prebuilt_native_library_dir
    kwargs["secondary_dex_weight_limit"] = read_int("android", "secondary_dex_weight_limit", 12 * 1024 * 1024)
    kwargs["copy_string_resources"] = copy_string_resources
    kwargs["filter_resources"] = filter_resources
    kwargs["generate_build_config"] = generate_build_config
    kwargs["generate_manifest"] = generate_manifest
    kwargs["manifest_utils"] = manifest_utils
    kwargs["merge_android_resources"] = merge_android_resources
    kwargs["merge_assets"] = merge_assets
    kwargs["mini_aapt"] = mini_aapt
    kwargs["native_libs_as_assets_metadata"] = native_libs_as_assets_metadata
    kwargs["package_strings_as_assets"] = package_strings_as_assets
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
    bootclasspath = [ctx.attrs.android_jar] + ctx.attrs.android_optional_jars
    return [
        DefaultInfo(),
        AndroidPlatformInfo(
            name = ctx.attrs.name,
        ),
        AndroidToolchainInfo(
            aapt2 = ctx.attrs.aapt2[RunInfo],
            adb = ctx.attrs.adb[RunInfo],
            aidl = ctx.attrs.aidl[RunInfo],
            android_bootclasspath = bootclasspath,
            android_jar = ctx.attrs.android_jar,
            apk_builder = ctx.attrs.apk_builder,
            apk_module_graph = ctx.attrs.apk_module_graph,
            compress_libraries = ctx.attrs.compress_libraries,
            d8_command = ctx.attrs.d8_command,
            filter_dex_class_names = ctx.attrs.filter_dex_class_names,
            filter_prebuilt_native_library_dir = ctx.attrs.filter_prebuilt_native_library_dir,
            multi_dex_command = ctx.attrs.multi_dex_command,
            copy_string_resources = ctx.attrs.copy_string_resources,
            filter_resources = ctx.attrs.filter_resources,
            framework_aidl_file = ctx.attrs.framework_aidl_file,
            generate_build_config = ctx.attrs.generate_build_config,
            generate_manifest = ctx.attrs.generate_manifest,
            instrumentation_test_runner_classpath = ctx.attrs.instrumentation_test_runner_classpath,
            instrumentation_test_runner_main_class = ctx.attrs.instrumentation_test_runner_main_class,
            manifest_utils = ctx.attrs.manifest_utils,
            merge_android_resources = ctx.attrs.merge_android_resources,
            merge_assets = ctx.attrs.merge_assets,
            mini_aapt = ctx.attrs.mini_aapt,
            optimized_proguard_config = ctx.attrs.optimized_proguard_config,
            native_libs_as_assets_metadata = ctx.attrs.native_libs_as_assets_metadata,
            package_strings_as_assets = ctx.attrs.package_strings_as_assets,
            proguard_config = ctx.attrs.proguard_config,
            proguard_jar = ctx.attrs.proguard_jar,
            proguard_max_heap_size = ctx.attrs.proguard_max_heap_size,
            secondary_dex_weight_limit = ctx.attrs.secondary_dex_weight_limit,
            unpack_aar = ctx.attrs.unpack_aar,
            zipalign = ctx.attrs.zipalign,
        ),
    ]

_config_backed_android_toolchain_rule = rule(
    attrs = {
        "aapt2": attrs.dep(providers = [RunInfo]),
        "adb": attrs.dep(providers = [RunInfo]),
        "aidl": attrs.dep(providers = [RunInfo]),
        "android_jar": attrs.source(),
        "android_optional_jars": attrs.list(attrs.source()),
        "apk_builder": attrs.dep(providers = [RunInfo]),
        "apk_module_graph": attrs.dep(providers = [RunInfo]),
        "compress_libraries": attrs.dep(providers = [RunInfo]),
        "copy_string_resources": attrs.dep(providers = [RunInfo]),
        "d8_command": attrs.dep(providers = [RunInfo]),
        "filter_dex_class_names": attrs.dep(providers = [RunInfo]),
        "filter_prebuilt_native_library_dir": attrs.dep(providers = [RunInfo]),
        "filter_resources": attrs.dep(providers = [RunInfo]),
        "framework_aidl_file": attrs.source(),
        "generate_build_config": attrs.dep(providers = [RunInfo]),
        "generate_manifest": attrs.dep(providers = [RunInfo]),
        "instrumentation_test_runner_classpath": attrs.list(attrs.source()),
        "instrumentation_test_runner_main_class": attrs.string(),
        "manifest_utils": attrs.dep(providers = [RunInfo]),
        "merge_android_resources": attrs.dep(providers = [RunInfo]),
        "merge_assets": attrs.dep(providers = [RunInfo]),
        "mini_aapt": attrs.dep(providers = [RunInfo]),
        "multi_dex_command": attrs.dep(providers = [RunInfo]),
        "native_libs_as_assets_metadata": attrs.dep(providers = [RunInfo]),
        "optimized_proguard_config": attrs.source(),
        "package_strings_as_assets": attrs.dep(providers = [RunInfo]),
        "proguard_config": attrs.source(),
        "proguard_jar": attrs.source(),
        "proguard_max_heap_size": attrs.string(),
        "secondary_dex_weight_limit": attrs.int(),
        "unpack_aar": attrs.dep(providers = [RunInfo]),
        "zipalign": attrs.dep(providers = [RunInfo]),
    },
    impl = _config_backed_android_toolchain_rule_impl,
)
