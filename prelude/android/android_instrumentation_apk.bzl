load("@fbcode//buck2/prelude/android:android_apk.bzl", "build_apk")
load("@fbcode//buck2/prelude/android:android_binary_native_library_rules.bzl", "get_android_binary_native_library_info")
load("@fbcode//buck2/prelude/android:android_binary_resources_rules.bzl", "get_android_binary_resources_info")
load("@fbcode//buck2/prelude/android:android_providers.bzl", "AndroidApkInfo", "AndroidApkUnderTestInfo", "AndroidInstrumentationApkInfo", "merge_android_packageable_info")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/android:configuration.bzl", "get_deps_by_platform")
load("@fbcode//buck2/prelude/android:dex_rules.bzl", "merge_to_single_dex")
load("@fbcode//buck2/prelude/java:java_providers.bzl", "create_java_packaging_dep", "get_all_java_packaging_deps")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

def android_instrumentation_apk_impl(ctx: "context"):
    # To begin with, let's just implement something that has a single DEX file and a manifest.
    _verify_params(ctx)

    apk_under_test_info = ctx.attr.apk[AndroidApkUnderTestInfo]

    # android_instrumentation_apk should just use the same platforms and primary_platform as the APK-under-test
    unfiltered_deps_by_platform = get_deps_by_platform(ctx)
    for platform in apk_under_test_info.platforms:
        expect(
            platform in unfiltered_deps_by_platform,
            "Android instrumentation APK must have any platforms that are in the APK-under-test!",
        )
    deps_by_platform = {platform: deps for platform, deps in unfiltered_deps_by_platform.items() if platform in apk_under_test_info.platforms}
    primary_platform = apk_under_test_info.primary_platform
    deps = deps_by_platform[primary_platform]

    # TODO(T122203218) Filter out packaging deps that are also in the apk_under_test
    java_packaging_deps = [packaging_dep for packaging_dep in get_all_java_packaging_deps(ctx, deps) if packaging_dep.dex]

    # TODO(T122203218) Filter out packageables that are also in the apk_under_test
    android_packageable_info = merge_android_packageable_info(ctx.actions, deps)

    resources_info = get_android_binary_resources_info(ctx, deps, android_packageable_info, use_proto_format = False, referenced_resources_lists = [])
    if resources_info.r_dot_java:
        java_packaging_deps += [create_java_packaging_dep(ctx, resources_info.r_dot_java.library_output.full_library)]

    # For instrumentation test APKs we always pre-dex, and we also always merge to a single dex.
    android_toolchain = ctx.attr._android_toolchain[AndroidToolchainInfo]
    pre_dexed_libs = [java_packaging_dep.dex for java_packaging_dep in java_packaging_deps]
    dex_files_info = merge_to_single_dex(ctx, android_toolchain, pre_dexed_libs)

    # TODO(T122203218) Filter out native libs that are also in the apk_under_test
    native_library_info = get_android_binary_native_library_info(ctx, android_packageable_info, deps_by_platform)

    output_apk = build_apk(
        actions = ctx.actions,
        android_toolchain = ctx.attr._android_toolchain[AndroidToolchainInfo],
        keystore = apk_under_test_info.keystore,
        dex_files_info = dex_files_info,
        native_library_info = native_library_info,
        resources_info = resources_info,
        java_packaging_deps = java_packaging_deps,
    )

    return [
        AndroidApkInfo(apk = output_apk, manifest = resources_info.manifest),
        AndroidInstrumentationApkInfo(apk_under_test = ctx.attr.apk[AndroidApkInfo].apk),
        DefaultInfo(default_outputs = [output_apk]),
    ]

def _verify_params(ctx: "context"):
    expect(ctx.attr.aapt_mode == "aapt2", "aapt1 is deprecated!")
    expect(ctx.attr.dex_tool == "d8", "dx is deprecated!")
