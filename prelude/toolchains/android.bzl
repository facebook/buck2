# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//android:android_toolchain.bzl", "AndroidPlatformInfo", "AndroidToolchainInfo")

def _android_sdk_tools_impl(ctx):
    sub_targets = {}

    sub_targets["aapt2"] = [RunInfo(args = ["{}/build-tools/{}/aapt2".format(ctx.attrs.android_sdk_path, ctx.attrs.build_tools_version)])]
    sub_targets["aidl"] = [RunInfo(args = ["{}/build-tools/{}/aidl".format(ctx.attrs.android_sdk_path, ctx.attrs.build_tools_version)])]
    sub_targets["zipalign"] = [RunInfo(args = ["{}/build-tools/{}/zipalign".format(ctx.attrs.android_sdk_path, ctx.attrs.build_tools_version)])]

    sub_targets["adb"] = [RunInfo(args = ["{}/build-tools/platform-tools/adb".format(ctx.attrs.android_sdk_path)])]

    android_jar = ctx.actions.declare_output("android.jar")
    ctx.actions.run(cmd_args(["ln", "-s", "{}/platforms/{}/android.jar".format(ctx.attrs.android_sdk_path, ctx.attrs.compile_sdk_version), android_jar.as_output()]), category = "android_jar_symlink")
    sub_targets["android.jar"] = [DefaultInfo(default_output = android_jar)]

    framework_aidl_file = ctx.actions.declare_output("framework.aidl")
    ctx.actions.run(cmd_args(["ln", "-s", "{}/platforms/{}/framework.aidl".format(ctx.attrs.android_sdk_path, ctx.attrs.compile_sdk_version), framework_aidl_file.as_output()]), category = "framework_aidl_symlink")
    sub_targets["framework.aidl"] = [DefaultInfo(default_output = framework_aidl_file)]

    optimized_proguard_config = ctx.actions.declare_output("proguard-android-optimize.txt")
    ctx.actions.run(cmd_args(["ln", "-s", "{}/platforms/tools/proguard/proguard-android-optimize.txt".format(ctx.attrs.android_sdk_path), optimized_proguard_config.as_output()]), category = "optimized_proguard_config_symlink")
    sub_targets["optimized_proguard_config"] = [DefaultInfo(default_output = optimized_proguard_config)]

    proguard_config = ctx.actions.declare_output("proguard-android.txt")
    ctx.actions.run(cmd_args(["ln", "-s", "{}/platforms/tools/proguard/proguard-android.txt".format(ctx.attrs.android_sdk_path), proguard_config.as_output()]), category = "proguard_config_symlink")
    sub_targets["proguard_config"] = [DefaultInfo(default_output = proguard_config)]

    proguard_jar = ctx.actions.declare_output("proguard.jar")
    ctx.actions.run(cmd_args(["ln", "-s", "{}/platforms/tools/proguard/lib/proguard.jar".format(ctx.attrs.android_sdk_path), proguard_jar.as_output()]), category = "proguard_jar_symlink")
    sub_targets["proguard.jar"] = [DefaultInfo(default_output = proguard_jar)]

    return [
        DefaultInfo(sub_targets = sub_targets),
    ]

android_sdk_tools = rule(
    impl = _android_sdk_tools_impl,
    attrs = {
        "android_sdk_path": attrs.string(default = "/opt/android/sdk_D37111790"),
        "build_tools_version": attrs.string(default = "35.0.0"),
        "compile_sdk_version": attrs.string(default = "android-35"),
    },
)

def system_android_toolchain(
        name,
        **kwargs):
    android_sdk_tools_name = "{}_android_sdk_tools".format(name)
    android_sdk_tools(
        name = android_sdk_tools_name,
    )

    android_sdk_tools_target = ":{}".format(android_sdk_tools_name)

    kwargs["aapt2_filter_resources"] = "prelude//android/tools:filter_extra_resources"
    kwargs["aapt2"] = "{}[aapt2]".format(android_sdk_tools_target)
    kwargs["aar_builder"] = "prelude//toolchains/android/src/com/facebook/buck/android/aar:aar_builder_binary"
    kwargs["adb"] = "{}[adb]".format(android_sdk_tools_target)
    kwargs["aidl"] = "{}[aidl]".format(android_sdk_tools_target)
    kwargs["android_jar"] = "{}[android.jar]".format(android_sdk_tools_target)
    kwargs["android_optional_jars"] = []
    kwargs["apk_builder"] = "prelude//toolchains/android/src/com/facebook/buck/android/apk:apk_builder_binary"
    kwargs["apk_module_graph"] = "prelude//toolchains/android/src/com/facebook/buck/android/apkmodule:apkmodule_binary"
    kwargs["app_without_resources_stub"] = "prelude//android/tools:app_without_resources_stub"
    kwargs["bundle_apks_builder"] = "prelude//toolchains/android/src/com/facebook/buck/android/bundle:bundle_apks_builder_binary"
    kwargs["bundle_builder"] = "prelude//toolchains/android/src/com/facebook/buck/android/bundle:bundle_builder_binary"
    kwargs["combine_native_library_dirs"] = "prelude//android/tools:combine_native_library_dirs"
    kwargs["copy_string_resources"] = "prelude//toolchains/android/src/com/facebook/buck/android/resources/strings:copy_string_resources_binary"
    kwargs["cross_module_native_deps_check"] = True
    kwargs["d8_command"] = "prelude//toolchains/android/src/com/facebook/buck/android/dex:run_d8_binary"
    kwargs["duplicate_class_checker"] = "prelude//android/tools:duplicate_class_checker"
    kwargs["exo_resources_rewriter"] = "prelude//toolchains/android/src/com/facebook/buck/android/resources:exo_resources_rewriter_binary"

    # TODO: We should support exopackage in OSS builds.
    kwargs["exopackage_agent_apk"] = None
    kwargs["filter_dex_class_names"] = "prelude//android/tools:filter_dex"
    kwargs["filter_prebuilt_native_library_dir"] = "prelude//android/tools:filter_prebuilt_native_library_dir"
    kwargs["filter_resources"] = "prelude//toolchains/android/src/com/facebook/buck/android/resources/filter:filter_resources_binary"
    kwargs["framework_aidl_file"] = "{}[framework.aidl]".format(android_sdk_tools_target)
    kwargs["generate_build_config"] = "prelude//toolchains/android/src/com/facebook/buck/android/build_config:generate_build_config_binary"
    kwargs["generate_manifest"] = "prelude//toolchains/android/src/com/facebook/buck/android/manifest:generate_manifest_binary"
    kwargs["installer"] = "prelude//toolchains/android/src/com/facebook/buck/installer/android:android_installer"
    kwargs["instrumentation_test_can_run_locally"] = True
    kwargs["instrumentation_test_runner_classpath"] = [
        "prelude//toolchains/android/src/com/facebook/buck/testrunner:testrunner-bin-fixed",
        "prelude//toolchains/android/third-party:android-common",
        "prelude//toolchains/android/third-party:ddmlib",
        "prelude//toolchains/android/third-party:guava-jar",
        "prelude//toolchains/android/third-party:failureaccess",
        "prelude//toolchains/android/third-party:listenablefuture",
        "prelude//toolchains/android/third-party:kxml2",
        "prelude//toolchains/android/third-party:protobuf",
        "prelude//toolchains/android/third-party:jackson-annotations",
        "prelude//toolchains/android/third-party:jackson-core",
        "prelude//toolchains/android/third-party:jackson-databind-jar",
    ]
    kwargs["instrumentation_test_runner_main_class"] = "com.facebook.buck.testrunner.InstrumentationMain"
    kwargs["jar_splitter_command"] = "prelude//toolchains/android/src/com/facebook/buck/android/dex:jar_splitter_binary"
    kwargs["list_tests_command"] = None
    kwargs["manifest_utils"] = "prelude//toolchains/android/src/com/facebook/buck/android:manifest_utils_binary"
    kwargs["merge_android_resource_sources"] = "prelude//toolchains/android/src/com/facebook/buck/android/aapt:merge_android_resource_sources_binary"
    kwargs["merge_android_resources"] = "prelude//toolchains/android/src/com/facebook/buck/android/resources:merge_android_resources_binary"
    kwargs["merge_assets"] = "prelude//toolchains/android/src/com/facebook/buck/android/resources:merge_assets_binary"
    kwargs["mergemap_tool"] = "prelude//android/tools:compute_merge_sequence"
    kwargs["mini_aapt"] = "prelude//toolchains/android/src/com/facebook/buck/android/aapt:mini_aapt_binary"
    kwargs["multi_dex_command"] = "prelude//toolchains/android/src/com/facebook/buck/android/dex:multi_dex_binary"
    kwargs["native_libs_as_assets_metadata"] = "prelude//android/tools:native_libs_as_assets_metadata"
    kwargs["optimized_proguard_config"] = "{}[optimized_proguard_config]".format(android_sdk_tools_target)
    kwargs["package_meta_inf_version_files"] = False
    kwargs["package_strings_as_assets"] = "prelude//toolchains/android/src/com/facebook/buck/android/resources/strings:package_strings_as_assets_binary"
    kwargs["proguard_config"] = "{}[proguard_config]".format(android_sdk_tools_target)
    kwargs["proguard_jar"] = "{}[proguard.jar]".format(android_sdk_tools_target)
    kwargs["r_dot_java_weight_factor"] = 8
    kwargs["replace_application_id_placeholders"] = "prelude//toolchains/android/src/com/facebook/buck/android/manifest:replace_application_id_placeholders_binary"
    kwargs["secondary_dex_compression_command"] = "prelude//toolchains/android/src/com/facebook/buck/android/dex:secondary_dex_compression_binary"
    kwargs["secondary_dex_weight_limit"] = 1024
    kwargs["set_application_id_to_specified_package"] = True
    kwargs["should_run_sanity_check_for_placeholders"] = True
    kwargs["unpack_aar"] = "prelude//android/tools:unpack_aar"
    kwargs["zipalign"] = "{}[zipalign]".format(android_sdk_tools_target)

    system_android_toolchain_rule(
        name = name,
        **kwargs
    )

def system_android_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        AndroidPlatformInfo(
            name = ctx.attrs.name,
        ),
        AndroidToolchainInfo(
            aapt2 = ctx.attrs.aapt2[RunInfo],
            aapt2_filter_resources = ctx.attrs.aapt2_filter_resources[RunInfo],
            aar_builder = ctx.attrs.aar_builder[RunInfo],
            adb = ctx.attrs.adb[RunInfo],
            aidl = ctx.attrs.aidl[RunInfo],
            android_bootclasspath = [ctx.attrs.android_jar],
            android_jar = ctx.attrs.android_jar,
            android_optional_jars = ctx.attrs.android_optional_jars,
            apk_builder = ctx.attrs.apk_builder,
            apk_module_graph = ctx.attrs.apk_module_graph,
            app_without_resources_stub = ctx.attrs.app_without_resources_stub,
            bundle_apks_builder = ctx.attrs.bundle_apks_builder,
            bundle_builder = ctx.attrs.bundle_builder,
            combine_native_library_dirs = ctx.attrs.combine_native_library_dirs,
            copy_string_resources = ctx.attrs.copy_string_resources,
            cross_module_native_deps_check = ctx.attrs.cross_module_native_deps_check,
            d8_command = ctx.attrs.d8_command,
            duplicate_class_checker = ctx.attrs.duplicate_class_checker,
            exo_resources_rewriter = ctx.attrs.exo_resources_rewriter,
            exopackage_agent_apk = ctx.attrs.exopackage_agent_apk,
            filter_dex_class_names = ctx.attrs.filter_dex_class_names,
            filter_prebuilt_native_library_dir = ctx.attrs.filter_prebuilt_native_library_dir,
            filter_resources = ctx.attrs.filter_resources,
            framework_aidl_file = ctx.attrs.framework_aidl_file,
            generate_build_config = ctx.attrs.generate_build_config,
            generate_manifest = ctx.attrs.generate_manifest,
            installer = ctx.attrs.installer,
            instrumentation_test_can_run_locally = ctx.attrs.instrumentation_test_can_run_locally,
            instrumentation_test_runner_classpath = ctx.attrs.instrumentation_test_runner_classpath,
            instrumentation_test_runner_main_class = ctx.attrs.instrumentation_test_runner_main_class,
            jar_splitter_command = ctx.attrs.jar_splitter_command,
            list_tests_command = ctx.attrs.list_tests_command,
            manifest_utils = ctx.attrs.manifest_utils,
            merge_android_resource_sources = ctx.attrs.merge_android_resource_sources,
            merge_android_resources = ctx.attrs.merge_android_resources,
            merge_assets = ctx.attrs.merge_assets,
            mergemap_tool = ctx.attrs.mergemap_tool[RunInfo],
            mini_aapt = ctx.attrs.mini_aapt,
            multi_dex_command = ctx.attrs.multi_dex_command,
            native_libs_as_assets_metadata = ctx.attrs.native_libs_as_assets_metadata,
            optimized_proguard_config = ctx.attrs.optimized_proguard_config,
            p7zip = None,
            package_meta_inf_version_files = ctx.attrs.package_meta_inf_version_files,
            package_strings_as_assets = ctx.attrs.package_strings_as_assets,
            proguard_config = ctx.attrs.proguard_config,
            proguard_jar = ctx.attrs.proguard_jar,
            r_dot_java_weight_factor = ctx.attrs.r_dot_java_weight_factor,
            replace_application_id_placeholders = ctx.attrs.replace_application_id_placeholders,
            secondary_dex_compression_command = ctx.attrs.secondary_dex_compression_command,
            secondary_dex_weight_limit = ctx.attrs.secondary_dex_weight_limit,
            set_application_id_to_specified_package = ctx.attrs.set_application_id_to_specified_package,
            should_run_sanity_check_for_placeholders = ctx.attrs.should_run_sanity_check_for_placeholders,
            unpack_aar = ctx.attrs.unpack_aar,
            zipalign = ctx.attrs.zipalign,
        ),
    ]

system_android_toolchain_rule = rule(
    attrs = {
        "aapt2": attrs.dep(providers = [RunInfo]),
        "aapt2_filter_resources": attrs.dep(providers = [RunInfo]),
        "aar_builder": attrs.dep(providers = [RunInfo]),
        "adb": attrs.dep(providers = [RunInfo]),
        "aidl": attrs.dep(providers = [RunInfo]),
        "android_jar": attrs.source(),
        "android_optional_jars": attrs.list(attrs.source()),
        "apk_builder": attrs.dep(providers = [RunInfo]),
        "apk_module_graph": attrs.dep(providers = [RunInfo]),
        "app_without_resources_stub": attrs.source(),
        "bundle_apks_builder": attrs.dep(providers = [RunInfo]),
        "bundle_builder": attrs.dep(providers = [RunInfo]),
        "combine_native_library_dirs": attrs.dep(providers = [RunInfo]),
        "copy_string_resources": attrs.dep(providers = [RunInfo]),
        "cross_module_native_deps_check": attrs.bool(),
        "d8_command": attrs.dep(providers = [RunInfo]),
        "duplicate_class_checker": attrs.option(attrs.dep(providers = [RunInfo]), default = None),
        "exo_resources_rewriter": attrs.dep(providers = [RunInfo]),
        "exopackage_agent_apk": attrs.option(attrs.source(), default = None),
        "filter_dex_class_names": attrs.dep(providers = [RunInfo]),
        "filter_prebuilt_native_library_dir": attrs.dep(providers = [RunInfo]),
        "filter_resources": attrs.dep(providers = [RunInfo]),
        "framework_aidl_file": attrs.source(),
        "generate_build_config": attrs.dep(providers = [RunInfo]),
        "generate_manifest": attrs.dep(providers = [RunInfo]),
        "installer": attrs.label(),
        "instrumentation_test_can_run_locally": attrs.bool(),
        "instrumentation_test_runner_classpath": attrs.list(attrs.source()),
        "instrumentation_test_runner_main_class": attrs.string(),
        "jar_splitter_command": attrs.dep(providers = [RunInfo]),
        "list_tests_command": attrs.option(attrs.dep(providers = [RunInfo]), default = None),
        "manifest_utils": attrs.dep(providers = [RunInfo]),
        "merge_android_resource_sources": attrs.dep(providers = [RunInfo]),
        "merge_android_resources": attrs.dep(providers = [RunInfo]),
        "merge_assets": attrs.dep(providers = [RunInfo]),
        "mergemap_tool": attrs.dep(providers = [RunInfo]),
        "mini_aapt": attrs.dep(providers = [RunInfo]),
        "multi_dex_command": attrs.dep(providers = [RunInfo]),
        "native_libs_as_assets_metadata": attrs.dep(providers = [RunInfo]),
        "optimized_proguard_config": attrs.source(),
        "package_meta_inf_version_files": attrs.bool(),
        "package_strings_as_assets": attrs.dep(providers = [RunInfo]),
        "proguard_config": attrs.source(),
        "proguard_jar": attrs.source(),
        "r_dot_java_weight_factor": attrs.int(),
        "replace_application_id_placeholders": attrs.dep(providers = [RunInfo]),
        "secondary_dex_compression_command": attrs.dep(providers = [RunInfo]),
        "secondary_dex_weight_limit": attrs.int(),
        "set_application_id_to_specified_package": attrs.bool(),
        "should_run_sanity_check_for_placeholders": attrs.bool(),
        "unpack_aar": attrs.dep(providers = [RunInfo]),
        "zipalign": attrs.dep(providers = [RunInfo]),
    },
    impl = system_android_toolchain_rule_impl,
    is_toolchain_rule = True,
)
