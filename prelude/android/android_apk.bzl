load("@fbcode//buck2/prelude/android:android_binary_native_library_rules.bzl", "get_android_binary_native_library_info")
load("@fbcode//buck2/prelude/android:android_binary_resources_rules.bzl", "get_android_binary_resources_info")
load("@fbcode//buck2/prelude/android:android_build_config.bzl", "generate_android_build_config", "get_build_config_fields")
load("@fbcode//buck2/prelude/android:android_providers.bzl", "AndroidApkInfo", "AndroidApkUnderTestInfo", "BuildConfigField", "CPU_FILTER_TO_ABI_DIRECTORY", "merge_android_packageable_info")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/android:configuration.bzl", "get_deps_by_platform")
load("@fbcode//buck2/prelude/android:dex_rules.bzl", "get_multi_dex", "get_single_primary_dex", "get_split_dex_merge_config", "merge_to_single_dex", "merge_to_split_dex")
load("@fbcode//buck2/prelude/android:preprocess_java_classes.bzl", "get_preprocessed_java_classes")
load("@fbcode//buck2/prelude/android:proguard.bzl", "get_proguard_output")
load("@fbcode//buck2/prelude/java:java_providers.bzl", "KeystoreInfo", "create_java_packaging_dep", "get_all_java_packaging_deps", "get_all_java_packaging_deps_from_packaging_infos")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

def android_apk_impl(ctx: "context") -> ["provider"]:
    sub_targets = {}

    _verify_params(ctx)

    cpu_filters = ctx.attrs.cpu_filters or CPU_FILTER_TO_ABI_DIRECTORY.keys()
    deps_by_platform = get_deps_by_platform(ctx)
    primary_platform = cpu_filters[0]
    deps = deps_by_platform[primary_platform]

    no_dx_target_labels = [no_dx_target.label.raw_target() for no_dx_target in ctx.attrs.no_dx]
    java_packaging_deps = [packaging_dep for packaging_dep in get_all_java_packaging_deps(ctx, deps) if packaging_dep.dex and packaging_dep.dex.dex.owner.raw_target() not in no_dx_target_labels]

    android_packageable_info = merge_android_packageable_info(ctx.label, ctx.actions, deps)
    build_config_infos = list(android_packageable_info.build_config_infos.traverse()) if android_packageable_info.build_config_infos else []

    build_config_libs = _get_build_config_java_libraries(ctx, build_config_infos)
    java_packaging_deps += get_all_java_packaging_deps_from_packaging_infos(ctx, build_config_libs)

    has_proguard_config = ctx.attrs.proguard_config != None or ctx.attrs.android_sdk_proguard_config == "default" or ctx.attrs.android_sdk_proguard_config == "optimized"
    should_pre_dex = not ctx.attrs.disable_pre_dex and not has_proguard_config and not ctx.attrs.preprocess_java_classes_bash

    referenced_resources_lists = [java_packaging_dep.dex.referenced_resources for java_packaging_dep in java_packaging_deps] if ctx.attrs.trim_resource_ids and should_pre_dex else []
    resources_info = get_android_binary_resources_info(ctx, deps, android_packageable_info, use_proto_format = False, referenced_resources_lists = referenced_resources_lists)
    if resources_info.r_dot_java:
        java_packaging_deps += [create_java_packaging_dep(ctx, resources_info.r_dot_java.library_output.full_library)]

    android_toolchain = ctx.attrs._android_toolchain[AndroidToolchainInfo]
    if should_pre_dex:
        pre_dexed_libs = [java_packaging_dep.dex for java_packaging_dep in java_packaging_deps]
        if ctx.attrs.use_split_dex:
            dex_files_info = merge_to_split_dex(
                ctx,
                android_toolchain,
                pre_dexed_libs,
                get_split_dex_merge_config(ctx, android_toolchain),
            )
        else:
            dex_files_info = merge_to_single_dex(ctx, android_toolchain, pre_dexed_libs)
    else:
        jars = [packaging_dep.jar for packaging_dep in java_packaging_deps]
        if ctx.attrs.preprocess_java_classes_bash:
            jars = get_preprocessed_java_classes(ctx, jars)
        if has_proguard_config:
            proguard_output = get_proguard_output(ctx, jars, java_packaging_deps, resources_info.proguard_config_file)
            jars = proguard_output.jars
            sub_targets["proguard_text_output"] = [
                DefaultInfo(
                    default_outputs = [ctx.actions.symlinked_dir(
                        "proguard_text_output",
                        {artifact.basename: artifact for artifact in proguard_output.proguard_artifacts},
                    )],
                ),
            ]
        else:
            proguard_output = None

        if ctx.attrs.use_split_dex:
            dex_files_info = get_multi_dex(
                ctx,
                ctx.attrs._android_toolchain[AndroidToolchainInfo],
                jars,
                ctx.attrs.primary_dex_patterns,
                proguard_output.proguard_configuration_output_file if proguard_output else None,
                proguard_output.proguard_mapping_output_file if proguard_output else None,
                is_optimized = has_proguard_config,
            )
        else:
            dex_files_info = get_single_primary_dex(
                ctx,
                ctx.attrs._android_toolchain[AndroidToolchainInfo],
                jars,
                is_optimized = has_proguard_config,
            )

    native_library_info = get_android_binary_native_library_info(ctx, android_packageable_info, deps_by_platform)
    unstripped_native_libs = native_library_info.unstripped_libs
    sub_targets["unstripped_native_libraries"] = [
        DefaultInfo(
            default_outputs = [ctx.actions.write("unstripped_native_libraries", unstripped_native_libs)],
            other_outputs = unstripped_native_libs,
        ),
    ]
    if resources_info.string_source_map:
        sub_targets["generate_string_resources"] = [DefaultInfo(default_outputs = [resources_info.string_source_map])]

    keystore = ctx.attrs.keystore[KeystoreInfo]
    output_apk = build_apk(
        actions = ctx.actions,
        android_toolchain = ctx.attrs._android_toolchain[AndroidToolchainInfo],
        keystore = keystore,
        dex_files_info = dex_files_info,
        native_library_info = native_library_info,
        resources_info = resources_info,
        java_packaging_deps = java_packaging_deps,
        compress_resources_dot_arsc = ctx.attrs.resource_compression == "enabled" or ctx.attrs.resource_compression == "enabled_with_strings_as_assets",
    )

    return [
        AndroidApkInfo(apk = output_apk, manifest = resources_info.manifest),
        AndroidApkUnderTestInfo(
            java_packaging_deps = java_packaging_deps,
            keystore = keystore,
            prebuilt_native_library_dirs = native_library_info.apk_under_test_prebuilt_native_library_dirs,
            platforms = deps_by_platform.keys(),
            primary_platform = primary_platform,
            resource_infos = resources_info.unfiltered_resource_infos,
            shared_libraries = native_library_info.apk_under_test_shared_libraries,
        ),
        DefaultInfo(default_outputs = [output_apk], sub_targets = sub_targets),
    ]

def build_apk(
        actions: "actions",
        keystore: KeystoreInfo.type,
        android_toolchain: AndroidToolchainInfo.type,
        dex_files_info: "DexFilesInfo",
        native_library_info: "AndroidBinaryNativeLibsInfo",
        resources_info: "AndroidBinaryResourcesInfo",
        java_packaging_deps: ["JavaPackagingDep"],
        compress_resources_dot_arsc: bool.type = False) -> "artifact":
    output_apk = actions.declare_output("output_apk.apk")

    apk_builder_args = cmd_args([
        android_toolchain.apk_builder[RunInfo],
        "--output-apk",
        output_apk.as_output(),
        "--resource-apk",
        resources_info.primary_resources_apk,
        "--dex-file",
        dex_files_info.primary_dex,
        "--keystore-path",
        keystore.store,
        "--keystore-properties-path",
        keystore.properties,
        "--zipalign_tool",
        android_toolchain.zipalign[RunInfo],
    ])

    if compress_resources_dot_arsc:
        apk_builder_args.add("--compress-resources-dot-arsc")

    asset_directories = native_library_info.native_lib_assets + dex_files_info.secondary_dex_dirs
    asset_directories_file = actions.write("asset_directories.txt", asset_directories)
    apk_builder_args.hidden(asset_directories)
    native_libs_for_apk = native_library_info.native_libs + native_library_info.native_libs_for_primary_apk
    native_library_directories = actions.write("native_library_directories", native_libs_for_apk)
    apk_builder_args.hidden(native_libs_for_apk)
    all_zip_files = [resources_info.packaged_string_assets] if resources_info.packaged_string_assets else []
    zip_files = actions.write("zip_files", all_zip_files)
    apk_builder_args.hidden(all_zip_files)
    prebuilt_jars = [packaging_dep.jar for packaging_dep in java_packaging_deps if packaging_dep.is_prebuilt_jar]
    jar_files_that_may_contain_resources = actions.write("jar_files_that_may_contain_resources", prebuilt_jars)
    apk_builder_args.hidden(prebuilt_jars)

    apk_builder_args.add([
        "--asset-directories-list",
        asset_directories_file,
        "--native-libraries-directories-list",
        native_library_directories,
        "--zip-files-list",
        zip_files,
        "--jar-files-that-may-contain-resources-list",
        jar_files_that_may_contain_resources,
    ])

    actions.run(apk_builder_args, category = "apk_build")

    return output_apk

def _get_build_config_java_libraries(ctx: "context", build_config_infos: ["AndroidBuildConfigInfo"]) -> ["JavaPackagingInfo"]:
    # BuildConfig deps should not be added for instrumented APKs because BuildConfig.class has
    # already been added to the APK under test.
    if ctx.attrs.package_type == "instrumented":
        return []

    build_config_constants = [
        BuildConfigField(type = "boolean", name = "DEBUG", value = str(ctx.attrs.package_type != "release").lower()),
        BuildConfigField(type = "boolean", name = "IS_EXOPACKAGE", value = str(len(ctx.attrs.exopackage_modes) > 0).lower()),
        # TODO(T104150125) add correct exopackage flags to BuildConfig
        BuildConfigField(type = "int", name = "EXOPACKAGE_FLAGS", value = "0"),
    ]

    default_build_config_fields = get_build_config_fields(ctx.attrs.build_config_values)

    java_libraries = []
    java_packages_seen = []
    for build_config_info in build_config_infos:
        java_package = build_config_info.package
        expect(java_package not in java_packages_seen, "Got the same java_package {} for different AndroidBuildConfigs".format(java_package))
        java_packages_seen.append(java_package)

        all_build_config_values = {}
        for build_config_field in build_config_info.build_config_fields + default_build_config_fields + build_config_constants:
            all_build_config_values[build_config_field.name] = build_config_field

        java_libraries.append(generate_android_build_config(
            ctx,
            java_package,
            java_package,
            True,  # use_constant_expressions
            all_build_config_values.values(),
            ctx.attrs.build_config_values_file[DefaultInfo].default_outputs[0] if type(ctx.attrs.build_config_values_file) == "dependency" else ctx.attrs.build_config_values_file,
        )[1])

    return java_libraries

def _verify_params(ctx: "context"):
    expect(ctx.attrs.aapt_mode == "aapt2", "aapt1 is deprecated!")
    expect(ctx.attrs.dex_tool == "d8", "dx is deprecated!")
    expect(ctx.attrs.allow_r_dot_java_in_secondary_dex == True)
