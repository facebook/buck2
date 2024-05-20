# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//android:android_apk.bzl", "build_apk")
load("@prelude//android:android_binary_native_library_rules.bzl", "get_android_binary_native_library_info")
load("@prelude//android:android_binary_resources_rules.bzl", "get_android_binary_resources_info")
load("@prelude//android:android_providers.bzl", "AndroidApkInfo", "AndroidApkUnderTestInfo", "AndroidInstrumentationApkInfo", "merge_android_packageable_info")
load("@prelude//android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@prelude//android:configuration.bzl", "get_deps_by_platform")
load("@prelude//android:dex_rules.bzl", "get_multi_dex", "get_single_primary_dex", "get_split_dex_merge_config", "merge_to_single_dex", "merge_to_split_dex")
load("@prelude//android:preprocess_java_classes.bzl", "get_preprocessed_java_classes")
load("@prelude//android:util.bzl", "create_enhancement_context")
load("@prelude//java:java_providers.bzl", "create_java_packaging_dep", "get_all_java_packaging_deps")
load("@prelude//java/utils:java_utils.bzl", "get_class_to_source_map_info")
load("@prelude//utils:expect.bzl", "expect")

def android_instrumentation_apk_impl(ctx: AnalysisContext):
    _verify_params(ctx)

    # jar preprocessing cannot be used when the jars were dexed already, so we have to disable predex when we want to preprocess the jars.
    disable_pre_dex = ctx.attrs.disable_pre_dex or ctx.attrs.preprocess_java_classes_bash

    apk_under_test_info = ctx.attrs.apk[AndroidApkUnderTestInfo]

    # android_instrumentation_apk uses the same platforms as the APK-under-test
    unfiltered_deps_by_platform = get_deps_by_platform(ctx)
    for platform in apk_under_test_info.platforms:
        expect(
            platform in unfiltered_deps_by_platform,
            "Android instrumentation APK must have any platforms that are in the APK-under-test!",
        )
    filtered_deps_by_platform = {platform: deps for platform, deps in unfiltered_deps_by_platform.items() if platform in apk_under_test_info.platforms}

    # We use the deps that don't have _build_only_native_code = True
    deps = unfiltered_deps_by_platform.values()[0]

    is_self_instrumenting = ctx.attrs.is_self_instrumenting

    java_packaging_deps = [
        packaging_dep
        for packaging_dep in get_all_java_packaging_deps(ctx, deps)
        if packaging_dep.dex and (is_self_instrumenting or not apk_under_test_info.java_packaging_deps.contains(packaging_dep.label.raw_target()))
    ]

    android_packageable_info = merge_android_packageable_info(ctx.label, ctx.actions, deps)

    resources_info = get_android_binary_resources_info(
        ctx,
        deps,
        android_packageable_info,
        java_packaging_deps = java_packaging_deps,
        use_proto_format = False,
        referenced_resources_lists = [],
        manifest_entries = apk_under_test_info.manifest_entries,
        resource_infos_to_exclude = apk_under_test_info.resource_infos if not is_self_instrumenting else None,
        r_dot_java_packages_to_exclude = apk_under_test_info.r_dot_java_packages.list() if not is_self_instrumenting else [],
    )
    android_toolchain = ctx.attrs._android_toolchain[AndroidToolchainInfo]
    java_packaging_deps += [
        create_java_packaging_dep(
            ctx,
            r_dot_java.library_info.library_output.full_library,
            dex_weight_factor = android_toolchain.r_dot_java_weight_factor,
        )
        for r_dot_java in resources_info.r_dot_java_infos
    ]

    enhance_ctx = create_enhancement_context(ctx)
    materialized_artifacts = []
    if not disable_pre_dex:
        pre_dexed_libs = [java_packaging_dep.dex for java_packaging_dep in java_packaging_deps]
        if ctx.attrs.use_split_dex:
            dex_merge_config = get_split_dex_merge_config(ctx, android_toolchain)
            dex_files_info = merge_to_split_dex(
                ctx,
                android_toolchain,
                pre_dexed_libs,
                dex_merge_config,
            )
        else:
            dex_files_info = merge_to_single_dex(ctx, android_toolchain, pre_dexed_libs)
    else:
        jars_to_owners = {packaging_dep.jar: packaging_dep.jar.owner.raw_target() for packaging_dep in java_packaging_deps}
        if ctx.attrs.preprocess_java_classes_bash:
            jars_to_owners, materialized_artifacts_dir = get_preprocessed_java_classes(enhance_ctx, jars_to_owners)
            if materialized_artifacts_dir:
                materialized_artifacts.append(materialized_artifacts_dir)
        if ctx.attrs.use_split_dex:
            dex_files_info = get_multi_dex(
                ctx,
                ctx.attrs._android_toolchain[AndroidToolchainInfo],
                jars_to_owners,
                ctx.attrs.primary_dex_patterns,
            )
        else:
            dex_files_info = get_single_primary_dex(
                ctx,
                ctx.attrs._android_toolchain[AndroidToolchainInfo],
                jars_to_owners.keys(),
            )

    native_library_info = get_android_binary_native_library_info(
        enhance_ctx,
        android_packageable_info,
        filtered_deps_by_platform,
        prebuilt_native_library_dirs_to_exclude = apk_under_test_info.prebuilt_native_library_dirs if not is_self_instrumenting else None,
        shared_libraries_to_exclude = apk_under_test_info.shared_libraries if not is_self_instrumenting else None,
    )

    output_apk = build_apk(
        label = ctx.label,
        actions = ctx.actions,
        android_toolchain = ctx.attrs._android_toolchain[AndroidToolchainInfo],
        keystore = apk_under_test_info.keystore,
        dex_files_info = dex_files_info,
        native_library_info = native_library_info,
        resources_info = resources_info,
    )

    class_to_srcs, _, _ = get_class_to_source_map_info(
        ctx,
        outputs = None,
        deps = deps,
    )

    return [
        AndroidApkInfo(apk = output_apk, materialized_artifacts = materialized_artifacts, manifest = resources_info.manifest),
        AndroidInstrumentationApkInfo(apk_under_test = ctx.attrs.apk[AndroidApkInfo].apk),
        DefaultInfo(default_output = output_apk, other_outputs = materialized_artifacts, sub_targets = enhance_ctx.get_sub_targets()),
        class_to_srcs,
    ]

def _verify_params(ctx: AnalysisContext):
    expect(ctx.attrs.aapt_mode == "aapt2", "aapt1 is deprecated!")
    expect(ctx.attrs.dex_tool == "d8", "dx is deprecated!")
