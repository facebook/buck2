# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//android:android_binary_resources_rules.bzl", "get_android_binary_resources_info")
load("@prelude//android:android_library.bzl", "build_android_library", "optional_jars")
load("@prelude//android:android_providers.bzl", "merge_android_packageable_info")
load("@prelude//android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@prelude//java:java_providers.bzl", "JavaLibraryInfo")
load("@prelude//java:java_test.bzl", "build_junit_test")
load("@prelude//java:java_toolchain.bzl", "JavaToolchainInfo")
load("@prelude//test:inject_test_run_info.bzl", "inject_test_run_info")
load("@prelude//utils:expect.bzl", "expect")

def robolectric_test_impl(ctx: AnalysisContext) -> list[Provider]:
    if ctx.attrs._build_only_native_code:
        return [DefaultInfo()]

    extra_cmds = []

    # Force robolectric to only use local dependency resolution.
    extra_cmds.append("-Drobolectric.offline=true")
    if ctx.attrs.robolectric_runtime_dependency:
        runtime_dependencies_dir = ctx.attrs.robolectric_runtime_dependency
    elif ctx.attrs.robolectric_runtime_dependencies:
        runtime_dependencies_dir = ctx.actions.symlinked_dir("runtime_dependencies", {
            runtime_dep.basename: runtime_dep
            for runtime_dep in ctx.attrs.robolectric_runtime_dependencies
        })
    else:
        runtime_dependencies_dir = None

    if runtime_dependencies_dir:
        extra_cmds.append(cmd_args(runtime_dependencies_dir, format = "-Drobolectric.dependency.dir={}"))

    all_packaging_deps = ctx.attrs.deps + ctx.attrs.exported_deps + ctx.attrs.runtime_deps
    android_packageable_info = merge_android_packageable_info(ctx.label, ctx.actions, all_packaging_deps)
    resources_info = get_android_binary_resources_info(
        ctx,
        all_packaging_deps,
        android_packageable_info,
        java_packaging_deps = [],  # Only used for third-party jar resources, which we don't care about here.
        use_proto_format = False,
        referenced_resources_lists = [],
        generate_strings_and_ids_separately = False,
        aapt2_preferred_density = ctx.attrs.preferred_density_for_binary_resources,
    )

    test_config_properties_file = ctx.actions.write(
        "test_config.properties",
        [
            cmd_args(["android_resource_apk", resources_info.primary_resources_apk], delimiter = "="),
            cmd_args(["android_merged_manifest", resources_info.manifest], delimiter = "="),
        ],
    )

    # Robolectric looks for a file named /com/android/tools/test_config.properties on the classpath
    test_config_symlinked_dir = ctx.actions.symlinked_dir("test_config_symlinked_dir", {"com/android/tools/test_config.properties": test_config_properties_file})
    test_config_properties_jar = ctx.actions.declare_output("test_config_properties.jar")
    jar_cmd = cmd_args([
        ctx.attrs._java_toolchain[JavaToolchainInfo].jar,
        "-cfM",  # -c: create new archive, -f: specify the file name, -M: do not create a manifest
        test_config_properties_jar.as_output(),
        "-C",
        test_config_symlinked_dir,
        ".",
    ])
    ctx.actions.run(jar_cmd, category = "test_config_properties_jar_cmd")
    extra_cmds.append(cmd_args(hidden = [resources_info.primary_resources_apk, resources_info.manifest]))

    r_dot_javas = [r_dot_java.library_info.library_output for r_dot_java in resources_info.r_dot_java_infos if r_dot_java.library_info.library_output]
    expect(len(r_dot_javas) <= 1, "android_library only works with single R.java")

    extra_sub_targets = {}
    if r_dot_javas:
        r_dot_java = r_dot_javas[0]
        extra_sub_targets["r_dot_java"] = [DefaultInfo(default_output = r_dot_java.full_library)]
    else:
        r_dot_java = None
    java_providers, _ = build_android_library(ctx, r_dot_java = r_dot_java, extra_sub_targets = extra_sub_targets)

    extra_classpath_entries = [test_config_properties_jar] + ctx.attrs._android_toolchain[AndroidToolchainInfo].android_bootclasspath + optional_jars(ctx)
    extra_classpath_entries.extend([r_dot_java.full_library for r_dot_java in r_dot_javas])
    external_runner_test_info = build_junit_test(
        ctx,
        java_providers.java_library_info,
        java_providers.java_packaging_info,
        java_providers.class_to_src_map,
        extra_cmds = extra_cmds,
        extra_classpath_entries = extra_classpath_entries,
    )

    providers = inject_test_run_info(ctx, external_runner_test_info) + [
        java_providers.java_library_intellij_info,
        java_providers.java_packaging_info,
        java_providers.template_placeholder_info,
        java_providers.default_info,
        java_providers.class_to_src_map,
        java_providers.java_global_code_info,
    ]

    if ctx.attrs.used_as_dependency_deprecated_do_not_use:
        providers.append(java_providers.java_library_info)
    else:
        java_library_without_compiling_deps = JavaLibraryInfo(
            compiling_deps = None,
            library_output = java_providers.java_library_info.library_output,
            output_for_classpath_macro = java_providers.java_library_info.output_for_classpath_macro,
        )
        providers.append(java_library_without_compiling_deps)

    return providers
