# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:validation_deps.bzl", "get_validation_deps_outputs")
load("@prelude//android:android_binary.bzl", "get_binary_info")
load("@prelude//android:android_providers.bzl", "AndroidAabInfo", "AndroidBinaryNativeLibsInfo", "AndroidBinaryResourcesInfo", "DexFilesInfo")
load("@prelude//android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@prelude//android:bundletool_util.bzl", "derive_universal_apk")
load("@prelude//java:java_providers.bzl", "KeystoreInfo")
load("@prelude//java/utils:java_more_utils.bzl", "get_path_separator_for_exec_os")

def android_bundle_impl(ctx: AnalysisContext) -> list[Provider]:
    android_binary_info = get_binary_info(ctx, use_proto_format = True)

    output_bundle = build_bundle(
        label = ctx.label,
        actions = ctx.actions,
        android_toolchain = ctx.attrs._android_toolchain[AndroidToolchainInfo],
        dex_files_info = android_binary_info.dex_files_info,
        native_library_info = android_binary_info.native_library_info,
        resources_info = android_binary_info.resources_info,
        bundle_config = ctx.attrs.bundle_config_file,
        validation_deps_outputs = get_validation_deps_outputs(ctx),
        packaging_options = ctx.attrs.packaging_options,
    )

    sub_targets = {}
    sub_targets.update(android_binary_info.sub_targets)
    if ctx.attrs.use_derived_apk:
        keystore = ctx.attrs.keystore[KeystoreInfo]
        default_output = derive_universal_apk(
            ctx,
            android_toolchain = ctx.attrs._android_toolchain[AndroidToolchainInfo],
            app_bundle = output_bundle,
            keystore = keystore,
        )
        sub_targets["aab"] = [DefaultInfo(
            default_outputs = [output_bundle],
        )]
    else:
        default_output = output_bundle

    java_packaging_deps = android_binary_info.java_packaging_deps
    return [
        DefaultInfo(default_output = default_output, other_outputs = android_binary_info.materialized_artifacts, sub_targets = sub_targets),
        AndroidAabInfo(aab = output_bundle, manifest = android_binary_info.resources_info.manifest, materialized_artifacts = android_binary_info.materialized_artifacts),
        TemplatePlaceholderInfo(
            keyed_variables = {
                "classpath": cmd_args([dep.jar for dep in java_packaging_deps if dep.jar], delimiter = get_path_separator_for_exec_os(ctx)),
                "classpath_including_targets_with_no_output": cmd_args([dep.output_for_classpath_macro for dep in java_packaging_deps], delimiter = get_path_separator_for_exec_os(ctx)),
            },
        ),
    ]

def build_bundle(
        label: Label,
        actions: AnalysisActions,
        android_toolchain: AndroidToolchainInfo,
        dex_files_info: DexFilesInfo,
        native_library_info: AndroidBinaryNativeLibsInfo,
        resources_info: AndroidBinaryResourcesInfo,
        bundle_config: Artifact | None,
        validation_deps_outputs: [list[Artifact], None] = None,
        packaging_options: dict | None = None) -> Artifact:
    output_bundle = actions.declare_output("{}.aab".format(label.name))

    bundle_builder_args = cmd_args([
        android_toolchain.bundle_builder[RunInfo],
        "--output-bundle",
        output_bundle.as_output(),
        "--resource-apk",
        resources_info.primary_resources_apk,
        "--dex-file",
        dex_files_info.primary_dex,
    ])

    # The outputs of validation_deps need to be added as hidden arguments
    # to an action for the validation_deps targets to be built and enforced.
    if validation_deps_outputs:
        bundle_builder_args.hidden(validation_deps_outputs)

    if bundle_config:
        bundle_builder_args.add(["--path-to-bundle-config-file", bundle_config])

    if android_toolchain.package_meta_inf_version_files:
        bundle_builder_args.add("--package-meta-inf-version-files")

    root_module_asset_directories = native_library_info.root_module_native_lib_assets + dex_files_info.root_module_secondary_dex_dirs
    root_module_asset_directories_file = actions.write("root_module_asset_directories.txt", root_module_asset_directories)
    bundle_builder_args.hidden(root_module_asset_directories)

    non_root_module_asset_directories = resources_info.module_manifests + dex_files_info.non_root_module_secondary_dex_dirs
    non_root_module_asset_directories_file = actions.write("non_root_module_asset_directories.txt", non_root_module_asset_directories)
    bundle_builder_args.hidden(non_root_module_asset_directories)
    non_root_module_asset_native_lib_directories = actions.write("non_root_module_asset_native_lib_directories.txt", native_library_info.non_root_module_native_lib_assets)
    bundle_builder_args.hidden(native_library_info.non_root_module_native_lib_assets)

    native_library_directories = actions.write("native_library_directories", native_library_info.native_libs_for_primary_apk)
    bundle_builder_args.hidden(native_library_info.native_libs_for_primary_apk)
    all_zip_files = [resources_info.packaged_string_assets] if resources_info.packaged_string_assets else []
    zip_files = actions.write("zip_files", all_zip_files)
    bundle_builder_args.hidden(all_zip_files)
    jar_files_that_may_contain_resources = actions.write("jar_files_that_may_contain_resources", resources_info.jar_files_that_may_contain_resources)
    bundle_builder_args.hidden(resources_info.jar_files_that_may_contain_resources)

    if resources_info.module_assets:
        bundle_builder_args.add(["--module-assets-dir", resources_info.module_assets])

    bundle_builder_args.add([
        "--root-module-asset-directories-list",
        root_module_asset_directories_file,
        "--non-root-module-asset-directories-list",
        non_root_module_asset_directories_file,
        "--non-root-module-asset-native-lib-directories-list",
        non_root_module_asset_native_lib_directories,
        "--native-libraries-directories-list",
        native_library_directories,
        "--zip-files-list",
        zip_files,
        "--jar-files-that-may-contain-resources-list",
        jar_files_that_may_contain_resources,
        "--zipalign_tool",
        android_toolchain.zipalign[RunInfo],
    ])

    if packaging_options:
        for key, value in packaging_options.items():
            if key != "excluded_resources":
                fail("Only 'excluded_resources' is supported in packaging_options right now!")
            else:
                bundle_builder_args.add("--excluded-resources", actions.write("excluded_resources.txt", value))

    actions.run(bundle_builder_args, category = "bundle_build")

    return output_bundle
