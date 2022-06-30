load("@fbcode//buck2/prelude/utils:utils.bzl", "filter_and_map_idx")

Aapt2LinkInfo = provider(
    fields = [
        # "APK" containing resources to be used by the Android binary
        "primary_resources_apk",  # artifact
        # proguard config needed to retain used resources
        "proguard_config_file",  # artifact
        # R.txt containing all the linked resources
        "r_dot_txt",  # artifact
    ],
)

AndroidBinaryNativeLibsInfo = provider(
    fields = [
        "native_libs",  # ["artifact"]
        "native_lib_assets",  # ["artifact"]
        "native_libs_for_system_library_loader",  # ["artifact"]
        "unstripped_libs",  # ["artifact"]
    ],
)

AndroidBinaryResourcesInfo = provider(
    fields = [
        # manifest to be used by the APK
        "manifest",  # artifact
        # "APK" containing resources to be used by the Android binary
        "primary_resources_apk",  # artifact
        # proguard config needed to retain used resources
        "proguard_config_file",  # artifact
        # R.txt containing all the linked resources
        "r_dot_java",  # ["JavaLibraryInfo", None]
        # directory containing filtered string resources files
        "string_source_map",  # ["artifact", None]
        # The resource infos that are used in this APK
        "unfiltered_resource_infos",  # ["AndroidResourceInfo"]
    ],
)

# Information about an `android_build_config`
BuildConfigField = record(
    type = str.type,
    name = str.type,
    value = str.type,
)

AndroidBuildConfigInfo = provider(
    fields = [
        "package",  # str.type
        "build_config_fields",  # ["BuildConfigField"]
    ],
)

# Information about an `android_manifest`
AndroidManifestInfo = provider(
    fields = [
        "manifest",  # artifact
        "merge_report",  # artifact
    ],
)

AndroidApkInfo = provider(
    fields = [
        "apk",
        "manifest",
    ],
)

AndroidApkUnderTestInfo = provider(
    fields = [
        "java_packaging_deps",  # ["JavaPackagingDep"]
        "keystore",  # "KeystoreInfo"
        "platforms",  # [str.type]
        "primary_platform",  # str.type
        "resource_infos",  # ["ResourceInfos"]
    ],
)

AndroidInstrumentationApkInfo = provider(
    fields = [
        "apk_under_test",  # "artifact"
    ],
)

CPU_FILTER_TO_ABI_DIRECTORY = {
    "arm64": "arm64-v8a",
    "armv7": "armeabi-v7a",
    "x86": "x86",
    "x86_64": "x86_64",
}

NativeLibraryFromPrebuiltAar = record(
    sub_dirs = {str.type: "artifact"},  # map of cpu-type to ABI directory artifact.
    use_system_library_loader = bool.type,
)

def _artifacts(args: "cmd_args", value: "artifact"):
    args.add(value)

AndroidBuildConfigInfoTSet = transitive_set()
ManifestTSet = transitive_set(args_projections = {"artifacts": _artifacts})
NativeLibraryFromPrebuiltAarTSet = transitive_set()
ResourceInfoTSet = transitive_set()

AndroidPackageableInfo = provider(
    fields = [
        "build_config_infos",  # ["AndroidBuildConfigInfoTSet", None]
        "manifests",  # ["ManifestTSet", None]
        "native_libs_from_prebuilt_aars",  # ["NativeLibraryFromPrebuiltAarTSet", None]
        "resource_infos",  # ["AndroidResourceInfoTSet", None]
    ],
)

# Information about an `android_resource`
AndroidResourceInfo = provider(
    fields = [
        # output of running `aapt2_compile` on the resources, if resources are present
        "aapt2_compile_output",  # ["artifact", None]
        #  if False, then the "res" are not affected by the strings-as-assets resource filter
        "allow_strings_as_assets_resource_filtering",  # bool.type
        # assets defined by this rule. May be empty
        "assets",  # ["artifact", None]
        # manifest file used by the resources, if resources are present
        "manifest_file",  # ["artifact", None]
        # package used for R.java, if resources are present
        "r_dot_java_package",  # ["artifact", None]
        # resources defined by this rule. May be empty
        "res",  # ["artifact", None]
        # symbols defined by the resources, if resources are present
        "text_symbols",  # ["artifact", None]
    ],
)

# `AndroidResourceInfos` that are exposed via `exported_deps`
ExportedAndroidResourceInfo = provider(
    fields = [
        "resource_infos",  # ["AndroidResourceInfo"]
    ],
)

DexFilesInfo = provider(
    fields = [
        "primary_dex",  # "artifact"
        "secondary_dex_dirs",  # ["artifact"],
        "proguard_text_files_path",  # ["artifact", None]
    ],
)

def merge_android_packageable_info(
        actions: "actions",
        deps: ["dependency"],
        build_config_info: ["AndroidBuildConfigInfo", None] = None,
        manifest: ["artifact", None] = None,
        native_lib_from_prebuilt_aar: [NativeLibraryFromPrebuiltAar.type, None] = None,
        resource_info: ["AndroidResourceInfo", None] = None) -> "AndroidPackageableInfo":
    android_packageable_deps = filter_and_map_idx(AndroidPackageableInfo, deps)

    build_config_infos = _get_transitive_set(
        actions,
        filter(None, [dep.build_config_infos for dep in android_packageable_deps]),
        build_config_info,
        AndroidBuildConfigInfoTSet,
    )

    manifests = _get_transitive_set(
        actions,
        filter(None, [dep.manifests for dep in android_packageable_deps]),
        manifest,
        ManifestTSet,
    )

    native_libs_from_prebuilt_aars = _get_transitive_set(
        actions,
        filter(None, [dep.native_libs_from_prebuilt_aars for dep in android_packageable_deps]),
        native_lib_from_prebuilt_aar,
        NativeLibraryFromPrebuiltAarTSet,
    )

    resource_infos = _get_transitive_set(
        actions,
        filter(None, [dep.resource_infos for dep in android_packageable_deps]),
        resource_info,
        ResourceInfoTSet,
    )

    return AndroidPackageableInfo(
        build_config_infos = build_config_infos,
        manifests = manifests,
        native_libs_from_prebuilt_aars = native_libs_from_prebuilt_aars,
        resource_infos = resource_infos,
    )

def _get_transitive_set(
        actions: "actions",
        children: ["transitive_set"],
        node: "_a",
        transitive_set_definition: "transitive_set_definition") -> ["transitive_set", None]:
    kwargs = {}
    if children:
        kwargs["children"] = children
    if node:
        kwargs["value"] = node

    return actions.tset(transitive_set_definition, **kwargs) if kwargs else None

def merge_exported_android_resource_info(
        exported_deps: ["dependency"]) -> "ExportedAndroidResourceInfo":
    exported_android_resource_infos = []
    for exported_dep in exported_deps:
        exported_resource_info = exported_dep[ExportedAndroidResourceInfo]
        if exported_resource_info:
            exported_android_resource_infos += exported_resource_info.resource_infos

        android_resource = exported_dep[AndroidResourceInfo]
        if android_resource:
            exported_android_resource_infos.append(android_resource)

    return ExportedAndroidResourceInfo(resource_infos = dedupe(exported_android_resource_infos))
