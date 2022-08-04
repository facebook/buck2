load("@fbcode//buck2/prelude:paths.bzl", "paths")
load("@fbcode//buck2/prelude/android:android_providers.bzl", "AndroidBinaryNativeLibsInfo", "CPU_FILTER_TO_ABI_DIRECTORY")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/linking:shared_libraries.bzl", "SharedLibraryInfo", "merge_shared_libraries", "traverse_shared_library_info")
load("@fbcode//buck2/prelude/utils:utils.bzl", "filter_and_map_idx")

def get_android_binary_native_library_info(
        ctx: "context",
        android_packageable_info: "AndroidPackageableInfo",
        deps_by_platform: {str.type: ["dependency"]},
        prebuilt_native_library_dirs_to_exclude: ["PrebuiltNativeLibraryDir"] = [],
        shared_libraries_to_exclude: ["SharedLibrary"] = []) -> AndroidBinaryNativeLibsInfo.type:
    traversed_prebuilt_native_library_dirs = android_packageable_info.prebuilt_native_library_dirs.traverse() if android_packageable_info.prebuilt_native_library_dirs else []
    all_prebuilt_native_library_dirs = [native_lib for native_lib in traversed_prebuilt_native_library_dirs if native_lib not in prebuilt_native_library_dirs_to_exclude]

    prebuilt_native_library_dirs, prebuilt_native_library_dirs_for_primary_apk, prebuilt_native_library_dir_assets = [], [], []
    for native_lib in all_prebuilt_native_library_dirs:
        if native_lib.for_primary_apk:
            prebuilt_native_library_dirs_for_primary_apk.append(native_lib)
        elif native_lib.is_asset:
            prebuilt_native_library_dir_assets.append(native_lib)
        else:
            prebuilt_native_library_dirs.append(native_lib)

    native_libs = _filter_prebuilt_native_library_dir(ctx, prebuilt_native_library_dirs, "native_libs")
    native_libs_for_primary_apk = _filter_prebuilt_native_library_dir(
        ctx,
        prebuilt_native_library_dirs_for_primary_apk,
        "native_libs_for_primary_apk",
    )
    native_lib_assets = _filter_prebuilt_native_library_dir(
        ctx,
        prebuilt_native_library_dir_assets,
        "native_lib_assets",
    )

    unstripped_libs = []
    all_shared_libraries = []
    for platform, deps in deps_by_platform.items():
        shared_library_info = merge_shared_libraries(
            ctx.actions,
            deps = filter_and_map_idx(SharedLibraryInfo, deps),
        )
        native_linkables = {so_name: shared_lib for so_name, shared_lib in traverse_shared_library_info(shared_library_info).items() if shared_lib not in shared_libraries_to_exclude}
        all_shared_libraries.extend(native_linkables.values())
        unstripped_libs += [shared_lib.lib.output for shared_lib in native_linkables.values()]

        platform_stripped_native_linkables, platform_stripped_native_linkable_assets = _get_native_linkables(ctx, platform, native_linkables)
        native_libs += platform_stripped_native_linkables
        native_lib_assets += platform_stripped_native_linkable_assets

    return AndroidBinaryNativeLibsInfo(
        apk_under_test_prebuilt_native_library_dirs = all_prebuilt_native_library_dirs,
        apk_under_test_shared_libraries = all_shared_libraries,
        native_libs = native_libs,
        native_libs_for_primary_apk = native_libs_for_primary_apk,
        unstripped_libs = unstripped_libs,
        native_lib_assets = native_lib_assets,
    )

def _filter_prebuilt_native_library_dir(
        ctx: "context",
        native_libs: ["PrebuiltNativeLibraryDir"],
        identifier: str.type) -> ["artifact"]:
    cpu_filters = ctx.attrs.cpu_filters or CPU_FILTER_TO_ABI_DIRECTORY.keys()
    abis = [CPU_FILTER_TO_ABI_DIRECTORY[cpu] for cpu in cpu_filters]
    filter_tool = ctx.attrs._android_toolchain[AndroidToolchainInfo].filter_prebuilt_native_library_dir[RunInfo]
    libs = []
    for i, native_lib in enumerate(reversed(native_libs)):
        lib_identifier = "{}-{}".format(identifier, i)
        output_dir = ctx.actions.declare_output(lib_identifier)
        ctx.actions.run(
            [filter_tool, native_lib.dir, output_dir.as_output(), "--abis"] + abis,
            category = "filter_prebuilt_native_library_dir",
            identifier = lib_identifier,
        )
        libs.append(output_dir)

    return libs

def _get_native_linkables(
        ctx: "context",
        platform: str.type,
        native_linkables: {str.type: "SharedLibrary"}) -> (["artifact"], ["artifact"]):
    cpu_filters = ctx.attrs.cpu_filters
    if cpu_filters and platform not in cpu_filters:
        return [], []

    stripped_native_linkables = []
    stripped_native_linkable_assets = []
    abi_directory = CPU_FILTER_TO_ABI_DIRECTORY[platform]
    for so_name, native_linkable in native_linkables.items():
        srcs = {paths.join(abi_directory, so_name): native_linkable.stripped_lib}
        symlinked_dir = ctx.actions.symlinked_dir("native_linkable_dir_{}_{}".format(
            platform,
            so_name.replace(".", "_"),
        ), srcs)

        if native_linkable.can_be_asset:
            stripped_native_linkable_assets.append(symlinked_dir)
        else:
            stripped_native_linkables.append(symlinked_dir)

    return stripped_native_linkables, stripped_native_linkable_assets
