load("@fbcode//buck2/prelude:paths.bzl", "paths")
load("@fbcode//buck2/prelude/android:android_providers.bzl", "AndroidBinaryNativeLibsInfo", "CPU_FILTER_TO_ABI_DIRECTORY")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/android:voltron.bzl", "ROOT_MODULE", "all_targets_in_root_module", "is_root_module")
load("@fbcode//buck2/prelude/linking:shared_libraries.bzl", "SharedLibraryInfo", "merge_shared_libraries", "traverse_shared_library_info")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect", "filter_and_map_idx")

_NATIVE_LIBS_AS_ASSETS_DIR = "assets/lib"

def get_android_binary_native_library_info(
        ctx: "context",
        android_packageable_info: "AndroidPackageableInfo",
        deps_by_platform: {str.type: ["dependency"]},
        apk_module_graph_file: ["artifact", None] = None,
        prebuilt_native_library_dirs_to_exclude: ["PrebuiltNativeLibraryDir"] = [],
        shared_libraries_to_exclude: ["SharedLibrary"] = []) -> AndroidBinaryNativeLibsInfo.type:
    traversed_prebuilt_native_library_dirs = android_packageable_info.prebuilt_native_library_dirs.traverse() if android_packageable_info.prebuilt_native_library_dirs else []
    all_prebuilt_native_library_dirs = [native_lib for native_lib in traversed_prebuilt_native_library_dirs if native_lib not in prebuilt_native_library_dirs_to_exclude]

    unstripped_libs = []
    all_shared_libraries = []
    platform_to_native_linkables = {}
    for platform, deps in deps_by_platform.items():
        shared_library_info = merge_shared_libraries(
            ctx.actions,
            deps = filter_and_map_idx(SharedLibraryInfo, deps),
        )
        native_linkables = {so_name: shared_lib for so_name, shared_lib in traverse_shared_library_info(shared_library_info).items() if shared_lib not in shared_libraries_to_exclude}
        all_shared_libraries.extend(native_linkables.values())
        unstripped_libs += [shared_lib.lib.output for shared_lib in native_linkables.values()]
        platform_to_native_linkables[platform] = native_linkables

    if apk_module_graph_file == None:
        native_libs_for_primary_apk, native_lib_assets_map = _get_native_libs_and_assets(
            ctx,
            all_targets_in_root_module,
            all_prebuilt_native_library_dirs,
            platform_to_native_linkables,
        )
        expect(len(native_lib_assets_map) <= 1, "Should not have native library assets declared for anything other than the root module!")
        return AndroidBinaryNativeLibsInfo(
            apk_under_test_prebuilt_native_library_dirs = all_prebuilt_native_library_dirs,
            apk_under_test_shared_libraries = all_shared_libraries,
            native_libs_for_primary_apk = native_libs_for_primary_apk,
            unstripped_libs = unstripped_libs,
            native_lib_assets = native_lib_assets_map.get(ROOT_MODULE, []),
        )
    else:
        fail("apk_module_graph_file support is not yet added!")

def _get_native_libs_and_assets(
        ctx: "context",
        get_module_from_target: "function",
        all_prebuilt_native_library_dirs: ["PrebuiltNativeLibraryDir"],
        platform_to_native_linkables: {str.type: {str.type: "SharedLibrary"}}) -> (["artifact"], {str.type: ["artifact"]}):
    is_packaging_native_libs_as_assets_supported = getattr(ctx.attrs, "package_asset_libraries", False)

    prebuilt_native_library_dirs_for_primary_apk, prebuilt_native_library_dir_assets_map = [], {}
    for native_lib in all_prebuilt_native_library_dirs:
        native_lib_target = str(native_lib.raw_target)
        module = get_module_from_target(native_lib_target)
        if not is_root_module(module) or (native_lib.is_asset and is_packaging_native_libs_as_assets_supported):
            # In buck1, we always package native libs as assets when they are not in the root module
            expect(is_root_module(module) or not native_lib.for_primary_apk, "{} which is marked as needing to be in the primary APK cannot be included in non-root-module {}".format(native_lib_target, module))
            prebuilt_native_library_dir_assets_map.setdefault(module, []).append(native_lib)
        else:
            prebuilt_native_library_dirs_for_primary_apk.append(native_lib)

    native_libs_for_primary_apk = _filter_prebuilt_native_library_dir(
        ctx,
        prebuilt_native_library_dirs_for_primary_apk,
        "native_libs_for_primary_apk",
    )
    native_lib_assets_map = {}
    for module, native_lib_assets in prebuilt_native_library_dir_assets_map.items():
        native_lib_assets_map[module] = _filter_prebuilt_native_library_dir(
            ctx,
            native_lib_assets,
            "native_lib_assets_for_module_{}".format(module),
            package_as_assets = True,
        )

    stripped_native_linkables_for_primary_apk, stripped_native_linkable_assets_map = _get_native_linkables(ctx, platform_to_native_linkables, get_module_from_target, is_packaging_native_libs_as_assets_supported)
    native_libs_for_primary_apk.append(stripped_native_linkables_for_primary_apk)
    for module, native_linkable_assets in stripped_native_linkable_assets_map.items():
        native_lib_assets_map.setdefault(module, []).append(native_linkable_assets)

    for module, native_lib_assets in native_lib_assets_map.items():
        metadata_file, native_library_paths = _get_native_libs_as_assets_metadata(ctx, native_lib_assets, module)
        if not is_root_module(module) or ctx.attrs.compress_asset_libraries:
            compressed_libs = _get_compressed_native_libs_as_assets(ctx, native_lib_assets, native_library_paths, module)
            native_lib_assets_map[module] = [metadata_file, compressed_libs]
        else:
            native_lib_assets.append(metadata_file)

    return native_libs_for_primary_apk, native_lib_assets_map

def _filter_prebuilt_native_library_dir(
        ctx: "context",
        native_libs: ["PrebuiltNativeLibraryDir"],
        identifier: str.type,
        package_as_assets = False) -> ["artifact"]:
    cpu_filters = ctx.attrs.cpu_filters or CPU_FILTER_TO_ABI_DIRECTORY.keys()
    abis = [CPU_FILTER_TO_ABI_DIRECTORY[cpu] for cpu in cpu_filters]
    filter_tool = ctx.attrs._android_toolchain[AndroidToolchainInfo].filter_prebuilt_native_library_dir[RunInfo]
    libs = []
    for i, native_lib in enumerate(reversed(native_libs)):
        lib_identifier = "{}-{}".format(identifier, i)
        base_output_dir = ctx.actions.declare_output(lib_identifier)
        output_dir = base_output_dir.project(_NATIVE_LIBS_AS_ASSETS_DIR) if package_as_assets else base_output_dir
        ctx.actions.run(
            [filter_tool, native_lib.dir, output_dir.as_output(), "--abis"] + abis,
            category = "filter_prebuilt_native_library_dir",
            identifier = lib_identifier,
        )
        libs.append(base_output_dir)

    return libs

def _get_native_linkables(
        ctx: "context",
        platform_to_native_linkables: {str.type: {str.type: "SharedLibrary"}},
        get_module_from_target: "function",
        package_native_libs_as_assets_enabled: bool.type) -> ("artifact", {str.type: "artifact"}):
    stripped_native_linkables_for_primary_apk_srcs = {}
    stripped_native_linkable_assets_srcs = {}

    cpu_filters = ctx.attrs.cpu_filters
    for platform, native_linkables in platform_to_native_linkables.items():
        if cpu_filters and platform not in cpu_filters:
            return [], {}

        abi_directory = CPU_FILTER_TO_ABI_DIRECTORY[platform]
        for so_name, native_linkable in native_linkables.items():
            native_linkable_target = str(native_linkable.label.raw_target())
            module = get_module_from_target(native_linkable_target)

            if not is_root_module(module) or (native_linkable.can_be_asset and package_native_libs_as_assets_enabled):
                expect(is_root_module(module) or not native_linkable.for_primary_apk, "{} which is marked as needing to be in the primary APK cannot be included in non-root-module {}".format(native_linkable_target, module))
                so_name_path = paths.join(_get_native_libs_as_assets_dir(module), abi_directory, so_name)
                stripped_native_linkable_assets_srcs.setdefault(module, {})[so_name_path] = native_linkable.stripped_lib
            else:
                so_name_path = paths.join(abi_directory, so_name)
                stripped_native_linkables_for_primary_apk_srcs[so_name_path] = native_linkable.stripped_lib

    stripped_native_linkables_for_primary_apk = ctx.actions.symlinked_dir(
        "stripped_native_linkables_for_primary_apk",
        stripped_native_linkables_for_primary_apk_srcs,
    )
    stripped_native_linkable_assets_map = {}
    for module, srcs in stripped_native_linkable_assets_srcs.items():
        stripped_native_linkable_assets_map[module] = ctx.actions.symlinked_dir(
            "stripped_native_linkable_assets_for_module_{}".format(module),
            srcs,
        )

    return (stripped_native_linkables_for_primary_apk, stripped_native_linkable_assets_map)

def _get_native_libs_as_assets_metadata(
        ctx: "context",
        native_lib_assets: ["artifact"],
        module: str.type) -> ("artifact", "artifact"):
    native_lib_assets_file = ctx.actions.write("{}/native_lib_assets".format(module), [cmd_args([native_lib_asset, _NATIVE_LIBS_AS_ASSETS_DIR], delimiter = "/") for native_lib_asset in native_lib_assets])
    metadata_output = ctx.actions.declare_output("{}/native_libs_as_assets_metadata.txt".format(module))
    native_library_paths = ctx.actions.declare_output("{}/native_libs_as_assets_paths.txt".format(module))
    metadata_cmd = cmd_args([
        ctx.attrs._android_toolchain[AndroidToolchainInfo].native_libs_as_assets_metadata[RunInfo],
        "--native-library-dirs",
        native_lib_assets_file,
        "--metadata-output",
        metadata_output.as_output(),
        "--native-library-paths-output",
        native_library_paths.as_output(),
    ]).hidden(native_lib_assets)
    ctx.actions.run(metadata_cmd, category = "get_native_libs_as_assets_metadata")
    return ctx.actions.symlinked_dir("{}/native_libs_as_assets_metadata".format(module), {paths.join(_NATIVE_LIBS_AS_ASSETS_DIR, "metadata.txt"): metadata_output}), native_library_paths

def _get_compressed_native_libs_as_assets(
        ctx: "context",
        native_lib_assets: ["artifact"],
        native_library_paths: "artifact",
        module: str.type) -> "artifact":
    output_dir = ctx.actions.declare_output("{}/compressed_native_libs_as_assets_dir".format(module))
    compressed_libraries_cmd = cmd_args([
        ctx.attrs._android_toolchain[AndroidToolchainInfo].compress_libraries[RunInfo],
        "--libraries",
        native_library_paths,
        "--output-dir",
        output_dir.as_output(),
        "--compression-type",
        ctx.attrs.asset_compression_algorithm or "xz",
        "--xz-compression-level",
        str(ctx.attrs.xz_compression_level),
    ]).hidden(native_lib_assets)
    ctx.actions.run(compressed_libraries_cmd, category = "compress_native_libs_as_assets")
    return ctx.actions.symlinked_dir("{}/compress_native_libs_as_assets".format(module), {_NATIVE_LIBS_AS_ASSETS_DIR: output_dir})

def _get_native_libs_as_assets_dir(module: str.type) -> str.type:
    return "assets/{}".format("lib" if is_root_module(module) else module)
