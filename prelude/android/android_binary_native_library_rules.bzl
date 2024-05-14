# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load(
    "@prelude//android:android_providers.bzl",
    "AndroidBinaryNativeLibsInfo",
    "AndroidPackageableInfo",
    "ExopackageNativeInfo",
    "PrebuiltNativeLibraryDir",  # @unused Used as a type
)
load("@prelude//android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@prelude//android:cpu_filters.bzl", "CPU_FILTER_FOR_PRIMARY_PLATFORM", "CPU_FILTER_TO_ABI_DIRECTORY")
load("@prelude//android:util.bzl", "EnhancementContext")
load("@prelude//android:voltron.bzl", "ROOT_MODULE", "all_targets_in_root_module", "get_apk_module_graph_info", "is_root_module")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo", "PicBehavior")
load(
    "@prelude//cxx:link.bzl",
    "cxx_link_shared_library",
)
load("@prelude//cxx:link_types.bzl", "link_options")
load(
    "@prelude//cxx:symbols.bzl",
    "extract_global_syms",
    "extract_undefined_syms",
)
load("@prelude//java:java_library.bzl", "compile_to_jar")  # @unused
load("@prelude//java:java_providers.bzl", "JavaClasspathEntry", "JavaLibraryInfo", "derive_compiling_deps")  # @unused
load("@prelude//linking:execution_preference.bzl", "LinkExecutionPreference")
load(
    "@prelude//linking:link_info.bzl",
    "LibOutputStyle",
    "LinkArgs",
    "LinkInfo",
    "LinkOrdering",
    "SharedLibLinkable",
    "get_lib_output_style",
    "set_link_info_link_whole",
    "unpack_link_args",
    "wrap_link_info",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "LinkableGraph",  # @unused Used as a type
    "LinkableNode",  # @unused Used as a type
    "create_linkable_graph",
    "get_linkable_graph_node_map_func",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibrary",  # @unused Used as a type
    "SharedLibraryInfo",  # @unused Used as a type
    "create_shlib",
    "get_strip_non_global_flags",
    "merge_shared_libraries",
    "traverse_shared_library_info",
    "with_unique_str_sonames",
)
load("@prelude//linking:strip.bzl", "strip_object")
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//utils:expect.bzl", "expect")
load("@prelude//utils:graph_utils.bzl", "breadth_first_traversal_by", "post_order_traversal", "pre_order_traversal")
load("@prelude//utils:set.bzl", "set", "set_type")  # @unused Used as a type
load("@prelude//utils:utils.bzl", "dedupe_by_value")

# Native libraries on Android are built for a particular Application Binary Interface (ABI). We
# package native libraries for one (or more, for multi-arch builds) ABIs into an Android APK.
#
# Our native libraries come from two sources:
# 1. "Prebuilt native library dirs", which are directory artifacts whose sub-directories are ABIs,
#    and those ABI subdirectories contain native libraries. These come from `android_prebuilt_aar`s
#    and `prebuilt_native_library`s, for example.
# 2. "Native linkables". These are each a single shared library - `.so`s for one particular ABI.
#
# Native libraries can be packaged into Android APKs in two ways.
# 1. As native libraries. This means that they are passed to the APK builder as native libraries,
#    and the APK builder will package `<ABI>/library.so` into the APK at `libs/<ABI>/library.so`.
# 2. As assets. These are passed to the APK build as assets, and are stored at
#    `assets/lib/<ABI>/library.so` In the root module, we only package a native library as an
#    asset if it is eligible to be an asset (e.g. `can_be_asset` on a `cxx_library`), and
#    `package_asset_libraries` is set to True for the APK. We create a metadata file at
#    `assets/libs/metadata.txt` that has a single line entry for each packaged asset consisting of
#    '<ABI/library_name> <file_size> <sha256>'.
#
#    Any native library that is not part of the root module (i.e. it is part of some other Voltron
#    module) is automatically packaged as an asset. Similarly, the metadata for each module is stored
#    at `assets/<module_name>/libs.txt`.

def get_android_binary_native_library_info(
        enhance_ctx: EnhancementContext,
        android_packageable_info: AndroidPackageableInfo,
        deps_by_platform: dict[str, list[Dependency]],
        apk_module_graph_file: Artifact | None = None,
        prebuilt_native_library_dirs_to_exclude: [set_type, None] = None,
        shared_libraries_to_exclude: [set_type, None] = None) -> AndroidBinaryNativeLibsInfo:
    ctx = enhance_ctx.ctx

    traversed_prebuilt_native_library_dirs = android_packageable_info.prebuilt_native_library_dirs.traverse() if android_packageable_info.prebuilt_native_library_dirs else []
    all_prebuilt_native_library_dirs = [
        native_lib
        for native_lib in traversed_prebuilt_native_library_dirs
        if not (prebuilt_native_library_dirs_to_exclude and prebuilt_native_library_dirs_to_exclude.contains(native_lib.raw_target))
    ]

    included_shared_lib_targets = []
    original_shared_libs_by_platform = {}  # dict[str, dict[str (soname), list[SharedLibrary]]]
    for platform, deps in deps_by_platform.items():
        if platform == CPU_FILTER_FOR_PRIMARY_PLATFORM and platform not in ctx.attrs.cpu_filters and len(ctx.attrs.cpu_filters) != 0:
            continue

        shared_libs = get_default_shared_libs(ctx, deps, shared_libraries_to_exclude)
        included_shared_lib_targets.extend([lib.label.raw_target() for lib in shared_libs.values()])
        original_shared_libs_by_platform[platform] = shared_libs

    if not all_prebuilt_native_library_dirs and not included_shared_lib_targets:
        enhance_ctx.debug_output("unstripped_native_libraries", ctx.actions.write("unstripped_native_libraries", []))
        enhance_ctx.debug_output("unstripped_native_libraries_json", ctx.actions.write_json("unstripped_native_libraries_json", {}))
        return AndroidBinaryNativeLibsInfo(
            prebuilt_native_library_dirs = [],
            shared_libraries = [],
            native_libs_for_primary_apk = [],
            exopackage_info = None,
            root_module_native_lib_assets = [],
            non_root_module_native_lib_assets = [],
            generated_java_code = [],
            unstripped_shared_libraries = None,
        )

    native_libs = ctx.actions.declare_output("native_libs_symlink")
    native_libs_metadata = ctx.actions.declare_output("native_libs_metadata_symlink")
    native_libs_always_in_primary_apk = ctx.actions.declare_output("native_libs_always_in_primary_apk_symlink")
    native_lib_assets_for_primary_apk = ctx.actions.declare_output("native_lib_assets_for_primary_apk_symlink")
    stripped_native_linkable_assets_for_primary_apk = ctx.actions.declare_output("stripped_native_linkable_assets_for_primary_apk_symlink")
    root_module_metadata_assets = ctx.actions.declare_output("root_module_metadata_assets_symlink")
    non_root_module_metadata_assets = ctx.actions.declare_output("non_root_module_metadata_assets_symlink")
    non_root_module_lib_assets = ctx.actions.declare_output("non_root_module_lib_assets_symlink")

    unstripped_native_libraries = ctx.actions.declare_output("unstripped_native_libraries")
    unstripped_native_libraries_json = ctx.actions.declare_output("unstripped_native_libraries_json")
    unstripped_native_libraries_files = ctx.actions.declare_output("unstripped_native_libraries.links", dir = True)

    dynamic_outputs = [
        native_libs,
        native_libs_metadata,
        native_libs_always_in_primary_apk,
        native_lib_assets_for_primary_apk,
        unstripped_native_libraries,
        unstripped_native_libraries_json,
        unstripped_native_libraries_files,
        stripped_native_linkable_assets_for_primary_apk,
        root_module_metadata_assets,
        non_root_module_metadata_assets,
        non_root_module_lib_assets,
    ]

    fake_input = ctx.actions.write("dynamic.trigger", "")

    # some cases don't actually need to use a dynamic_output, but it's simplest to consistently use it anyway. we need some fake input to allow that.
    dynamic_inputs = [fake_input]
    if apk_module_graph_file:
        dynamic_inputs.append(apk_module_graph_file)
    split_groups_map = None
    native_library_merge_dir = None
    native_merge_debug = None
    generated_java_code = []

    glue_linkables = None
    if getattr(ctx.attrs, "native_library_merge_glue", None):
        glue_linkables = {}
        for platform, glue in ctx.attrs.native_library_merge_glue.items():
            glue_link_graph = glue.get(LinkableGraph)
            expect(glue_link_graph != None, "native_library_merge_glue (`{}`) should be a linkable target", glue.label)
            glue_linkable = glue_link_graph.nodes.value.linkable
            expect(glue_linkable != None, "native_library_merge_glue (`{}`) should be a linkable target", glue.label)
            expect(glue_linkable.preferred_linkage == Linkage("static"), "buck2 currently only supports preferred_linkage='static' native_library_merge_glue")
            glue_linkables[platform] = (glue.label, glue_linkable.link_infos[LibOutputStyle("pic_archive")].default)

    linkable_nodes_by_platform = {}
    native_library_merge_sequence = getattr(ctx.attrs, "native_library_merge_sequence", None)
    native_library_merge_map = getattr(ctx.attrs, "native_library_merge_map", None)
    has_native_merging = native_library_merge_sequence or native_library_merge_map
    enable_relinker = getattr(ctx.attrs, "enable_relinker", False)

    if has_native_merging or enable_relinker:
        native_merge_debug = ctx.actions.declare_output("native_merge_debug", dir = True)
        dynamic_outputs.append(native_merge_debug)

        # We serialize info about the linkable graph and the apk module mapping and pass that to an
        # external subcommand to apply a merge sequence algorithm and return us the merge mapping.
        for platform, deps in deps_by_platform.items():
            linkable_graph = create_linkable_graph(ctx, deps = deps)
            graph_node_map = get_linkable_graph_node_map_func(linkable_graph)()
            linkables_debug = ctx.actions.write("linkables." + platform, list(graph_node_map.keys()))
            enhance_ctx.debug_output("linkables." + platform, linkables_debug)
            linkable_nodes_by_platform[platform] = graph_node_map

    lib_outputs_by_platform = _declare_library_subtargets(ctx, dynamic_outputs, original_shared_libs_by_platform, native_library_merge_map, native_library_merge_sequence, enable_relinker)

    if native_library_merge_sequence:
        native_library_merge_input_file = ctx.actions.write_json("mergemap.input", {
            "linkable_graphs_by_platform": encode_linkable_graph_for_mergemap(linkable_nodes_by_platform),
            "native_library_merge_sequence": ctx.attrs.native_library_merge_sequence,
            "native_library_merge_sequence_blocklist": ctx.attrs.native_library_merge_sequence_blocklist or [],
        })
        mergemap_cmd = cmd_args(ctx.attrs._android_toolchain[AndroidToolchainInfo].mergemap_tool)
        mergemap_cmd.add(cmd_args(native_library_merge_input_file, format = "--mergemap-input={}"))
        if apk_module_graph_file:
            mergemap_cmd.add(cmd_args(apk_module_graph_file, format = "--apk-module-graph={}"))
        native_library_merge_dir = ctx.actions.declare_output("merge_sequence_output")
        native_library_merge_map = native_library_merge_dir.project("merge.map")
        split_groups_map = native_library_merge_dir.project("split_groups.map")
        mergemap_cmd.add(cmd_args(native_library_merge_dir.as_output(), format = "--output={}"))
        ctx.actions.run(mergemap_cmd, category = "compute_mergemap")
        enhance_ctx.debug_output("compute_merge_sequence", native_library_merge_dir)

        dynamic_inputs.append(native_library_merge_map)
        dynamic_inputs.append(split_groups_map)

    mergemap_gencode_jar = None
    if has_native_merging and ctx.attrs.native_library_merge_code_generator:
        mergemap_gencode_jar = ctx.actions.declare_output("MergedLibraryMapping.jar")
        dynamic_outputs.append(mergemap_gencode_jar)
        library_output = JavaClasspathEntry(
            full_library = mergemap_gencode_jar,
            abi = mergemap_gencode_jar,
            abi_as_dir = None,
            required_for_source_only_abi = False,
        )
        generated_java_code.append(
            JavaLibraryInfo(
                compiling_deps = derive_compiling_deps(ctx.actions, library_output, []),
                library_output = library_output,
                output_for_classpath_macro = library_output.full_library,
            ),
        )

    def dynamic_native_libs_info(ctx: AnalysisContext, artifacts, outputs):
        get_module_from_target = all_targets_in_root_module
        if apk_module_graph_file:
            get_module_from_target = get_apk_module_graph_info(ctx, apk_module_graph_file, artifacts).target_to_module_mapping_function

        split_groups = None
        merged_shared_lib_targets_by_platform = {}  # dict[str, dict[Label, str]]
        if has_native_merging:
            native_library_merge_debug_outputs = {}

            # When changing this dynamic_output, the workflow is a lot better if you compute the module graph once and
            # then set it as the binary's precomputed_apk_module_graph attr.
            if ctx.attrs.native_library_merge_sequence:
                merge_map_by_platform = artifacts[native_library_merge_map].read_json()
                split_groups = artifacts[split_groups_map].read_json()
                native_library_merge_debug_outputs["merge_sequence_output"] = native_library_merge_dir
            elif ctx.attrs.native_library_merge_map:
                merge_map_by_platform = {}
                for platform, linkable_nodes in linkable_nodes_by_platform.items():
                    merge_map = merge_map_by_platform.setdefault(platform, {})
                    merge_lib_to_fancy_regexes = {
                        merge_lib: [regex(pattern, fancy = True) for pattern in patterns]
                        for merge_lib, patterns in ctx.attrs.native_library_merge_map.items()
                    }
                    for target, _node in linkable_nodes.items():
                        raw_target = str(target.raw_target())
                        merge_result = None
                        for merge_lib, fancy_regexes in merge_lib_to_fancy_regexes.items():
                            for fancy_regex in fancy_regexes:
                                if fancy_regex.match(raw_target):
                                    merge_result = merge_lib
                                    break
                            if merge_result:
                                break
                        if merge_result:
                            merge_map[str(target)] = merge_result
                merge_map = ctx.actions.write_json("merge.map", merge_map_by_platform, pretty = True)
                native_library_merge_debug_outputs["merge_map_output"] = merge_map
            else:
                fail("unreachable")

            shared_object_targets = {}
            debug_info_by_platform = {}  # dict[str, MergedLinkablesDebugInfo]
            merged_shared_libs_by_platform = {}  # dict[str, dict[str, MergedSharedLibrary]]
            for platform in original_shared_libs_by_platform:
                merged_shared_libs, debug_info = _get_merged_linkables_for_platform(
                    ctx,
                    ctx.attrs._cxx_toolchain[platform][CxxToolchainInfo],
                    platform if len(original_shared_libs_by_platform) > 1 else None,
                    glue_linkable = glue_linkables[platform] if glue_linkables else None,
                    default_shared_libs = original_shared_libs_by_platform[platform],
                    linkable_nodes = linkable_nodes_by_platform[platform],
                    merge_map = merge_map_by_platform[platform],
                    merge_linker_args = ctx.attrs.native_library_merge_linker_args or {},
                    apk_module_graph = get_module_from_target,
                )
                debug_info_by_platform[platform] = debug_info
                merged_shared_libs_by_platform[platform] = merged_shared_libs
                merged_shared_lib_targets = {}
                for soname, lib in merged_shared_libs.items():
                    shared_object_targets[soname] = [str(target.raw_target()) for target in lib.primary_constituents]

                    for target in lib.primary_constituents:
                        merged_shared_lib_targets[target] = soname
                merged_shared_lib_targets_by_platform[platform] = merged_shared_lib_targets

            debug_data_json = ctx.actions.write_json("native_merge_debug.json", debug_info_by_platform, pretty = True)
            native_library_merge_debug_outputs["native_merge_debug.json"] = debug_data_json

            shared_object_targets_lines = ""
            for soname, targets in shared_object_targets.items():
                shared_object_targets_lines += soname + " " + " ".join(targets) + "\n"
            shared_object_targets_txt = ctx.actions.write("shared_object_targets.txt", shared_object_targets_lines)
            native_library_merge_debug_outputs["shared_object_targets.txt"] = shared_object_targets_txt

            if mergemap_gencode_jar:
                merged_library_map = write_merged_library_map(ctx, merged_shared_libs_by_platform)
                mergemap_gencode = run_mergemap_codegen(ctx, merged_library_map)
                compile_to_jar(ctx, [mergemap_gencode], output = outputs[mergemap_gencode_jar])
                native_library_merge_debug_outputs["NativeLibraryMergeGeneratedCode.java"] = mergemap_gencode
                native_library_merge_debug_outputs["merged_library_map.txt"] = merged_library_map
                native_library_merge_debug_outputs["mergemap_gencode.jar"] = mergemap_gencode_jar

            ctx.actions.symlinked_dir(outputs[native_merge_debug], native_library_merge_debug_outputs)

            final_shared_libs_by_platform = {
                platform: {soname: d.lib for soname, d in merged_shared_libs.items()}
                for platform, merged_shared_libs in merged_shared_libs_by_platform.items()
            }
        elif enable_relinker:
            final_shared_libs_by_platform, native_library_merge_debug_outputs = _create_all_relinkable_links(
                ctx,
                original_shared_libs_by_platform,
                linkable_nodes_by_platform,
            )
            ctx.actions.symlinked_dir(outputs[native_merge_debug], native_library_merge_debug_outputs)

        else:
            final_shared_libs_by_platform = original_shared_libs_by_platform

        if enable_relinker:
            unrelinked_shared_libs_by_platform = final_shared_libs_by_platform
            final_shared_libs_by_platform = relink_libraries(ctx, final_shared_libs_by_platform)
            _link_library_subtargets(ctx, outputs, lib_outputs_by_platform, original_shared_libs_by_platform, unrelinked_shared_libs_by_platform, merged_shared_lib_targets_by_platform, split_groups, native_merge_debug, unrelinked = True)

        _link_library_subtargets(ctx, outputs, lib_outputs_by_platform, original_shared_libs_by_platform, final_shared_libs_by_platform, merged_shared_lib_targets_by_platform, split_groups, native_merge_debug)

        unstripped_libs = {}
        for platform, libs in final_shared_libs_by_platform.items():
            for lib in libs.values():
                unstripped_libs[lib.lib.output] = platform
        ctx.actions.write(outputs[unstripped_native_libraries], unstripped_libs.keys())
        ctx.actions.write_json(outputs[unstripped_native_libraries_json], unstripped_libs)
        ctx.actions.symlinked_dir(outputs[unstripped_native_libraries_files], {
            "{}/{}".format(platform, lib.short_path): lib
            for lib, platform in unstripped_libs.items()
        })

        dynamic_info = _get_native_libs_and_assets(
            ctx,
            get_module_from_target,
            all_prebuilt_native_library_dirs,
            final_shared_libs_by_platform,
        )

        # Since we are using a dynamic action, we need to declare the outputs in advance.
        # Rather than passing the created outputs into `_get_native_libs_and_assets`, we
        # just symlink to the outputs that function produces.
        ctx.actions.symlink_file(outputs[native_libs], dynamic_info.native_libs)
        ctx.actions.symlink_file(outputs[native_libs_metadata], dynamic_info.native_libs_metadata)
        ctx.actions.symlink_file(outputs[native_libs_always_in_primary_apk], dynamic_info.native_libs_always_in_primary_apk)
        ctx.actions.symlink_file(outputs[native_lib_assets_for_primary_apk], dynamic_info.native_lib_assets_for_primary_apk if dynamic_info.native_lib_assets_for_primary_apk else ctx.actions.symlinked_dir("empty_native_lib_assets", {}))
        ctx.actions.symlink_file(outputs[stripped_native_linkable_assets_for_primary_apk], dynamic_info.stripped_native_linkable_assets_for_primary_apk if dynamic_info.stripped_native_linkable_assets_for_primary_apk else ctx.actions.symlinked_dir("empty_stripped_native_linkable_assets", {}))
        ctx.actions.symlink_file(outputs[root_module_metadata_assets], dynamic_info.root_module_metadata_assets)
        ctx.actions.symlink_file(outputs[non_root_module_metadata_assets], dynamic_info.non_root_module_metadata_assets)
        ctx.actions.symlink_file(outputs[non_root_module_lib_assets], dynamic_info.non_root_module_lib_assets if dynamic_info.non_root_module_lib_assets else ctx.actions.symlinked_dir("empty_non_root_module_lib_assets", {}))

    ctx.actions.dynamic_output(dynamic = dynamic_inputs, inputs = [], outputs = [o.as_output() for o in dynamic_outputs], f = dynamic_native_libs_info)
    all_native_libs = ctx.actions.symlinked_dir("debug_all_native_libs", {"others": native_libs, "primary": native_libs_always_in_primary_apk})

    lib_subtargets = _create_library_subtargets(lib_outputs_by_platform, native_libs)
    enhance_ctx.debug_output("native_libs", all_native_libs, sub_targets = lib_subtargets)
    if native_merge_debug:
        enhance_ctx.debug_output("native_merge_debug", native_merge_debug)

    enhance_ctx.debug_output("unstripped_native_libraries", unstripped_native_libraries, other_outputs = [unstripped_native_libraries_files])
    enhance_ctx.debug_output("unstripped_native_libraries_json", unstripped_native_libraries_json, other_outputs = [unstripped_native_libraries_files])

    native_libs_for_primary_apk, exopackage_info = _get_exopackage_info(ctx, native_libs_always_in_primary_apk, native_libs, native_libs_metadata)
    return AndroidBinaryNativeLibsInfo(
        prebuilt_native_library_dirs = all_prebuilt_native_library_dirs,
        shared_libraries = included_shared_lib_targets,
        native_libs_for_primary_apk = native_libs_for_primary_apk,
        exopackage_info = exopackage_info,
        root_module_native_lib_assets = [native_lib_assets_for_primary_apk, stripped_native_linkable_assets_for_primary_apk, root_module_metadata_assets],
        non_root_module_native_lib_assets = [non_root_module_metadata_assets, non_root_module_lib_assets],
        generated_java_code = generated_java_code,
        unstripped_shared_libraries = unstripped_native_libraries_files,
    )

_NativeLibSubtargetArtifacts = record(
    default = Artifact,
    unrelinked = Artifact | None,
)

# Merged libraries are dynamic dependencies, but outputs need to be declared in advance to be used by subtargets.
# This means we have to declare outputs for all possible merged libs (every merged name and every unmerged library name).
def _declare_library_subtargets(
        ctx: AnalysisContext,
        dynamic_outputs: list[Artifact],
        original_shared_libs_by_platform: dict[str, dict[str, SharedLibrary]],
        native_library_merge_map,
        native_library_merge_sequence,
        enable_relinker: bool) -> dict[str, dict[str, _NativeLibSubtargetArtifacts]]:
    lib_outputs_by_platform = {}
    for platform, original_shared_libs in original_shared_libs_by_platform.items():
        sonames = set()
        sonames.update(original_shared_libs.keys())
        if native_library_merge_map:
            sonames.update(native_library_merge_map.keys())
        elif native_library_merge_sequence:
            for entry in native_library_merge_sequence:
                if type(entry) == "list":
                    sonames.update([soname for (soname, _) in entry])
                else:
                    (soname, _) = entry
                    sonames.add(soname)

        lib_outputs = {}
        for soname in sonames.list():
            output_path = _platform_output_path(soname, platform if len(original_shared_libs_by_platform) > 1 else None)
            lib_output = ctx.actions.declare_output(output_path, dir = True)
            dynamic_outputs.append(lib_output)
            if enable_relinker:
                output_path = output_path + ".unrelinked"
                unrelinked_lib_output = ctx.actions.declare_output(output_path, dir = True)
                dynamic_outputs.append(unrelinked_lib_output)
                lib_outputs[soname] = _NativeLibSubtargetArtifacts(
                    default = lib_output,
                    unrelinked = unrelinked_lib_output,
                )
            else:
                lib_outputs[soname] = _NativeLibSubtargetArtifacts(
                    default = lib_output,
                    unrelinked = None,
                )

        lib_outputs_by_platform[platform] = lib_outputs
    return lib_outputs_by_platform

# Bind debug library subtarget outputs to actual outputs.
# For individual libraries, link to either the unmerged or merged output.
# For merged libraries, link to either the merged output, or a symlinked dir of all merged split group outputs.
def _link_library_subtargets(
        ctx: AnalysisContext,
        outputs,  # IndexSet[OutputArtifact]
        lib_outputs_by_platform: dict[str, dict[str, _NativeLibSubtargetArtifacts]],  # dict[platform, dict[soname,  _NativeLibSubtargetArtifacts]]
        original_shared_libs_by_platform: dict[str, dict[str, SharedLibrary]],
        final_shared_libs_by_platform: dict[str, dict[str, SharedLibrary]],
        merged_shared_lib_targets_by_platform: dict[str, dict[Label, str]],
        split_groups: dict[str, str] | None,
        native_merge_debug,
        unrelinked: bool = False):
    for platform, final_shared_libs in final_shared_libs_by_platform.items():
        merged_lib_outputs = {}
        for soname, lib in final_shared_libs.items():
            base_soname = soname
            if split_groups and soname in split_groups:
                base_soname = split_groups[soname]

            group_outputs = merged_lib_outputs.setdefault(base_soname, {})
            group_outputs[soname] = lib.lib.output

        for soname, lib_outputs in lib_outputs_by_platform[platform].items():
            if soname in merged_lib_outputs:
                group_outputs = merged_lib_outputs[soname]
            elif soname in original_shared_libs_by_platform[platform]:
                # link unmerged soname to merged output
                original_shared_lib = original_shared_libs_by_platform[platform][soname]
                merged_soname = merged_shared_lib_targets_by_platform[platform][original_shared_lib.label]
                if split_groups and merged_soname in split_groups:
                    merged_soname = split_groups[merged_soname]
                group_outputs = merged_lib_outputs[merged_soname]
            else:
                # merged group name has no constituents, link to debug output
                group_outputs = {soname: native_merge_debug}

            output = lib_outputs.default
            if unrelinked:
                output = lib_outputs.unrelinked
            ctx.actions.symlinked_dir(outputs[output], group_outputs)

def _create_library_subtargets(lib_outputs_by_platform: dict[str, dict[str, _NativeLibSubtargetArtifacts]], native_libs: Artifact):
    def create_library_subtarget(output: _NativeLibSubtargetArtifacts):
        if output.unrelinked:
            sub_targets = {"unrelinked": [DefaultInfo(default_outputs = [output.unrelinked])]}
            return [DefaultInfo(default_outputs = [output.default], sub_targets = sub_targets)]
        return [DefaultInfo(default_outputs = [output.default])]

    if len(lib_outputs_by_platform) > 1:
        return {
            platform: [DefaultInfo(default_outputs = [native_libs], sub_targets = {
                soname: create_library_subtarget(output)
                for soname, output in lib_outputs.items()
            })]
            for platform, lib_outputs in lib_outputs_by_platform.items()
        }
    elif len(lib_outputs_by_platform) == 1:
        lib_outputs = list(lib_outputs_by_platform.values())[0]
        return {
            soname: create_library_subtarget(output)
            for soname, output in lib_outputs.items()
        }
    else:
        # TODO(ctolliday) at this point we should have thrown an error earlier if no libraries matched cpu_filters
        # (or returned earlier if there are no native library deps)
        return {}

# We could just return two artifacts of libs (one for the primary APK, one which can go
# either into the primary APK or be exopackaged), and one artifact of assets,
# but we'd need an extra action in order to combine them (we can't use `symlinked_dir` since
# the paths overlap) so it's easier to just be explicit about exactly what we produce.
_NativeLibsAndAssetsInfo = record(
    native_libs = Artifact,
    native_libs_metadata = Artifact,
    native_libs_always_in_primary_apk = Artifact,
    native_lib_assets_for_primary_apk = Artifact | None,
    stripped_native_linkable_assets_for_primary_apk = Artifact | None,
    root_module_metadata_assets = Artifact,
    non_root_module_metadata_assets = Artifact,
    non_root_module_lib_assets = [Artifact, None],
)

def _get_exopackage_info(
        ctx: AnalysisContext,
        native_libs_always_in_primary_apk: Artifact,
        native_libs: Artifact,
        native_libs_metadata: Artifact) -> (list[Artifact], [ExopackageNativeInfo, None]):
    is_exopackage_enabled_for_native_libs = "native_library" in getattr(ctx.attrs, "exopackage_modes", [])
    if is_exopackage_enabled_for_native_libs:
        return [native_libs_always_in_primary_apk], ExopackageNativeInfo(directory = native_libs, metadata = native_libs_metadata)
    else:
        return [native_libs, native_libs_always_in_primary_apk], None

def _get_native_libs_and_assets(
        ctx: AnalysisContext,
        get_module_from_target: typing.Callable,
        all_prebuilt_native_library_dirs: list[PrebuiltNativeLibraryDir],
        platform_to_native_linkables: dict[str, dict[str, SharedLibrary]]) -> _NativeLibsAndAssetsInfo:
    is_packaging_native_libs_as_assets_supported = getattr(ctx.attrs, "package_asset_libraries", False)

    prebuilt_native_library_dirs = []
    prebuilt_native_library_dirs_always_in_primary_apk = []
    prebuilt_native_library_dir_assets_for_primary_apk = []
    prebuilt_native_library_dir_module_assets_map = {}
    prebuilt_native_library_dir_module_libs_map = {}
    for native_lib in all_prebuilt_native_library_dirs:
        native_lib_target = str(native_lib.raw_target)
        module = get_module_from_target(native_lib_target)
        expect(
            not native_lib.for_primary_apk or is_root_module(module),
            "{} which is marked as needing to be in the primary APK cannot be included in non-root-module {}".format(native_lib_target, module),
        )
        expect(
            not native_lib.for_primary_apk or not native_lib.is_asset,
            "{} which is marked as needing to be in the primary APK cannot be an asset".format(native_lib_target),
        )
        if not is_root_module(module):
            if native_lib.is_asset:
                prebuilt_native_library_dir_module_assets_map.setdefault(module, []).append(native_lib)
            else:
                prebuilt_native_library_dir_module_libs_map.setdefault(module, []).append(native_lib)
        elif native_lib.is_asset and is_packaging_native_libs_as_assets_supported:
            expect(not native_lib.for_primary_apk, "{} which is marked as needing to be in the primary APK cannot be an asset".format(native_lib_target))
            prebuilt_native_library_dir_assets_for_primary_apk.append(native_lib)
        elif native_lib.for_primary_apk:
            prebuilt_native_library_dirs_always_in_primary_apk.append(native_lib)
        else:
            prebuilt_native_library_dirs.append(native_lib)

    native_libs = _filter_prebuilt_native_library_dir(
        ctx,
        prebuilt_native_library_dirs,
        "native_libs",
    )
    native_libs_always_in_primary_apk = _filter_prebuilt_native_library_dir(
        ctx,
        prebuilt_native_library_dirs_always_in_primary_apk,
        "native_libs_always_in_primary_apk",
    )
    native_lib_assets_for_primary_apk = _filter_prebuilt_native_library_dir(
        ctx,
        prebuilt_native_library_dir_assets_for_primary_apk,
        "native_lib_assets_for_primary_apk",
        package_as_assets = True,
        module = ROOT_MODULE,
    ) if prebuilt_native_library_dir_assets_for_primary_apk else None
    native_lib_module_assets_map = {}
    for module, native_lib_dir in prebuilt_native_library_dir_module_assets_map.items():
        native_lib_module_assets_map.setdefault(module, []).append(_filter_prebuilt_native_library_dir(
            ctx,
            native_lib_dir,
            "native_lib_assets_for_module_{}".format(module),
            package_as_assets = True,
            module = module,
        ))
    for module, native_lib_dir in prebuilt_native_library_dir_module_libs_map.items():
        native_lib_module_assets_map.setdefault(module, []).append(_filter_prebuilt_native_library_dir(
            ctx,
            native_lib_dir,
            "native_lib_libs_for_module_{}".format(module),
            package_as_assets = False,
            module = module,
        ))

    stripped_linkables = _get_native_linkables(ctx, platform_to_native_linkables, get_module_from_target, is_packaging_native_libs_as_assets_supported)
    for module, native_linkable_assets in stripped_linkables.linkable_module_assets_map.items():
        native_lib_module_assets_map.setdefault(module, []).append(native_linkable_assets)

    root_module_metadata_srcs = {}
    non_root_module_metadata_srcs = {}
    non_root_module_libs_srcs = []
    assets_for_primary_apk = filter(None, [native_lib_assets_for_primary_apk, stripped_linkables.linkable_assets_for_primary_apk])
    stripped_linkable_assets_for_primary_apk = stripped_linkables.linkable_assets_for_primary_apk
    if assets_for_primary_apk:
        metadata_file = _get_native_libs_as_assets_metadata(ctx, assets_for_primary_apk, ROOT_MODULE)
        root_module_metadata_srcs[paths.join(_get_native_libs_as_assets_dir(ROOT_MODULE), "metadata.txt")] = metadata_file

    for module, native_lib_assets in native_lib_module_assets_map.items():
        metadata_file = _get_native_libs_as_assets_metadata(ctx, native_lib_assets, module)
        libs_metadata_path = paths.join("assets", "libs.txt")
        non_root_module_metadata_srcs[paths.join(_get_native_libs_as_assets_dir(module), libs_metadata_path)] = metadata_file
        non_root_module_libs_srcs.extend(native_lib_assets)

    non_root_module_libs = None
    if non_root_module_libs_srcs:
        non_root_module_libs = ctx.actions.declare_output("non_root_module_libs")
        ctx.actions.run(
            cmd_args([
                ctx.attrs._android_toolchain[AndroidToolchainInfo].combine_native_library_dirs[RunInfo],
                "--output-dir",
                non_root_module_libs.as_output(),
                "--library-dirs",
            ] + non_root_module_libs_srcs),
            category = "combine_non_root_module_native_libs",
        )

    combined_native_libs = ctx.actions.declare_output("combined_native_libs", dir = True)
    native_libs_metadata = ctx.actions.declare_output("native_libs_metadata.txt")
    ctx.actions.run(cmd_args([
        ctx.attrs._android_toolchain[AndroidToolchainInfo].combine_native_library_dirs[RunInfo],
        "--output-dir",
        combined_native_libs.as_output(),
        "--library-dirs",
        native_libs,
        stripped_linkables.linkables,
        "--metadata-file",
        native_libs_metadata.as_output(),
    ]), category = "combine_native_libs")

    combined_native_libs_always_in_primary_apk = ctx.actions.declare_output("combined_native_libs_always_in_primary_apk", dir = True)
    ctx.actions.run(cmd_args([
        ctx.attrs._android_toolchain[AndroidToolchainInfo].combine_native_library_dirs[RunInfo],
        "--output-dir",
        combined_native_libs_always_in_primary_apk.as_output(),
        "--library-dirs",
        native_libs_always_in_primary_apk,
        stripped_linkables.linkables_always_in_primary_apk,
    ]), category = "combine_native_libs_always_in_primary_apk")

    return _NativeLibsAndAssetsInfo(
        native_libs = combined_native_libs,
        native_libs_metadata = native_libs_metadata,
        native_libs_always_in_primary_apk = combined_native_libs_always_in_primary_apk,
        native_lib_assets_for_primary_apk = native_lib_assets_for_primary_apk,
        stripped_native_linkable_assets_for_primary_apk = stripped_linkable_assets_for_primary_apk,
        root_module_metadata_assets = ctx.actions.symlinked_dir("root_module_metadata_assets", root_module_metadata_srcs),
        non_root_module_metadata_assets = ctx.actions.symlinked_dir("non_root_module_metadata_assets", non_root_module_metadata_srcs),
        non_root_module_lib_assets = non_root_module_libs,
    )

def _filter_prebuilt_native_library_dir(
        ctx: AnalysisContext,
        native_libs: list[PrebuiltNativeLibraryDir],
        identifier: str,
        package_as_assets: bool = False,
        module: str = ROOT_MODULE) -> Artifact:
    cpu_filters = ctx.attrs.cpu_filters or CPU_FILTER_TO_ABI_DIRECTORY.keys()
    abis = [CPU_FILTER_TO_ABI_DIRECTORY[cpu] for cpu in cpu_filters]
    filter_tool = ctx.attrs._android_toolchain[AndroidToolchainInfo].filter_prebuilt_native_library_dir[RunInfo]
    native_libs_dirs = [native_lib.dir for native_lib in native_libs]
    native_libs_dirs_file = ctx.actions.write("{}_list.txt".format(identifier), native_libs_dirs)
    base_output_dir = ctx.actions.declare_output(identifier, dir = True)
    if module == ROOT_MODULE:
        output_dir = base_output_dir.project(_get_native_libs_as_assets_dir(module)) if package_as_assets else base_output_dir
    elif package_as_assets:
        output_dir = base_output_dir.project(paths.join(_get_native_libs_as_assets_dir(module), "assets"))
    else:
        output_dir = base_output_dir.project(paths.join(_get_native_libs_as_assets_dir(module), "lib"))
    ctx.actions.run(
        cmd_args([filter_tool, native_libs_dirs_file, output_dir.as_output(), "--abis"] + abis).hidden(native_libs_dirs),
        category = "filter_prebuilt_native_library_dir",
        identifier = identifier,
    )

    return base_output_dir

_StrippedNativeLinkables = record(
    linkables = Artifact,
    linkables_always_in_primary_apk = Artifact,
    linkable_assets_for_primary_apk = Artifact | None,
    linkable_module_assets_map = dict[str, Artifact],
)

def _get_native_linkables(
        ctx: AnalysisContext,
        platform_to_native_linkables: dict[str, dict[str, SharedLibrary]],
        get_module_from_target: typing.Callable,
        package_native_libs_as_assets_enabled: bool) -> _StrippedNativeLinkables:
    stripped_native_linkables_srcs = {}
    stripped_native_linkables_always_in_primary_apk_srcs = {}
    stripped_native_linkable_assets_for_primary_apk_srcs = {}
    stripped_native_linkable_module_assets_srcs = {}
    strip_libraries = getattr(ctx.attrs, "strip_libraries", True)

    cpu_filters = ctx.attrs.cpu_filters
    for platform, native_linkables in platform_to_native_linkables.items():
        if cpu_filters and platform not in cpu_filters and platform != CPU_FILTER_FOR_PRIMARY_PLATFORM:
            fail("Platform `{}` is not in the CPU filters `{}`".format(platform, cpu_filters))

        abi_directory = CPU_FILTER_TO_ABI_DIRECTORY[platform]
        for so_name, native_linkable in native_linkables.items():
            native_linkable_target = str(native_linkable.label.raw_target())
            module = get_module_from_target(native_linkable_target)
            lib = native_linkable.stripped_lib if strip_libraries else native_linkable.lib.output

            expect(
                not native_linkable.for_primary_apk or is_root_module(module),
                "{} which is marked as needing to be in the primary APK cannot be included in non-root-module {}".format(native_linkable_target, module),
            )
            expect(
                not native_linkable.for_primary_apk or not native_linkable.can_be_asset,
                "{} which is marked as needing to be in the primary APK cannot be an asset".format(native_linkable_target),
            )

            if is_root_module(module):
                if native_linkable.can_be_asset and package_native_libs_as_assets_enabled:
                    native_libs_assets_dir = paths.join(_get_native_libs_as_assets_dir(module))
                    so_name_path = paths.join(native_libs_assets_dir, abi_directory, so_name)
                    stripped_native_linkable_assets_for_primary_apk_srcs[so_name_path] = lib
                else:
                    so_name_path = paths.join(abi_directory, so_name)
                    if native_linkable.for_primary_apk:
                        stripped_native_linkables_always_in_primary_apk_srcs[so_name_path] = lib
                    else:
                        stripped_native_linkables_srcs[so_name_path] = lib
            else:
                module_dir = "assets" if native_linkable.can_be_asset else "lib"
                so_name_path = paths.join(_get_native_libs_as_assets_dir(module), module_dir, abi_directory, so_name)
                stripped_native_linkable_module_assets_srcs.setdefault(module, {})[so_name_path] = lib

    stripped_native_linkables = ctx.actions.symlinked_dir(
        "stripped_native_linkables",
        stripped_native_linkables_srcs,
    )
    stripped_native_linkables_always_in_primary_apk = ctx.actions.symlinked_dir(
        "stripped_native_linkables_always_in_primary_apk",
        stripped_native_linkables_always_in_primary_apk_srcs,
    )
    stripped_native_linkable_assets_for_primary_apk = ctx.actions.symlinked_dir(
        "stripped_native_linkables_assets_for_primary_apk",
        stripped_native_linkable_assets_for_primary_apk_srcs,
    ) if stripped_native_linkable_assets_for_primary_apk_srcs else None
    stripped_native_linkable_module_assets_map = {}
    for module, srcs in stripped_native_linkable_module_assets_srcs.items():
        stripped_native_linkable_module_assets_map[module] = ctx.actions.symlinked_dir(
            "stripped_native_linkable_assets_for_module_{}".format(module),
            srcs,
        )

    return _StrippedNativeLinkables(
        linkables = stripped_native_linkables,
        linkables_always_in_primary_apk = stripped_native_linkables_always_in_primary_apk,
        linkable_assets_for_primary_apk = stripped_native_linkable_assets_for_primary_apk,
        linkable_module_assets_map = stripped_native_linkable_module_assets_map,
    )

def _get_native_libs_as_assets_metadata(
        ctx: AnalysisContext,
        native_lib_assets: list[Artifact],
        module: str) -> Artifact:
    native_lib_assets_file = ctx.actions.write("{}/native_lib_assets".format(module), [cmd_args([native_lib_asset, _get_native_libs_as_assets_dir(module)], delimiter = "/") for native_lib_asset in native_lib_assets])
    metadata_output = ctx.actions.declare_output("{}/native_libs_as_assets_metadata.txt".format(module))
    metadata_cmd = cmd_args([
        ctx.attrs._android_toolchain[AndroidToolchainInfo].native_libs_as_assets_metadata[RunInfo],
        "--native-library-dirs",
        native_lib_assets_file,
        "--metadata-output",
        metadata_output.as_output(),
    ]).hidden(native_lib_assets)
    ctx.actions.run(metadata_cmd, category = "get_native_libs_as_assets_metadata", identifier = module)
    return metadata_output

def _get_native_libs_as_assets_dir(module: str) -> str:
    return "assets/{}".format("lib" if is_root_module(module) else module)

def get_default_shared_libs(ctx: AnalysisContext, deps: list[Dependency], shared_libraries_to_exclude) -> dict[str, SharedLibrary]:
    shared_library_info = merge_shared_libraries(
        ctx.actions,
        deps = filter(None, [x.get(SharedLibraryInfo) for x in deps]),
    )
    return {
        soname: shared_lib
        for soname, shared_lib in with_unique_str_sonames(traverse_shared_library_info(shared_library_info)).items()
        if not (shared_libraries_to_exclude and shared_libraries_to_exclude.contains(shared_lib.label.raw_target()))
    }

_LinkableSharedNode = record(
    raw_target = field(str),
    soname = field(str),
    labels = field(list[str], []),
    # Linkable deps of this target.
    deps = field(list[Label], []),
    can_be_asset = field(bool),
)

def encode_linkable_graph_for_mergemap(graph_node_map_by_platform: dict[str, dict[Label, LinkableNode]]) -> dict[str, dict[Label, _LinkableSharedNode]]:
    return {
        platform: {
            target: _LinkableSharedNode(
                raw_target = str(target.raw_target()),
                # FIXME(JakobDegen): The definition of `LinkableNode` claims that it's ok for this
                # to be `None` (I assume in the case of static preferred linkage), so either that is
                # wrong or this is. See the diff that added this FIXME for how to reproduce
                soname = node.default_soname,
                labels = node.labels,
                deps = node.deps + node.exported_deps,
                can_be_asset = node.can_be_asset,  # and not node.exclude_from_android_merge
            )
            for target, node in graph_node_map.items()
        }
        for platform, graph_node_map in graph_node_map_by_platform.items()
    }

# Debugging info about the linkables merge process. All of this will be written in one of the outputs of
# the `[native_merge_debug]` subtarget.
MergedLinkablesDebugInfo = record(
    unmerged_statics = list[str],
    group_debug = dict[str, typing.Any],
    with_default_soname = list[typing.Any],
    missing_default_solibs = list[Label],
)

# As shared lib output of the linkables merge process. This is not necessarily an actually merged node (there
# may be just one constituent)
MergedSharedLibrary = record(
    soname = str,
    lib = SharedLibrary,
    apk_module = str,
    # this only includes solib constituents that are included in the android merge map
    solib_constituents = list[str],
    is_actually_merged = bool,
    primary_constituents = list[Label],
)

# information about a link group derived from the merge mapping
LinkGroupData = record(
    group_name = [str, Label],
    constituents = list[Label],
    apk_module = str,
)

# Lookup key for somerge groups, either the soname for shared libraries or the target name for unmerged statics
GroupLabel = str

# Represents the primary constituents and deps of primary constituents used to create a LinkGroupLinkableNode for a non-prebuilt shared library.
LinkGroupMergeInfo = record(
    label = GroupLabel,
    deps = list[GroupLabel],
    exported_deps = list[GroupLabel],
    constituent_link_infos = list[LinkInfo],
)

# Represents a node in the final merged linkable map. Most of these will be shared libraries, either prebuilt shared libs or
# libraries that are created below for a node in the link_groups_graph. The exception is for non-merged static-only nodes.
LinkGroupLinkableNode = record(
    # The LinkInfo to add to the link line for a node that links against this.
    link = LinkInfo,
    deps = list[GroupLabel],
    exported_deps = list[GroupLabel],
    shared_lib = [SharedLibrary, None],
    # linker flags to be exported by any node that links against this. This can only be non-None for non-merged static only nodes (as we don't
    # propagate exported linker flags through transitive shared lib deps).
    exported_linker_flags = [(list[typing.Any], list[typing.Any]), None],
)

def write_merged_library_map(ctx: AnalysisContext, shared_libs_by_platform: dict[str, dict[str, MergedSharedLibrary]]) -> Artifact:
    """
    Writes the "merged library map". This is a map of original soname to final soname of the form:

    ```
    original_soname1 final_soname1
    original_soname2 final_soname1
    original_soname3 final_soname2
    ...
    ```
    """
    solib_map = {}  # dict[final_soname, set[original_soname]]
    for _, shared_libs in shared_libs_by_platform.items():
        for soname in shared_libs.keys():
            merged_shared_lib = shared_libs[soname]
            if merged_shared_lib.is_actually_merged:
                solib_map.setdefault(soname, set()).update(merged_shared_lib.solib_constituents)

    lines = []
    for final_soname in sorted(solib_map.keys()):
        for original_soname in solib_map[final_soname].list():
            lines.append("{} {}".format(original_soname, final_soname))

    # we wanted it sorted by original_soname
    return ctx.actions.write("merged_library_map.txt", sorted(lines))

def run_mergemap_codegen(ctx: AnalysisContext, merged_library_map: Artifact) -> Artifact:
    mapping_java = ctx.actions.declare_output("MergedLibraryMapping.java")
    args = cmd_args(ctx.attrs.native_library_merge_code_generator[RunInfo])
    args.add([merged_library_map, mapping_java.as_output()])
    ctx.actions.run(args, category = "mergemap_codegen")
    return mapping_java

# We can't merge a prebuilt shared (that has no archive) and must use it's original info.
# Ideally this would probably be structured info on the linkablenode.
def _is_prebuilt_shared(node_data: LinkableNode) -> bool:
    shared_link_info = node_data.link_infos.get(LibOutputStyle("shared_lib"), None)
    if not shared_link_info or not shared_link_info.default.linkables:
        return False
    pic_archive_info = node_data.link_infos.get(LibOutputStyle("pic_archive"), None)
    if not pic_archive_info or not pic_archive_info.default.linkables:
        return True
    return False

def _has_linkable(node_data: LinkableNode) -> bool:
    for _, output in node_data.link_infos.items():
        if output.default.linkables:
            return True
    return False

def _platform_output_path(path: str, platform: [str, None] = None):
    if platform:
        return platform + "/" + path
    return path

def _transitive_has_linkable(
        target: Label,
        linkable_nodes: dict[Label, LinkableNode],
        transitive_linkable_cache: dict[Label, bool]) -> bool:
    if target in transitive_linkable_cache:
        return transitive_linkable_cache[target]

    target_node = linkable_nodes.get(target)
    for dep in target_node.deps:
        if _has_linkable(linkable_nodes.get(dep)) or _transitive_has_linkable(dep, linkable_nodes, transitive_linkable_cache):
            transitive_linkable_cache[target] = True
            return True
    for dep in target_node.exported_deps:
        if _has_linkable(linkable_nodes.get(dep)) or _transitive_has_linkable(dep, linkable_nodes, transitive_linkable_cache):
            transitive_linkable_cache[target] = True
            return True

    transitive_linkable_cache[target] = False
    return False

def _shared_lib_for_prebuilt_shared(
        ctx: AnalysisContext,
        cxx_toolchain: CxxToolchainInfo,
        target: Label,
        node_data: LinkableNode,
        linkable_nodes: dict[Label, LinkableNode],
        transitive_linkable_cache: dict[Label, bool],
        platform: [str, None] = None) -> SharedLibrary:
    expect(
        len(node_data.shared_libs.libraries) == 1,
        "unexpected shared_libs length for somerge of {} ({})".format(target, node_data.shared_libs),
    )

    # TODO(cjhopman): We don't currently support prebuilt shared libs with deps on other libs because
    # we don't compute the shared lib deps of prebuilt shared libs here. That
    # shouldn't be too hard, but we haven't needed it.
    for dep in node_data.deps:
        expect(
            not _transitive_has_linkable(dep, linkable_nodes, transitive_linkable_cache),
            "prebuilt shared library `{}` with deps not supported by somerge".format(target),
        )
    for dep in node_data.exported_deps:
        expect(
            not _transitive_has_linkable(dep, linkable_nodes, transitive_linkable_cache),
            "prebuilt shared library `{}` with exported_deps not supported by somerge".format(target),
        )

    shlib = node_data.shared_libs.libraries[0]
    soname = shlib.soname.ensure_str()
    shlib = shlib.lib
    output_path = _platform_output_path(soname, platform)
    return create_shlib(
        lib = shlib,
        stripped_lib = strip_lib(ctx, cxx_toolchain, shlib.output, output_path),
        link_args = None,
        shlib_deps = None,
        can_be_asset = node_data.can_be_asset,
        for_primary_apk = False,
        soname = soname,
        label = target,
    )

def _get_merged_linkables_for_platform(
        ctx: AnalysisContext,
        cxx_toolchain: CxxToolchainInfo,
        platform: str | None,
        glue_linkable: [(Label, LinkInfo), None],
        default_shared_libs: dict[str, SharedLibrary],
        linkable_nodes: dict[Label, LinkableNode],
        merge_map: dict[str, [str, None]],
        merge_linker_args: dict[str, typing.Any],
        apk_module_graph: typing.Callable) -> (dict[str, MergedSharedLibrary], MergedLinkablesDebugInfo):
    """
    This takes the merge mapping and constructs the resulting merged shared libraries.

    This is similar to link_groups used by ios and cxx_binary, but is sufficiently different that it needs its
    own implementation. Potentially we could find a generalization of them that covers both.

    An overview of how this works:

    The input includes a mapping of target -> link group. This mapping does not need to be comprehensive, a target
    not in that mapping is assigned to its own link group. The input also contains a mapping of target -> apk module,
    and this must have a mapping for every target. A link group cannot contain targets in different apk modules.

    Once we have the mapping of link groups, we construct an analog of the LinkableGraph on the new link group graph.
    This new graph consists of LinkGroupLinkableNodes (in some sense, the merged equivalent of LinkableNode).
    We traverse this graph from the bottom up, producing link info for each node.

    First, there are some special cases:

    If a target is "not actually merged" (i.e. it's in a link group where it is the only constituent) then it may get
    special handling if:
        (1) it is lib with static preferred linkage or it contains no linkables (i.e. no code)
        (2) a prebuild shared lib
    For both these cases we will produce a LinkGroupLinkableNode that basically reuses their original information. For
    every other case we will produce a node that represents a shared library that we define.

    When constructing a LinkableNode for a link group, we will be traversing sort of a hybrid graph, as we will traverse
    the primary constituents of the link group itself in the original LinkableGraph, but dependencies outside of the link
    group will be mapped to the corresponding LinkGroupLinkableNode (and potentially then further traversing that node's
    deps).

    The merge mapping input determines the "primary constituents" of each link group. The "real constituents" of that link
    group will include all those primary constituents, all of the transitive non-shared lib deps in the linkgroup linkable
    graph, and then all the shared lib dependencies of all of them (that is describing a traversal similar to the
    normal link strategies).

    There are some subtle differences in the handling of primary constituents and the statically linked non-primary
    constituents:
    1. all primary constituents are public nodes, non primary ones are only public if they are transitively exported_deps
    of a primary constituent. A public node is linked via "link whole".
    2. linker_flags of primary constituents are included in the link, for non primary they are not
    """
    debug_info = MergedLinkablesDebugInfo(
        unmerged_statics = [],
        group_debug = {},
        with_default_soname = [],
        missing_default_solibs = [],
    )

    linkable_nodes_graph = {k: dedupe(v.deps + v.exported_deps) for k, v in linkable_nodes.items()}
    topo_sorted_targets = pre_order_traversal(linkable_nodes_graph)

    # first we collect basic information about each link group, this will populate the fields in LinkGroupData and
    # map target labels to their link group name.
    link_groups = {}
    target_to_link_group = {}

    # Because we cannot attach this to the LinkableNode after the fact, declare a cache for each platform
    transitive_linkable_cache = {}

    for target in topo_sorted_targets:
        expect(target not in target_to_link_group, "prelude internal error, target seen twice?")
        target_apk_module = apk_module_graph(str(target.raw_target()))

        link_group = merge_map.get(str(target), None)
        if not link_group:
            link_group = str(target)
            link_groups[link_group] = LinkGroupData(
                group_name = target,
                constituents = [target],
                apk_module = target_apk_module,
            )
        elif link_group in link_groups:
            link_group_data = link_groups[link_group]

            # TODO(cjhopman): buck1 provides a more useful error here in that it lists the module mappings for all
            # constituents of the merge group (rather than just one conflict). That allows users to resolve all the
            # issues at once. With merge sequence merging (the replacement for merge map), this error shouldn't ever be hit
            # and so maybe it's not necessary to improve it.
            expect(
                link_group_data.apk_module == target_apk_module,
                "Native library merge of {} has inconsistent application module mappings:\n{} is in module {}\n{} is in module {}",
                link_group_data.group_name,
                target,
                target_apk_module,
                link_group_data.constituents[0],
                link_group_data.apk_module,
            )
            link_groups[link_group].constituents.append(target)
        else:
            link_groups[link_group] = LinkGroupData(
                group_name = link_group,
                constituents = [target],
                apk_module = target_apk_module,
            )

        target_to_link_group[target] = link_group

    # Now that all targets are assigned to a link group, build up the link group graph.
    link_groups_graph_builder = {}
    for target in topo_sorted_targets:
        target_group = target_to_link_group[target]
        group_deps = link_groups_graph_builder.setdefault(target_group, {})
        for dep in linkable_nodes_graph[target]:
            dep_group = target_to_link_group[dep]
            if target_group != dep_group:
                group_deps[dep_group] = True
    link_groups_graph = {k: list(v.keys()) for k, v in link_groups_graph_builder.items()}

    archive_output_style = LibOutputStyle("pic_archive")
    shlib_output_style = LibOutputStyle("shared_lib")

    link_group_linkable_nodes = {}
    group_shared_libs = {}
    included_default_solibs = {}

    # Now we will traverse from the leaves up the graph (the link groups graph). As we traverse, we will produce
    # a link group linkablenode for each group.
    for group in post_order_traversal(link_groups_graph):
        group_data = link_groups[group]
        is_actually_merged = len(group_data.constituents) > 1

        can_be_asset = True
        for target in group_data.constituents:
            if not linkable_nodes[target].can_be_asset:
                can_be_asset = False
                break

        if not is_actually_merged:
            target = group_data.constituents[0]
            node_data = linkable_nodes[target]

            if node_data.preferred_linkage == Linkage("static") or not _has_linkable(node_data):
                debug_info.unmerged_statics.append(target)
                link_group_linkable_nodes[group] = LinkGroupLinkableNode(
                    link = node_data.link_infos[archive_output_style].default,
                    deps = dedupe_by_value([target_to_link_group[t] for t in node_data.deps]),
                    exported_deps = dedupe_by_value([target_to_link_group[t] for t in node_data.exported_deps]),
                    shared_lib = None,
                    exported_linker_flags = (node_data.linker_flags.exported_flags, node_data.linker_flags.exported_post_flags),
                )
                continue

            if _is_prebuilt_shared(node_data):
                shared_lib = _shared_lib_for_prebuilt_shared(
                    ctx,
                    cxx_toolchain,
                    target,
                    node_data,
                    linkable_nodes,
                    transitive_linkable_cache,
                    platform,
                )
                link_group_linkable_nodes[group] = LinkGroupLinkableNode(
                    link = node_data.link_infos[shlib_output_style].default,
                    deps = [],
                    exported_deps = [],
                    shared_lib = shared_lib,
                    # exported linker flags for shared libs are in their linkinfo itself and are not exported from dependents
                    exported_linker_flags = None,
                )
                group_shared_libs[shared_lib.soname.ensure_str()] = MergedSharedLibrary(
                    soname = shared_lib.soname.ensure_str(),
                    lib = shared_lib,
                    apk_module = group_data.apk_module,
                    solib_constituents = [],
                    is_actually_merged = False,
                    primary_constituents = [target],
                )
                continue

        exported_linker_flags = []
        exported_linker_post_flags = []
        links = []

        if is_actually_merged and glue_linkable:
            links.append(set_link_info_link_whole(glue_linkable[1]))

        solib_constituents = []
        group_deps = []
        group_exported_deps = []
        for key in group_data.constituents:
            expect(target_to_link_group[key] == group)
            node = linkable_nodes[key]

            default_solibs = list([shlib.soname.ensure_str() for shlib in node.shared_libs.libraries])
            if not default_solibs and node.preferred_linkage == Linkage("static"):
                default_solibs = [node.default_soname]

            for soname in default_solibs:
                included_default_solibs[soname] = True
                if node.include_in_android_mergemap:
                    solib_constituents.append(soname)

            node = linkable_nodes[key]
            link_info = node.link_infos[archive_output_style].default

            # the propagated link info should already be wrapped with exported flags.
            link_info = wrap_link_info(
                link_info,
                pre_flags = node.linker_flags.flags,
                post_flags = node.linker_flags.post_flags,
            )
            exported_linker_flags.extend(node.linker_flags.exported_flags)
            exported_linker_post_flags.extend(node.linker_flags.exported_post_flags)
            links.append(set_link_info_link_whole(link_info))

            dep_groups = [target_to_link_group[dep] for dep in node.deps]
            group_deps.extend([dep_group for dep_group in dep_groups if dep_group != group])

            exported_dep_groups = [target_to_link_group[dep] for dep in node.exported_deps]
            group_exported_deps.extend([dep_group for dep_group in exported_dep_groups if dep_group != group])

        soname = group
        if not is_actually_merged:
            soname = linkable_nodes[group_data.constituents[0]].default_soname
            debug_info.with_default_soname.append((soname, group_data.constituents[0]))

        output_path = _platform_output_path(soname, platform)

        link_merge_info = LinkGroupMergeInfo(
            label = group,
            deps = dedupe_by_value(group_deps),
            exported_deps = dedupe_by_value(group_exported_deps),
            constituent_link_infos = links,
        )
        link_args, shlib_deps, link_deps_graph = _create_merged_link_args(
            root_target = link_merge_info,
            linkable_nodes = link_group_linkable_nodes,
            cxx_toolchain = cxx_toolchain,
        )
        link_args = [link_args]
        if soname in merge_linker_args:
            link_args += [LinkArgs(infos = [LinkInfo(pre_flags = merge_linker_args[soname])])]

        shared_lib = create_shared_lib(
            ctx,
            output_path = output_path,
            soname = soname,
            link_args = link_args,
            cxx_toolchain = cxx_toolchain,
            shared_lib_deps = [link_group_linkable_nodes[label].shared_lib.soname.ensure_str() for label in shlib_deps],
            label = group_data.constituents[0],
            can_be_asset = can_be_asset,
        )

        link_group_linkable_nodes[group] = LinkGroupLinkableNode(
            link = LinkInfo(
                name = soname,
                pre_flags = exported_linker_flags,
                linkables = [SharedLibLinkable(
                    lib = shared_lib.lib.output,
                )],
                post_flags = exported_linker_post_flags,
            ),
            deps = link_merge_info.deps,
            exported_deps = link_merge_info.exported_deps,
            shared_lib = shared_lib,
            # exported linker flags for shared libs are in their linkinfo itself and are not exported from dependents
            exported_linker_flags = None,
        )
        group_shared_libs[soname] = MergedSharedLibrary(
            soname = soname,
            lib = shared_lib,
            apk_module = group_data.apk_module,
            solib_constituents = solib_constituents,
            is_actually_merged = is_actually_merged,
            primary_constituents = group_data.constituents,
        )

        debug_info.group_debug.setdefault(
            group,
            struct(
                soname = soname,
                merged = is_actually_merged,
                primary_constituents = group_data.constituents,
                real_constituents = link_deps_graph.keys(),
                shlib_deps = shlib_deps,
                exported_linker_flags = exported_linker_flags,
                exported_linker_post_flags = exported_linker_post_flags,
            ),
        )

    debug_info.missing_default_solibs.extend([d for d in default_shared_libs if d not in included_default_solibs])

    return group_shared_libs, debug_info

# The current default link strategies don't produce enough information in the
# SharedLibrary objects to perform relinking. To do that best, linking should be based on
# the LinkableGraph rather than the current approach with MergedLinkInfo.
# The overall plan for linking is to move to linkable graph-based linking, but for now
# we can do it just for the case that we need it.
def _create_all_relinkable_links(
        ctx: AnalysisContext,
        platform_to_original_native_linkables: dict[str, dict[str, SharedLibrary]],
        graph_node_map_by_platform: dict[str, dict[Label, LinkableNode]]) -> (dict[str, dict[str, SharedLibrary]], dict[str, typing.Any]):
    final_platform_to_native_linkables = {}
    link_graphs_by_platform = {}
    for platform in platform_to_original_native_linkables:
        linkables, link_graphs = _create_relinkable_links(
            ctx,
            cxx_toolchain = ctx.attrs._cxx_toolchain[platform][CxxToolchainInfo],
            linkable_nodes = graph_node_map_by_platform[platform],
            platform = platform,
        )
        link_graphs_by_platform[platform] = link_graphs
        final_platform_to_native_linkables[platform] = linkables

    # sanity check that we produce the same list of linkables that are produced by standard linking.
    original_sonames = sorted(platform_to_original_native_linkables.keys())
    final_sonames = sorted(final_platform_to_native_linkables.keys())
    expect(original_sonames == final_sonames, "Unexpected differences in final sonames! {} {}".format(original_sonames, final_sonames))

    debug_outputs = {}

    # The biggest issue we could run into here is that we produce different link args than the original, so let's make that easy to debug.
    for platform in platform_to_original_native_linkables:
        for soname, lib in platform_to_original_native_linkables[platform].items():
            final = final_platform_to_native_linkables[platform][soname]
            original_args, _ = ctx.actions.write(
                "{}/{}/original.args".format(platform, soname),
                [unpack_link_args(args, LinkOrdering("topological")) for args in lib.link_args] if lib.link_args else "",
                allow_args = True,
            )
            final_args, _ = ctx.actions.write(
                "{}/{}/final.args".format(platform, soname),
                [unpack_link_args(args, LinkOrdering("topological")) for args in final.link_args] if final.link_args else "",
                allow_args = True,
            )
            debug_outputs["{}/{}/original.args".format(platform, soname)] = original_args
            debug_outputs["{}/{}/final.args".format(platform, soname)] = final_args

            if lib.label in link_graphs_by_platform[platform]:
                link_graph = ctx.actions.write_json(
                    "{}/{}/link.graph".format(platform, soname),
                    link_graphs_by_platform[platform][lib.label],
                    pretty = True,
                )
                debug_outputs["{}/{}/link.graph".format(platform, soname)] = link_graph

            # TODO(cjhopman): should we also just produce a diff here? We could also consider creating sort of a merged diff or a list
            # of the differing argsfiles.
            # We can't compare them eagerly because the link args have large tsets that we don't want to traverse at analysis time.

    return final_platform_to_native_linkables, debug_outputs

def _create_relinkable_links(
        ctx: AnalysisContext,
        *,
        cxx_toolchain: CxxToolchainInfo,
        linkable_nodes: dict[Label, LinkableNode],
        platform: str) -> (dict[str, SharedLibrary], dict[Label, dict[Label, list[Label]]]):
    linkable_nodes_graph = {target: value.deps + value.exported_deps for target, value in linkable_nodes.items()}
    shared_libs = {}
    shared_lib_overrides = {}
    transitive_linkable_cache = {}
    debug_link_deps = {}
    for target in post_order_traversal(linkable_nodes_graph):
        node = linkable_nodes[target]
        if node.preferred_linkage == Linkage("static") or not _has_linkable(node):
            continue

        if _is_prebuilt_shared(node):
            shared_lib = _shared_lib_for_prebuilt_shared(ctx, cxx_toolchain, target, node, linkable_nodes, transitive_linkable_cache, platform)
        else:
            soname = node.default_soname
            output_path = "relinkable-libs/{}/{}".format(platform, soname)
            link_args, shlib_deps, link_deps_graph = _create_link_args(
                cxx_toolchain = cxx_toolchain,
                root_target = target,
                node = node,
                graph = linkable_nodes,
                shared_lib_overrides = shared_lib_overrides,
            )
            debug_link_deps[target] = link_deps_graph
            shared_lib = create_shared_lib(
                ctx,
                output_path = output_path,
                soname = soname,
                link_args = [link_args],
                cxx_toolchain = cxx_toolchain,
                shared_lib_deps = [shared_lib_overrides[lib].name for lib in shlib_deps if lib in shared_lib_overrides],
                label = target,
                can_be_asset = node.can_be_asset,
            )
        shared_lib_overrides[target] = LinkInfo(
            name = shared_lib.soname.ensure_str(),
            pre_flags = node.linker_flags.exported_flags,
            linkables = [SharedLibLinkable(
                lib = shared_lib.lib.output,
            )],
            post_flags = node.linker_flags.exported_post_flags,
        )
        shared_libs[shared_lib.soname.ensure_str()] = shared_lib

    return {lib.soname.ensure_str(): lib for lib in shared_libs.values()}, debug_link_deps

# To support migration from a tset-based link strategy, we are trying to match buck's internal tset
# traversal logic here.  Look for implementation of TopologicalTransitiveSetIteratorGen
def _rust_matching_topological_traversal(
        roots: list[typing.Any],
        get_nodes_to_traverse_func: typing.Callable) -> list[typing.Any]:
    counts = {}

    for label in breadth_first_traversal_by(None, roots, get_nodes_to_traverse_func):
        for dep in get_nodes_to_traverse_func(label):
            if dep in counts:
                counts[dep] += 1
            else:
                counts[dep] = 1

    # some of the targets in roots might be transitive deps of others, we only put those that are true roots
    # in the stack at this point
    stack = [root_target for root_target in roots if not root_target in counts]
    true_roots = len(stack)

    result = []
    for _ in range(2000000000):
        if not stack:
            break
        next = stack.pop()
        result.append(next)
        deps = get_nodes_to_traverse_func(next)
        for child in deps[::-1]:  # reverse order ensures we put things on the stack in the same order as rust's tset traversal
            counts[child] -= 1
            if counts[child] == 0:
                stack.append(child)

    if len(result) != true_roots + len(counts):
        fail()  # fail_cycle

    return result

def _create_link_args(
        *,
        cxx_toolchain: CxxToolchainInfo,
        root_target: Label,
        node: LinkableNode,
        graph: dict[Label, LinkableNode],
        shared_lib_overrides: dict[Label, LinkInfo] | None = None) -> (LinkArgs, list[Label], dict[Label, list[Label]]):
    if LinkOrdering(cxx_toolchain.linker_info.link_ordering) != LinkOrdering("topological"):
        fail("don't yet support link ordering {}".format(cxx_toolchain.linker_info.link_ordering))

    # TODO(cjhopman): verify picbehavior == pic
    link_strategy = node.default_link_strategy
    if not shared_lib_overrides:
        shared_lib_overrides = {}

    link_traversal_cache = {}

    def link_traversal(label: Label) -> list[Label]:
        def link_traversal_deps(label):
            node = graph[label]
            if label == root_target:
                return node.deps + node.exported_deps
            actual_linkable_type = get_lib_output_style(link_strategy, node.preferred_linkage, PicBehavior("supported"))
            if actual_linkable_type == LibOutputStyle("shared_lib"):
                return node.exported_deps
            else:
                return node.deps + node.exported_deps

        res = link_traversal_cache.get(label, None)
        if res:
            return res
        res = link_traversal_deps(label)
        link_traversal_cache[label] = res
        return res

    links = []
    shlib_deps = []
    for target in _rust_matching_topological_traversal([root_target], link_traversal):
        is_root = target == root_target
        node = graph[target]
        preferred_linkable_type = get_lib_output_style(link_strategy, node.preferred_linkage, PicBehavior("supported"))

        if is_root:
            link_info = node.link_infos[LibOutputStyle("pic_archive")].default
            link_info = wrap_link_info(
                link_info,
                pre_flags = node.linker_flags.flags,
                post_flags = node.linker_flags.post_flags,
            )
            links.append(set_link_info_link_whole(link_info))
        elif preferred_linkable_type == LibOutputStyle("shared_lib"):
            if target in shared_lib_overrides:
                links.append(shared_lib_overrides[target])
            else:
                links.append(node.link_infos[LibOutputStyle("shared_lib")].default)
            shlib_deps.append(target)
        else:
            links.append(node.link_infos[preferred_linkable_type].default)

    extra_runtime_flags = cxx_toolchain.linker_info.shared_dep_runtime_ld_flags or []
    if extra_runtime_flags:
        links.append(LinkInfo(pre_flags = extra_runtime_flags))
    return LinkArgs(infos = links), shlib_deps, link_traversal_cache

# Equivalent to _create_link_args but for somerge
def _create_merged_link_args(
        *,
        cxx_toolchain: CxxToolchainInfo,
        root_target: LinkGroupMergeInfo,
        linkable_nodes: dict[GroupLabel, LinkGroupLinkableNode]) -> (LinkArgs, list[GroupLabel], dict[GroupLabel, list[GroupLabel]]):
    if LinkOrdering(cxx_toolchain.linker_info.link_ordering) != LinkOrdering("topological"):
        fail("don't yet support link ordering {}".format(cxx_toolchain.linker_info.link_ordering))

    link_traversal_cache = {}

    def link_traversal(label: GroupLabel) -> list[GroupLabel]:
        def link_traversal_deps(label: GroupLabel):
            if label == root_target.label:
                return root_target.deps + root_target.exported_deps

            linkable_node = linkable_nodes[label]
            if linkable_node.shared_lib:
                return linkable_node.exported_deps
            else:
                return linkable_node.deps + linkable_node.exported_deps

        res = link_traversal_cache.get(label, None)
        if res:
            return res
        res = link_traversal_deps(label)
        link_traversal_cache[label] = res
        return res

    links = []
    shlib_deps = []
    for label in _rust_matching_topological_traversal([root_target.label], link_traversal):
        if label == root_target.label:
            links.extend(root_target.constituent_link_infos)
        else:
            linkable_node = linkable_nodes[label]
            links.append(linkable_node.link)
            if linkable_node.shared_lib:
                shlib_deps.append(label)

    extra_runtime_flags = cxx_toolchain.linker_info.shared_dep_runtime_ld_flags or []
    if extra_runtime_flags:
        links.append(LinkInfo(pre_flags = extra_runtime_flags))
    return LinkArgs(infos = links), shlib_deps, link_traversal_cache

# When linking shared libraries, by default, all symbols are exported from the library. In a
# particular application, though, many of those symbols may never be used. Ideally, in each apk,
# each shared library would only export the minimal set of symbols that are used by other libraries
# in the apk. This would allow the linker to remove any dead code within the library (the linker
# can strip all code that is unreachable from the set of exported symbols).
#
# The native relinker tries to remedy the situation. When enabled for an apk, the native
# relinker will take the set of libraries in the apk and relink them in reverse order telling the
# linker to only export those symbols that are referenced by a higher library.
#
# The way this works is that the relinker does a topological traversal of the linked libraries (i.e.
# top-down, visiting nodes before their dependencies, this is the opposite order of most things we do
# in a build) and does:
# 1. extract the set of global symbols by the original lib
# 2. intersect that with the set of undefined symbols in all transitive dependents (i.e. higher in the graph) and
#    rules for required symbols (Java_*, Jni_Onload, relinker blocklist)
# 3. write a version script that says to make public only those symbols from (2)
# 4. link the lib with the exact same link line + the version script. Note that this means that the relinked libraries each are
#    actually linked against non-relinked ones. This does mean there's some risk of not detecting missing symbols (though mostly
#    only if they are caused by the relinker changes themselves).
# 5. extract the list of undefined symbols in the relinked libs (i.e. those symbols needed from dependencies and what had been
#    used in (1) above from higher nodes).
def relink_libraries(ctx: AnalysisContext, libraries_by_platform: dict[str, dict[str, SharedLibrary]]) -> dict[str, dict[str, SharedLibrary]]:
    relinked_libraries_by_platform = {}
    for platform, shared_libraries in libraries_by_platform.items():
        cxx_toolchain = ctx.attrs._cxx_toolchain[platform][CxxToolchainInfo]

        relinked_libraries = relinked_libraries_by_platform.setdefault(platform, {})
        unsupported_libs = {}
        shlib_graph = {}
        rev_shlib_graph = {}
        for soname, solib in shared_libraries.items():
            shlib_graph[soname] = []
            rev_shlib_graph.setdefault(soname, [])
            if solib.shlib_deps == None or solib.link_args == None:
                unsupported_libs[soname] = True
            else:
                for dep in solib.shlib_deps:
                    shlib_graph[soname].append(dep)
                    rev_shlib_graph.setdefault(dep, []).append(soname)
        needed_symbols_files = {}
        for soname in pre_order_traversal(shlib_graph):
            if soname in unsupported_libs:
                relinked_libraries[soname] = shared_libraries[soname]
                continue

            original_shared_library = shared_libraries[soname]
            output_path = "xdso-dce-relinker-libs/{}/{}".format(platform, soname)

            provided_symbols_file = extract_provided_symbols(ctx, cxx_toolchain, original_shared_library.lib.output)
            needed_symbols_for_this = [needed_symbols_files.get(rdep) for rdep in rev_shlib_graph[soname]]
            relinker_version_script = ctx.actions.declare_output(output_path + ".relinker.version_script")
            create_relinker_version_script(
                ctx.actions,
                output = relinker_version_script,
                relinker_allowlist = [regex(s) for s in ctx.attrs.relinker_whitelist],
                provided_symbols = provided_symbols_file,
                needed_symbols = needed_symbols_for_this,
            )
            relinker_link_args = original_shared_library.link_args + [LinkArgs(flags = [cmd_args(relinker_version_script, format = "-Wl,--version-script={}")])]

            shared_lib = create_shared_lib(
                ctx,
                output_path = output_path,
                soname = soname,
                link_args = relinker_link_args,
                cxx_toolchain = cxx_toolchain,
                shared_lib_deps = original_shared_library.shlib_deps,
                label = original_shared_library.label,
                can_be_asset = original_shared_library.can_be_asset,
            )
            needed_symbols_from_this = extract_undefined_symbols(ctx, cxx_toolchain, shared_lib.lib.output)
            unioned_needed_symbols_file = ctx.actions.declare_output(output_path + ".all_needed_symbols")
            union_needed_symbols(ctx.actions, unioned_needed_symbols_file, needed_symbols_for_this + [needed_symbols_from_this])
            needed_symbols_files[soname] = unioned_needed_symbols_file

            relinked_libraries[soname] = shared_lib

    return relinked_libraries_by_platform

def extract_provided_symbols(ctx: AnalysisContext, toolchain: CxxToolchainInfo, lib: Artifact) -> Artifact:
    return extract_global_syms(ctx, toolchain, lib, "relinker_extract_provided_symbols")

def create_relinker_version_script(actions: AnalysisActions, relinker_allowlist: list[regex], output: Artifact, provided_symbols: Artifact, needed_symbols: list[Artifact]):
    def create_version_script(ctx, artifacts, outputs):
        all_needed_symbols = {}
        for symbols_file in needed_symbols:
            for line in artifacts[symbols_file].read_string().strip().split("\n"):
                all_needed_symbols[line] = True

        symbols_to_keep = []
        for symbol in artifacts[provided_symbols].read_string().strip().split("\n"):
            keep_symbol = False
            if symbol in all_needed_symbols:
                keep_symbol = True
            elif "JNI_OnLoad" in symbol:
                keep_symbol = True
            elif "Java_" in symbol:
                keep_symbol = True
            else:
                for pattern in relinker_allowlist:
                    if pattern.match(symbol):
                        keep_symbol = True
                        break

            if keep_symbol:
                symbols_to_keep.append(symbol)

        version_script = "{\n"
        if symbols_to_keep:
            version_script += "global:\n"
        for symbol in symbols_to_keep:
            version_script += "  {};\n".format(symbol)
        version_script += "local: *;\n"
        version_script += "};\n"
        ctx.actions.write(outputs[output], version_script)

    actions.dynamic_output(dynamic = needed_symbols + [provided_symbols], inputs = [], outputs = [output.as_output()], f = create_version_script)

def extract_undefined_symbols(ctx: AnalysisContext, toolchain: CxxToolchainInfo, lib: Artifact) -> Artifact:
    return extract_undefined_syms(ctx, toolchain, lib, "relinker_extract_undefined_symbols")

def union_needed_symbols(actions: AnalysisActions, output: Artifact, needed_symbols: list[Artifact]):
    def compute_union(ctx, artifacts, outputs):
        unioned_symbols = {}
        for symbols_file in needed_symbols:
            for line in artifacts[symbols_file].read_string().strip().split("\n"):
                unioned_symbols[line] = True
        symbols = sorted(unioned_symbols.keys())
        ctx.actions.write(outputs[output], symbols)

    actions.dynamic_output(dynamic = needed_symbols, inputs = [], outputs = [output.as_output()], f = compute_union)

def strip_lib(ctx: AnalysisContext, cxx_toolchain: CxxToolchainInfo, shlib: Artifact, output_path: [str, None] = None):
    strip_flags = cmd_args(get_strip_non_global_flags(cxx_toolchain))
    return strip_object(
        ctx,
        cxx_toolchain,
        shlib,
        strip_flags,
        output_path = output_path,
    )

def create_shared_lib(
        ctx: AnalysisContext,
        *,
        output_path: str,
        soname: str,
        link_args: list[LinkArgs],
        cxx_toolchain: CxxToolchainInfo,
        shared_lib_deps: list[str],
        label: Label,
        can_be_asset: bool) -> SharedLibrary:
    for link_arg in link_args:
        flags = link_arg.flags or []
        for info in link_arg.infos or []:
            flags += info.pre_flags or []
            flags += info.post_flags or []
        for flag in flags:
            flag = str(flag)
            if flag.endswith("--exclude-libs,ALL") or flag.endswith("--exclude-libs=ALL"):
                fail("The behavior of --exclude-libs,ALL is not predictable when building Android binaries and may cause runtime crashes, remove it from {} (or its merged constituents)".format(label))
    link_result = cxx_link_shared_library(
        ctx = ctx,
        output = output_path,
        name = soname,
        opts = link_options(
            links = link_args,
            link_execution_preference = LinkExecutionPreference("any"),
            identifier = output_path,
            strip = False,
            cxx_toolchain = cxx_toolchain,
        ),
    )

    shlib = link_result.linked_object
    return create_shlib(
        lib = shlib,
        stripped_lib = strip_lib(ctx, cxx_toolchain, shlib.output),
        shlib_deps = shared_lib_deps,
        link_args = link_args,
        can_be_asset = can_be_asset,
        for_primary_apk = False,
        soname = soname,
        label = label,
    )
