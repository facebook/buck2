# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:artifacts.bzl", "single_artifact")
load("@prelude//:paths.bzl", "paths")
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load(
    "@prelude//linking:link_info.bzl",
    "CxxSanitizerRuntimeInfo",
)
load("@prelude//utils:utils.bzl", "flatten_dict")
load(
    ":apple_asset_catalog.bzl",
    "compile_apple_asset_catalog",
)
load(
    ":apple_asset_catalog_types.bzl",
    "AppleAssetCatalogSpec",  # @unused Used as a type
)
load(":apple_bundle_destination.bzl", "AppleBundleDestination")
load(":apple_bundle_part.bzl", "AppleBundlePart")
load(":apple_bundle_types.bzl", "AppleBundleInfo", "AppleBundleTypeAppClip", "AppleBundleTypeDefault", "AppleBundleTypeWatchApp")
load(":apple_bundle_utility.bzl", "get_bundle_resource_processing_options", "get_extension_attr", "get_product_name")
load(":apple_core_data.bzl", "compile_apple_core_data")
load(
    ":apple_core_data_types.bzl",
    "AppleCoreDataSpec",  # @unused Used as a type
)
load(":apple_info_plist.bzl", "process_info_plist", "process_plist")
load(":apple_library.bzl", "AppleLibraryInfo")
load(
    ":apple_resource_types.bzl",
    "AppleResourceDestination",
    "AppleResourceSpec",  # @unused Used as a type
    "CxxResourceSpec",  # @unused Used as a type
)
load(":apple_resource_utility.bzl", "apple_bundle_destination_from_resource_destination")
load(
    ":resource_groups.bzl",
    "create_resource_graph",
    "get_filtered_resources",
    "get_resource_graph_node_map_func",
    "get_resource_group_info",
)
load(":scene_kit_assets.bzl", "compile_scene_kit_assets")
load(
    ":scene_kit_assets_types.bzl",
    "SceneKitAssetsSpec",  # @unused Used as a type
)

AppleBundleResourcePartListOutput = record(
    # Resource parts to be copied into an Apple bundle, *excluding* binaries
    resource_parts = field(list[AppleBundlePart]),
    # Part that holds the info.plist
    info_plist_part = field(AppleBundlePart),
)

def get_apple_bundle_resource_part_list(ctx: AnalysisContext) -> AppleBundleResourcePartListOutput:
    parts = []

    parts.extend(_create_pkg_info_if_needed(ctx))
    parts.extend(_copy_privacy_manifest_if_needed(ctx))

    (resource_specs, asset_catalog_specs, core_data_specs, scene_kit_assets_spec, cxx_resource_specs) = _select_resources(ctx)

    # If we've pulled in native/C++ resources from deps, inline them into the
    # bundle under the `CxxResources` namespace.
    cxx_resources = flatten_dict([s.resources for s in cxx_resource_specs])
    if cxx_resources:
        cxx_res_dir = ctx.actions.copied_dir(
            "CxxResources",
            {
                name: resource.default_output
                for name, resource in cxx_resources.items()
            },
        )
        resource_specs.append(
            AppleResourceSpec(
                dirs = [cxx_res_dir],
                destination = AppleResourceDestination("resources"),
            ),
        )

    cxx_sanitizer_runtime_info = ctx.attrs.binary.get(CxxSanitizerRuntimeInfo) if ctx.attrs.binary else None
    if cxx_sanitizer_runtime_info:
        runtime_resource_spec = AppleResourceSpec(
            files = cxx_sanitizer_runtime_info.runtime_files,
            destination = AppleResourceDestination("frameworks"),
            # Sanitizer dylibs require signing, for hardened runtime on macOS and iOS device builds
            codesign_files_on_copy = True,
        )
        resource_specs.append(runtime_resource_spec)

    asset_catalog_result = compile_apple_asset_catalog(ctx, asset_catalog_specs)
    if asset_catalog_result != None:
        asset_catalog_part = AppleBundlePart(
            source = asset_catalog_result.compiled_catalog,
            destination = AppleBundleDestination("resources"),
            # We only interested in directory contents
            new_name = "",
        )
        parts.append(asset_catalog_part)

    extra_plist = asset_catalog_result.catalog_plist if asset_catalog_result != None else None
    info_plist_part = process_info_plist(ctx = ctx, override_input = extra_plist)

    core_data_result = compile_apple_core_data(ctx, core_data_specs, get_product_name(ctx))
    if core_data_result != None:
        core_data_part = AppleBundlePart(
            source = core_data_result,
            destination = AppleBundleDestination("resources"),
            # We only interested in directory contents
            new_name = "",
        )
        parts.append(core_data_part)

    scene_kit_assets_result = compile_scene_kit_assets(ctx, scene_kit_assets_spec)
    if scene_kit_assets_result != None:
        scene_kit_assets_part = AppleBundlePart(
            source = scene_kit_assets_result,
            destination = AppleBundleDestination("resources"),
            # We only interested in directory contents
            new_name = "",
        )
        parts.append(scene_kit_assets_part)

    parts.extend(_copy_resources(ctx, resource_specs))
    parts.extend(_copy_first_level_bundles(ctx))
    parts.extend(_copy_public_headers(ctx))
    parts.extend(_copy_module_map(ctx))

    return AppleBundleResourcePartListOutput(
        resource_parts = parts,
        info_plist_part = info_plist_part,
    )

# Same logic as in v1, see `buck_client/src/com/facebook/buck/apple/ApplePkgInfo.java`
def _create_pkg_info_if_needed(ctx: AnalysisContext) -> list[AppleBundlePart]:
    extension = get_extension_attr(ctx)
    if extension == "xpc" or extension == "qlgenerator":
        return []
    artifact = ctx.actions.write("PkgInfo", "APPLWRUN\n")
    return [AppleBundlePart(source = artifact, destination = AppleBundleDestination("metadata"))]

def _copy_privacy_manifest_if_needed(ctx: AnalysisContext) -> list[AppleBundlePart]:
    privacy_manifest = ctx.attrs.privacy_manifest
    if privacy_manifest == None:
        return []

    # According to apple docs, privacy manifest has to be named as `PrivacyInfo.xcprivacy`
    if privacy_manifest.short_path.split("/", 1)[-1] == "PrivacyInfo.xcprivacy":
        artifact = privacy_manifest
    else:
        output = ctx.actions.declare_output("PrivacyInfo.xcprivacy")
        artifact = ctx.actions.copy_file(output.as_output(), privacy_manifest)
    return [AppleBundlePart(source = artifact, destination = AppleBundleDestination("metadata"))]

def _select_resources(ctx: AnalysisContext) -> ((list[AppleResourceSpec], list[AppleAssetCatalogSpec], list[AppleCoreDataSpec], list[SceneKitAssetsSpec], list[CxxResourceSpec])):
    resource_group_info = get_resource_group_info(ctx)
    if resource_group_info:
        resource_groups_deps = resource_group_info.implicit_deps
        resource_group_mappings = resource_group_info.mappings
    else:
        resource_groups_deps = []
        resource_group_mappings = {}

    resource_graph = create_resource_graph(
        ctx = ctx,
        labels = [],
        bundle_binary = ctx.attrs.binary,
        deps = ctx.attrs.deps + resource_groups_deps,
        exported_deps = [],
    )
    resource_graph_node_map_func = get_resource_graph_node_map_func(resource_graph)
    return get_filtered_resources(ctx.label, resource_graph_node_map_func, ctx.attrs.resource_group, resource_group_mappings)

def _copy_public_headers(ctx: AnalysisContext) -> list[AppleBundlePart]:
    if not ctx.attrs.copy_public_framework_headers:
        return []
    binary = getattr(ctx.attrs, "binary")
    if binary == None:
        return []
    apple_library_info = binary.get(AppleLibraryInfo)
    if apple_library_info == None:
        return []
    tset = apple_library_info.public_framework_headers

    bundle_parts = []
    if tset._tset:
        for public_framework_headers in tset._tset.traverse():
            for public_framework_header in public_framework_headers:
                for artifact in public_framework_header.artifacts:
                    bundle_parts.append(AppleBundlePart(source = artifact, destination = AppleBundleDestination("headers")))

    if apple_library_info.swift_header:
        bundle_parts.append(AppleBundlePart(source = apple_library_info.swift_header, destination = AppleBundleDestination("headers")))

    return bundle_parts

def _copy_module_map(ctx: AnalysisContext) -> list[AppleBundlePart]:
    extension = get_extension_attr(ctx)
    if not extension == "framework":
        return []
    module_map = ctx.attrs.module_map
    if not module_map:
        return []
    return [AppleBundlePart(source = module_map, destination = AppleBundleDestination("modules"))]

def _copy_resources(ctx: AnalysisContext, specs: list[AppleResourceSpec]) -> list[AppleBundlePart]:
    result = []

    for spec in specs:
        bundle_destination = apple_bundle_destination_from_resource_destination(spec.destination)
        result += [_process_apple_resource_file_if_needed(
            ctx = ctx,
            file = single_artifact(x).default_output,
            destination = bundle_destination,
            destination_relative_path = None,
            codesign_on_copy = spec.codesign_files_on_copy,
            codesign_entitlements = spec.codesign_entitlements,
            codesign_flags_override = spec.codesign_flags_override,
        ) for x in spec.files]
        result += _bundle_parts_for_dirs(spec.dirs, bundle_destination, False)
        result += _bundle_parts_for_dirs(spec.content_dirs, bundle_destination, True)
        result += _bundle_parts_for_variant_files(ctx, spec)

    return result

def _copy_first_level_bundles(ctx: AnalysisContext) -> list[AppleBundlePart]:
    first_level_bundle_infos = filter(None, [dep.get(AppleBundleInfo) for dep in ctx.attrs.deps])
    return filter(None, [_copied_bundle_spec(info) for info in first_level_bundle_infos])

def _copied_bundle_spec(bundle_info: AppleBundleInfo) -> [None, AppleBundlePart]:
    bundle = bundle_info.bundle
    bundle_extension = paths.split_extension(bundle.short_path)[1]
    if bundle_extension == ".framework":
        destination = AppleBundleDestination("frameworks")
        codesign_on_copy = True
    elif bundle_extension == ".app":
        app_destination_type = "plugins"
        if bundle_info.bundle_type == AppleBundleTypeWatchApp:
            app_destination_type = "watchapp"
        elif bundle_info.bundle_type == AppleBundleTypeAppClip:
            app_destination_type = "appclips"
        elif bundle_info.bundle_type != AppleBundleTypeDefault:
            fail("Unhandled bundle type `{}`".format(bundle_info.bundle_type))
        destination = AppleBundleDestination(app_destination_type)
        codesign_on_copy = False
    elif bundle_extension == ".appex":
        destination = AppleBundleDestination("plugins")
        codesign_on_copy = False
    elif bundle_extension == ".qlgenerator":
        destination = AppleBundleDestination("quicklook")
        codesign_on_copy = True
    elif bundle_extension == ".xpc":
        destination = AppleBundleDestination("xpcservices")
        codesign_on_copy = True
    else:
        fail("Extension `{}` is not yet supported.".format(bundle_extension))
    return AppleBundlePart(
        source = bundle,
        destination = destination,
        codesign_on_copy = codesign_on_copy,
    )

def _bundle_parts_for_dirs(generated_dirs: list[Artifact], destination: AppleBundleDestination, copy_contents_only: bool) -> list[AppleBundlePart]:
    return [AppleBundlePart(
        source = generated_dir,
        destination = destination,
        new_name = "" if copy_contents_only else None,
    ) for generated_dir in generated_dirs]

def _bundle_parts_for_variant_files(ctx: AnalysisContext, spec: AppleResourceSpec) -> list[AppleBundlePart]:
    result = []

    # By definition, all variant files go into the resources destination
    bundle_destination = AppleBundleDestination("resources")
    for variant_file in spec.variant_files:
        variant_dest_subpath = _get_dest_subpath_for_variant_file(variant_file)
        bundle_part = _process_apple_resource_file_if_needed(
            ctx = ctx,
            file = variant_file,
            destination = bundle_destination,
            destination_relative_path = variant_dest_subpath,
        )
        result.append(bundle_part)

    for locale, variant_files in spec.named_variant_files.items():
        if not locale.endswith(".lproj"):
            fail("Keys for named variant files have to end with '.lproj' suffix, got {}".format(locale))
        result += [
            _process_apple_resource_file_if_needed(
                ctx = ctx,
                file = variant_file,
                destination = bundle_destination,
                destination_relative_path = paths.join(locale, paths.basename(variant_file.short_path)),
            )
            for variant_file in variant_files
        ]

    return result

def _run_ibtool(
        ctx: AnalysisContext,
        raw_file: Artifact,
        output: OutputArtifact,
        action_flags: list[str],
        target_device: [None, str],
        action_identifier: str,
        output_is_dir: bool) -> None:
    # TODO(T110378103): detect and add minimum deployment target automatically
    # TODO(T110378113): add support for ibtool modules (turned on by `ibtool_module_flag` field of `apple_bundle` rule)

    # Equivalent of `AppleProcessResources::BASE_IBTOOL_FLAGS` from v1
    base_flags = ["--output-format", "human-readable-text", "--notices", "--warnings", "--errors"]
    ibtool = ctx.attrs._apple_toolchain[AppleToolchainInfo].ibtool
    ibtool_flags = getattr(ctx.attrs, "ibtool_flags", None) or []
    ibtool_command = [ibtool] + base_flags + ibtool_flags
    if target_device != None:
        ibtool_command.extend(["--target-device", target_device])
    ibtool_command.extend(action_flags)
    if output_is_dir:
        ibtool_command.append('"$TMPDIR"')
    else:
        ibtool_command.append(output)
    ibtool_command.append(raw_file)

    if output_is_dir:
        # Sandboxing and fs isolation on RE machines results in Xcode tools failing
        # when those are working in freshly created directories in buck-out.
        # See https://fb.workplace.com/groups/1042353022615812/permalink/1872164996301273/
        # As a workaround create a directory in tmp, use it for Xcode tools, then
        # copy the result to buck-out.
        wrapper_script, _ = ctx.actions.write(
            "ibtool_wrapper.sh",
            [
                cmd_args("set -euo pipefail"),
                cmd_args('export TMPDIR="$(mktemp -d)"'),
                cmd_args(cmd_args(ibtool_command), delimiter = " "),
                cmd_args(output, format = 'mkdir -p {} && cp -r "$TMPDIR"/ {}'),
            ],
            allow_args = True,
        )
        command = cmd_args(["/bin/sh", wrapper_script]).hidden([ibtool_command, output])
    else:
        command = ibtool_command

    processing_options = get_bundle_resource_processing_options(ctx)
    ctx.actions.run(command, prefer_local = processing_options.prefer_local, allow_cache_upload = processing_options.allow_cache_upload, category = "apple_ibtool", identifier = action_identifier)

def _compile_ui_resource(
        ctx: AnalysisContext,
        raw_file: Artifact,
        output: OutputArtifact,
        target_device: [None, str] = None,
        output_is_dir: bool = False) -> None:
    _run_ibtool(
        ctx = ctx,
        raw_file = raw_file,
        output = output,
        action_flags = ["--compile"],
        target_device = target_device,
        action_identifier = "compile_" + raw_file.basename,
        output_is_dir = output_is_dir,
    )

def _link_ui_resource(
        ctx: AnalysisContext,
        raw_file: Artifact,
        output: OutputArtifact,
        target_device: [None, str] = None,
        output_is_dir: bool = False) -> None:
    _run_ibtool(
        ctx = ctx,
        raw_file = raw_file,
        output = output,
        action_flags = ["--link"],
        target_device = target_device,
        action_identifier = "link_" + raw_file.basename,
        output_is_dir = output_is_dir,
    )

def _process_apple_resource_file_if_needed(
        ctx: AnalysisContext,
        file: Artifact,
        destination: AppleBundleDestination,
        destination_relative_path: [str, None],
        codesign_on_copy: bool = False,
        codesign_entitlements: Artifact | None = None,
        codesign_flags_override: list[str] | None = None) -> AppleBundlePart:
    output_dir = "_ProcessedResources"
    basename = paths.basename(file.short_path)
    output_is_contents_dir = False
    if basename.endswith(".plist") or basename.endswith(".stringsdict"):
        processed = ctx.actions.declare_output(paths.join(output_dir, file.short_path))
        process_plist(
            ctx = ctx,
            input = file,
            output = processed.as_output(),
            action_id = destination_relative_path,
        )
    elif basename.endswith(".storyboard"):
        compiled = ctx.actions.declare_output(paths.join(output_dir, paths.replace_extension(file.short_path, ".storyboardc")), dir = True)
        if get_is_watch_bundle(ctx):
            output_is_contents_dir = True
            _compile_ui_resource(ctx = ctx, raw_file = file, output = compiled.as_output(), target_device = "watch")
            processed = ctx.actions.declare_output(paths.join(output_dir, paths.replace_extension(file.short_path, "_linked_storyboard")), dir = True)
            _link_ui_resource(ctx = ctx, raw_file = compiled, output = processed.as_output(), target_device = "watch", output_is_dir = True)
        else:
            processed = compiled
            _compile_ui_resource(ctx, file, processed.as_output())
    elif basename.endswith(".xib"):
        processed = ctx.actions.declare_output(paths.join(output_dir, paths.replace_extension(file.short_path, ".nib")))
        _compile_ui_resource(ctx, file, processed.as_output())
    else:
        processed = file

    # When name is empty string only content of the directory will be copied, as opposed to the directory itself.
    # When name is `None`, directory or file will be copied as it is, without renaming.
    new_name = destination_relative_path if destination_relative_path else ("" if output_is_contents_dir else None)
    return AppleBundlePart(source = processed, destination = destination, new_name = new_name, codesign_on_copy = codesign_on_copy, codesign_entitlements = codesign_entitlements, codesign_flags_override = codesign_flags_override)

# Returns a path relative to the _parent_ of the lproj dir.
# For example, given a variant file with a short path of`XX/YY.lproj/ZZ`
# it would return `YY.lproj/ZZ`.
def _get_dest_subpath_for_variant_file(variant_file: Artifact) -> str:
    dir_name = paths.basename(paths.dirname(variant_file.short_path))
    if not dir_name.endswith("lproj"):
        fail("Variant files have to be in a directory with name ending in '.lproj' but `{}` was not.".format(variant_file.short_path))
    file_name = paths.basename(variant_file.short_path)
    return paths.join(dir_name, file_name)

def get_is_watch_bundle(ctx: AnalysisContext) -> bool:
    return ctx.attrs._apple_toolchain[AppleToolchainInfo].sdk_name.startswith("watch")
