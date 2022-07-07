load("@fbcode//buck2/prelude:paths.bzl", "paths")
load("@fbcode//buck2/prelude/apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")
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
load(":apple_bundle_types.bzl", "AppleBundleInfo")
load(":apple_bundle_utility.bzl", "get_extension_attr", "get_product_name")
load(":apple_core_data.bzl", "compile_apple_core_data")
load(
    ":apple_core_data_types.bzl",
    "AppleCoreDataSpec",  # @unused Used as a type
)
load(":apple_info_plist.bzl", "process_info_plist", "process_plist")
load(
    ":apple_resource_types.bzl",
    "AppleResourceSpec",  # @unused Used as a type
)
load(":apple_resource_utility.bzl", "apple_bundle_destination_from_resource_destination")
load(":link_groups.bzl", "get_link_group_mappings")
load(":resource_groups.bzl", "create_resource_graph", "get_filtered_resources", "get_resource_groups")

AppleBundleResourcePartListOutput = record(
    # Resource parts to be copied into an Apple bundle, *excluding* binaries
    resource_parts = field([AppleBundlePart.type]),
    # Part that holds the info.plist
    info_plist_part = field(AppleBundlePart.type),
)

def get_apple_bundle_resource_part_list(ctx: "context") -> AppleBundleResourcePartListOutput.type:
    parts = []

    parts.extend(_create_pkg_info_if_needed(ctx))

    resource_specs, asset_catalog_specs, core_data_specs = _select_resources(ctx)

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

    parts.extend(_copy_resources(ctx, resource_specs))
    parts.extend(_copy_first_level_bundles(ctx))

    return AppleBundleResourcePartListOutput(
        resource_parts = parts,
        info_plist_part = info_plist_part,
    )

# Same logic as in v1, see `buck_client/src/com/facebook/buck/apple/ApplePkgInfo.java`
def _create_pkg_info_if_needed(ctx: "context") -> [AppleBundlePart.type]:
    extension = get_extension_attr(ctx)
    if extension == "xpc" or extension == "qlgenerator":
        return []
    artifact = ctx.actions.write("PkgInfo", "APPLWRUN\n")
    return [AppleBundlePart(source = artifact, destination = AppleBundleDestination("metadata"))]

def _select_resources(ctx: "context") -> ([AppleResourceSpec.type], [AppleAssetCatalogSpec.type], [AppleCoreDataSpec.type]):
    resource_groups = get_resource_groups(ctx)
    resource_groups_deps = [mapping.target for group in resource_groups for mapping in group.mappings]
    resource_graph = create_resource_graph(
        root = ctx.label,
        labels = [],
        deps = ctx.attr.deps + filter(None, [ctx.attr.binary]) + resource_groups_deps,
        exported_deps = [],
    )
    resource_group_mappings = get_link_group_mappings(resource_groups, resource_graph)
    return get_filtered_resources(resource_graph, ctx.attr.resource_group, resource_group_mappings)

def _copy_resources(ctx: "context", specs: [AppleResourceSpec.type]) -> [AppleBundlePart.type]:
    result = []

    for spec in specs:
        bundle_destination = apple_bundle_destination_from_resource_destination(spec.destination)
        result += [_process_apple_resource_file_if_needed(
            ctx = ctx,
            file = x,
            destination = bundle_destination,
            destination_relative_path = None,
            codesign_on_copy = spec.codesign_files_on_copy,
        ) for x in spec.files]
        result += _bundle_parts_for_dirs(spec.dirs, bundle_destination, False)
        result += _bundle_parts_for_dirs(spec.content_dirs, bundle_destination, True)
        result += _bundle_parts_for_variant_files(ctx, spec)

    return result

def _copy_first_level_bundles(ctx: "context") -> [AppleBundlePart.type]:
    first_level_bundle_infos = filter(None, [dep[AppleBundleInfo] for dep in ctx.attr.deps])
    return filter(None, [_copied_bundle_spec(info) for info in first_level_bundle_infos])

def _copied_bundle_spec(bundle_info: AppleBundleInfo.type) -> [None, AppleBundlePart.type]:
    bundle = bundle_info.bundle
    bundle_extension = paths.split_extension(bundle.short_path)[1]
    if bundle_extension == ".framework":
        destination = AppleBundleDestination("frameworks")
        codesign_on_copy = True
    elif bundle_extension == ".app":
        expect(bundle_info.is_watchos != None, "Field should be set for bundles with extension {}".format(bundle_extension))
        destination = AppleBundleDestination("watchapp" if bundle_info.is_watchos else "plugins")
        codesign_on_copy = False
    elif bundle_extension == ".appex":
        destination = AppleBundleDestination("plugins")
        codesign_on_copy = False
    else:
        fail("Extension `{}` is not yet supported.".format(bundle_extension))
    return AppleBundlePart(
        source = bundle,
        destination = destination,
        codesign_on_copy = codesign_on_copy,
    )

def _bundle_parts_for_dirs(generated_dirs: ["artifact"], destination: AppleBundleDestination.type, copy_contents_only: bool.type) -> [AppleBundlePart.type]:
    return [AppleBundlePart(
        source = generated_dir,
        destination = destination,
        new_name = "" if copy_contents_only else None,
    ) for generated_dir in generated_dirs]

def _bundle_parts_for_variant_files(ctx: "context", spec: AppleResourceSpec.type) -> [AppleBundlePart.type]:
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
        ctx: "context",
        raw_file: "artifact",
        output: "output_artifact",
        action_flags: [str.type],
        target_device: [None, str.type],
        action_identifier: str.type,
        output_is_dir: bool.type) -> None:
    # TODO(T110378103): detect and add minimum deployment target automatically
    # TODO(T110378113): add support for ibtool modules (turned on by `ibtool_module_flag` field of `apple_bundle` rule)

    # Equivalent of `AppleProcessResources::BASE_IBTOOL_FLAGS` from v1
    base_flags = ["--output-format", "human-readable-text", "--notices", "--warnings", "--errors"]

    ibtool = ctx.attr._apple_toolchain[AppleToolchainInfo].ibtool
    ibtool_command = [ibtool] + base_flags + (ctx.attr.ibtool_flags or [])
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
        wrapper_script, hidden = ctx.actions.write(
            "ibtool_wrapper.sh",
            [
                cmd_args('export TMPDIR="$(mktemp -d)"'),
                cmd_args(cmd_args(ibtool_command), delimiter = " "),
                cmd_args(output, format = 'mkdir -p {} && cp -r "$TMPDIR"/ {}'),
            ],
            allow_args = True,
        )
        command = cmd_args(["/bin/sh", wrapper_script]).hidden(hidden + [ibtool_command, output])
    else:
        command = ibtool_command

    ctx.actions.run(command, category = "apple_ibtool", identifier = action_identifier)

def _compile_ui_resource(
        ctx: "context",
        raw_file: "artifact",
        output: "output_artifact",
        target_device: [None, str.type] = None,
        output_is_dir: bool.type = False) -> None:
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
        ctx: "context",
        raw_file: "artifact",
        output: "output_artifact",
        target_device: [None, str.type] = None,
        output_is_dir: bool.type = False) -> None:
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
        ctx: "context",
        file: "artifact",
        destination: AppleBundleDestination.type,
        destination_relative_path: [str.type, None],
        codesign_on_copy: bool.type = False) -> AppleBundlePart.type:
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
        compiled = ctx.actions.declare_output(paths.join(output_dir, paths.replace_extension(file.short_path, ".storyboardc")))
        if get_is_watch_bundle(ctx):
            output_is_contents_dir = True
            _compile_ui_resource(ctx = ctx, raw_file = file, output = compiled.as_output(), target_device = "watch")
            processed = ctx.actions.declare_output(paths.join(output_dir, paths.replace_extension(file.short_path, "_linked_storyboard")))
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
    return AppleBundlePart(source = processed, destination = destination, new_name = new_name, codesign_on_copy = codesign_on_copy)

# Returns a path relative to the _parent_ of the lproj dir.
# For example, given a variant file with a short path of`XX/YY.lproj/ZZ`
# it would return `YY.lproj/ZZ`.
def _get_dest_subpath_for_variant_file(variant_file: "artifact") -> str.type:
    dir_name = paths.basename(paths.dirname(variant_file.short_path))
    if not dir_name.endswith("lproj"):
        fail("Variant files have to be in a directory with name ending in '.lproj' but `{}` was not.".format(variant_file.short_path))
    file_name = paths.basename(variant_file.short_path)
    return paths.join(dir_name, file_name)

def get_is_watch_bundle(ctx: "context") -> bool.type:
    return ctx.attr._apple_toolchain[AppleToolchainInfo].watch_kit_stub_binary != None
