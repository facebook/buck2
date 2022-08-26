load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//utils:utils.bzl", "flatten")
load(":apple_asset_catalog_compilation_options.bzl", "AppleAssetCatalogsCompilationOptions", "get_apple_asset_catalogs_compilation_options")  # @unused Used as a type
load(":apple_asset_catalog_types.bzl", "AppleAssetCatalogResult", "AppleAssetCatalogSpec", "StringWithSourceTarget")
load(":apple_bundle_utility.bzl", "get_bundle_min_target_version", "get_bundle_resource_processing_options")
load(":apple_sdk.bzl", "get_apple_sdk_name")
load(":apple_sdk_metadata.bzl", "get_apple_sdk_metadata_for_sdk_name")
load(":resource_groups.bzl", "create_resource_graph")

def apple_asset_catalog_impl(ctx: "context") -> ["provider"]:
    spec = AppleAssetCatalogSpec(
        app_icon = StringWithSourceTarget(source = ctx.label, value = ctx.attrs.app_icon) if ctx.attrs.app_icon != None else None,
        dirs = ctx.attrs.dirs,
        launch_image = StringWithSourceTarget(source = ctx.label, value = ctx.attrs.launch_image) if ctx.attrs.launch_image != None else None,
    )
    graph = create_resource_graph(
        ctx = ctx,
        labels = ctx.attrs.labels,
        deps = [],
        exported_deps = [],
        asset_catalog_spec = spec,
    )
    return [DefaultInfo(default_outputs = []), graph]

def compile_apple_asset_catalog(ctx: "context", specs: [AppleAssetCatalogSpec.type]) -> [AppleAssetCatalogResult.type, None]:
    single_spec = _merge_asset_catalog_specs(ctx, specs)
    if len(single_spec.dirs) == 0:
        return None
    plist = ctx.actions.declare_output("AssetCatalog.plist")
    catalog = ctx.actions.declare_output("AssetCatalogCompiled")
    processing_options = get_bundle_resource_processing_options(ctx)
    compilation_options = get_apple_asset_catalogs_compilation_options(ctx)
    command = _get_actool_command(ctx, single_spec, catalog.as_output(), plist.as_output(), compilation_options)
    ctx.actions.run(command, prefer_local = processing_options.prefer_local, category = "apple_asset_catalog")
    return AppleAssetCatalogResult(compiled_catalog = catalog, catalog_plist = plist)

def _merge_asset_catalog_specs(ctx: "context", xs: [AppleAssetCatalogSpec.type]) -> AppleAssetCatalogSpec.type:
    app_icon = _get_at_most_one_attribute(ctx, xs, "app_icon")
    launch_image = _get_at_most_one_attribute(ctx, xs, "launch_image")
    dirs = dedupe(flatten([x.dirs for x in xs]))
    return AppleAssetCatalogSpec(app_icon = app_icon, dirs = dirs, launch_image = launch_image)

def _get_at_most_one_attribute(ctx: "context", xs: ["_record"], attr_name: str.type) -> ["StringWithSourceTarget", None]:
    all_values = dedupe(filter(None, [getattr(x, attr_name) for x in xs]))
    if len(all_values) > 1:
        fail("At most one asset catalog in the dependencies of `{}` can have an `{}` attribute. At least 2 catalogs are providing it: `{}` and `{}`.".format(_get_target(ctx), attr_name, all_values[0].source, all_values[1].source))
    elif len(all_values) == 1:
        return all_values[0]
    else:
        return None

def _get_target(ctx: "context") -> str.type:
    return ctx.label.package + ":" + ctx.label.name

def _get_actool_command(ctx: "context", info: AppleAssetCatalogSpec.type, catalog_output: "output_artifact", plist_output: "output_artifact", compilation_options: AppleAssetCatalogsCompilationOptions.type) -> "cmd_args":
    external_name = get_apple_sdk_name(ctx)
    target_device = get_apple_sdk_metadata_for_sdk_name(external_name).target_device_flags

    actool = ctx.attrs._apple_toolchain[AppleToolchainInfo].actool
    actool_command = cmd_args([
                                  actool,
                                  "--platform",
                                  external_name,
                                  "--minimum-deployment-target",
                                  get_bundle_min_target_version(ctx),
                                  "--compile",
                                  catalog_output,
                                  "--output-partial-info-plist",
                                  plist_output,
                              ] +
                              target_device +
                              (
                                  ["--app-icon", info.app_icon.value] if info.app_icon else []
                              ) + (
                                  ["--launch-image", info.launch_image.value] if info.launch_image else []
                              ) + (
                                  ["--notices"] if compilation_options.enable_notices else []
                              ) + (
                                  ["--warnings"] if compilation_options.enable_warnings else []
                              ) + (
                                  ["--errors"] if compilation_options.enable_errors else []
                              ) + (
                                  ["--compress-pngs"] if compilation_options.compress_pngs else []
                              ) +
                              ["--optimization", compilation_options.optimization] +
                              ["--output-format", compilation_options.output_format] +
                              compilation_options.extra_flags +
                              info.dirs)

    # `actool` expects the output directory to be present.
    # Use the wrapper script to create the directory first and then actually call `actool`.
    wrapper_script, hidden = ctx.actions.write(
        "actool_wrapper.sh",
        [
            cmd_args(catalog_output, format = "mkdir -p {}"),
            cmd_args(actool_command, delimiter = " "),
        ],
        allow_args = True,
    )
    command = cmd_args(["/bin/sh", wrapper_script]).hidden(hidden + [actool_command])
    return command
