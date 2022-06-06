load("@fbcode//buck2/prelude:attributes.bzl", "RType")
load("@fbcode//buck2/prelude/android:aapt2_link.bzl", "get_aapt2_link")
load("@fbcode//buck2/prelude/android:android_manifest.bzl", "generate_android_manifest")
load("@fbcode//buck2/prelude/android:android_providers.bzl", "AndroidBinaryResourcesInfo")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/android:r_dot_java.bzl", "generate_r_dot_java")
load(
    "@fbcode//buck2/prelude/cxx:resources.bzl",
    "gather_cxx_resources",
)
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JavaToolchainInfo")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

def get_android_binary_resources_info(
        ctx: "context",
        deps: ["dependency"],
        android_packageable_info: "AndroidPackageableInfo",
        use_proto_format: bool.type,
        referenced_resources_lists: ["artifact"]) -> "AndroidBinaryResourcesInfo":
    resource_infos = list(android_packageable_info.resource_infos.traverse() if android_packageable_info.resource_infos else [])

    android_manifest = _get_manifest(ctx, android_packageable_info)

    aapt2_link_info = get_aapt2_link(
        ctx,
        ctx.attr._android_toolchain[AndroidToolchainInfo],
        [resource_info.aapt2_compile_output for resource_info in resource_infos if resource_info.has_resources],
        android_manifest,
        includes_vector_drawables = getattr(ctx.attr, "includes_vector_drawables", False),
        no_auto_version = getattr(ctx.attr, "no_auto_version_resources", False),
        no_version_transitions = getattr(ctx.attr, "no_version_transitions_resources", False),
        no_auto_add_overlay = getattr(ctx.attr, "no_auto_add_overlay_resources", False),
        use_proto_format = use_proto_format,
        no_resource_removal = True,
        package_id_offset = 0,
        should_keep_raw_values = getattr(ctx.attr, "aapt2_keep_raw_values", False),
        resource_stable_ids = getattr(ctx.attr, "resource_stable_ids", None),
        compiled_resource_apks = [],
        additional_aapt2_params = getattr(ctx.attr, "additional_aapt_params", []),
        extra_filtered_resources = getattr(ctx.attr, "extra_filtered_resources", []),
    )

    override_symbols_paths = ctx.actions.write("override_symbols_paths", [])
    resources = [resource for resource in resource_infos if resource.has_resources]
    r_dot_java = None if len(resources) == 0 else generate_r_dot_java(
        ctx,
        ctx.attr._android_toolchain[AndroidToolchainInfo].merge_android_resources[RunInfo],
        ctx.attr._java_toolchain[JavaToolchainInfo],
        resources,
        get_effective_banned_duplicate_resource_types(
            getattr(ctx.attr, "duplicate_resource_behavior", "allow_by_default"),
            getattr(ctx.attr, "allowed_duplicate_resource_types", []),
            getattr(ctx.attr, "banned_duplicate_resource_types", []),
        ),
        [aapt2_link_info.r_dot_txt],
        override_symbols_paths,
        getattr(ctx.attr, "duplicate_resource_whitelist", None),
        getattr(ctx.attr, "resource_union_package", None),
        referenced_resources_lists,
    )

    cxx_resources = _get_cxx_resources(ctx, deps)
    apk_with_merged_assets = _merge_assets(ctx, aapt2_link_info.primary_resources_apk, resource_infos, cxx_resources)

    return AndroidBinaryResourcesInfo(
        manifest = android_manifest,
        primary_resources_apk = apk_with_merged_assets,
        proguard_config_file = aapt2_link_info.proguard_config_file,
        r_dot_java = r_dot_java,
    )

def _get_manifest(ctx: "context", android_packageable_info: "AndroidPackageableInfo") -> "artifact":
    robolectric_manifest = getattr(ctx.attr, "robolectric_manifest", None)
    if robolectric_manifest:
        return robolectric_manifest

    if ctx.attr.manifest:
        expect(ctx.attr.manifest_skeleton == None, "Only one of manifest and manifest_skeleton should be declared")
        android_manifest = ctx.attr.manifest
    else:
        expect(ctx.attr.manifest_skeleton != None, "Must declare one of manifest and manifest_skeleton")
        android_manifest, _ = generate_android_manifest(
            ctx,
            ctx.attr._android_toolchain[AndroidToolchainInfo].generate_manifest[RunInfo],
            ctx.attr.manifest_skeleton,
            "dex",  # ROOT_APKMODULE_NAME,
            android_packageable_info.manifests,
            ctx.attr.manifest_entries.get("placeholders", {}),
        )

    return android_manifest

def _merge_assets(
        ctx: "context",
        base_apk: "artifact",
        resource_infos: ["AndroidResourceInfo"],
        cxx_resources: ["artifact", None]) -> "artifact":
    assets_dirs = [resource_info.assets for resource_info in resource_infos if resource_info.assets]
    if cxx_resources != None:
        assets_dirs.extend([cxx_resources])
    if len(assets_dirs) == 0:
        return base_apk

    merge_assets_cmd = cmd_args(ctx.attr._android_toolchain[AndroidToolchainInfo].merge_assets[RunInfo])

    merged_assets_output = ctx.actions.declare_output("merged_assets.ap_")
    merge_assets_cmd.add(["--output-apk", merged_assets_output.as_output()])

    if base_apk != None:
        merge_assets_cmd.add(["--base-apk", base_apk])

    assets_dirs_file = ctx.actions.write("assets_dirs", assets_dirs)
    merge_assets_cmd.add(["--assets-dirs", assets_dirs_file])
    merge_assets_cmd.hidden(assets_dirs)

    ctx.actions.run(merge_assets_cmd, category = "merge_assets")

    return merged_assets_output

def get_effective_banned_duplicate_resource_types(
        duplicate_resource_behavior: str.type,
        allowed_duplicate_resource_types: [str.type],
        banned_duplicate_resource_types: [str.type]) -> [str.type]:
    if duplicate_resource_behavior == "allow_by_default":
        expect(
            len(allowed_duplicate_resource_types) == 0,
            "Cannot set allowed_duplicate_resource_types if duplicate_resource_behaviour is allow_by_default",
        )
        return banned_duplicate_resource_types
    elif duplicate_resource_behavior == "ban_by_default":
        expect(
            len(banned_duplicate_resource_types) == 0,
            "Cannot set banned_duplicate_resource_types if duplicate_resource_behaviour is ban_by_default",
        )
        return [rtype for rtype in RType if rtype not in allowed_duplicate_resource_types]
    else:
        fail("Unrecognized duplicate_resource_behavior: {}".format(duplicate_resource_behavior))

def _get_cxx_resources(ctx: "context", deps: ["dependency"]) -> ["artifact", None]:
    cxx_resources = gather_cxx_resources(
        label = ctx.label,
        resources = {},
        deps = deps,
    )

    symlink_tree_dict = {}
    resource_maps = cxx_resources.values()
    for resource_map in resource_maps:
        for name, (resource, _other) in resource_map.items():
            symlink_tree_dict["cxx-resources/{}".format(name)] = resource

    return ctx.actions.symlinked_dir("cxx_resources_dir", symlink_tree_dict) if symlink_tree_dict else None
