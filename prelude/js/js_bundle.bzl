load("@fbcode//buck2/prelude/android:android_providers.bzl", "AndroidResourceInfo", "merge_android_packageable_info")
load("@fbcode//buck2/prelude/android:android_resource.bzl", "JAVA_PACKAGE_FILENAME", "aapt2_compile", "get_text_symbols")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/js:js_providers.bzl", "JsBundleInfo", "JsLibraryInfo")
load("@fbcode//buck2/prelude/js:js_utils.bzl", "RAM_BUNDLE_TYPES", "TRANSFORM_PROFILES", "fixup_command_args", "get_bundle_name", "get_flavors", "run_worker_command")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect", "flatten", "map_idx")

def _build_dependencies_file(
        ctx: "context",
        transform_profile: str.type,
        transitive_js_library_outputs: ["artifact"]) -> "artifact":
    dependencies_file = ctx.actions.declare_output("{}/dependencies_file", transform_profile)

    # ctx.attr.extra_json can contain attr.arg().
    #
    # As a result, we need to pass extra_data_args as hidden arguments so that the rule
    # it is referencing exists as an input.
    extra_data_args = cmd_args(
        ctx.attr.extra_json if ctx.attr.extra_json else "{}",
        delimiter = "",
    )
    job_args = {
        "command": "dependencies",
        "entryPoints": [ctx.attr.entry] if type(ctx.attr.entry) == "string" else list(ctx.attr.entry),
        "extraData": extra_data_args,
        "flavors": get_flavors(ctx),
        "libraries": transitive_js_library_outputs,
        "outputFilePath": dependencies_file,
        "platform": ctx.attr._platform,
        "release": ctx.attr._is_release,
    }
    command_args_file = ctx.actions.write_json(
        "{}_dep_command_args".format(transform_profile),
        job_args,
    )

    run_worker_command(
        ctx = ctx,
        worker_tool = ctx.attr.worker,
        command_args_file = fixup_command_args(ctx, command_args_file) if ctx.attr.extra_json else command_args_file,
        identifier = transform_profile,
        category = "dependencies",
        hidden_artifacts = [
            dependencies_file.as_output(),
            extra_data_args,
        ] + transitive_js_library_outputs,
    )
    return dependencies_file

def _build_js_bundle(
        ctx: "context",
        bundle_name: str.type,
        ram_bundle_name: str.type,
        ram_bundle_command: str.type,
        transform_profile: str.type,
        transitive_js_library_outputs: ["artifact"],
        dependencies_file: "artifact") -> JsBundleInfo.type:
    base_dir = "{}_{}".format(ram_bundle_name, transform_profile) if ram_bundle_name else transform_profile
    assets_dir = ctx.actions.declare_output("{}/assets_dir".format(base_dir))
    bundle_dir_output = ctx.actions.declare_output("{}/js".format(base_dir))
    misc_dir_path = ctx.actions.declare_output("{}/misc_dir_path".format(base_dir))
    source_map = ctx.actions.declare_output("{}/source_map".format(base_dir))

    # ctx.attr.extra_json can contain attr.arg().
    #
    # As a result, we need to pass extra_data_args as hidden arguments so that the rule
    # it is referencing exists as an input.
    extra_data_args = cmd_args(
        ctx.attr.extra_json if ctx.attr.extra_json else "{}",
        delimiter = "",
    )
    job_args = {
        "assetsDirPath": assets_dir,
        "bundlePath": cmd_args(
            [bundle_dir_output, bundle_name],
            delimiter = "/",
        ),
        "command": "bundle",
        "entryPoints": [ctx.attr.entry] if type(ctx.attr.entry) == "string" else list(ctx.attr.entry),
        "extraData": extra_data_args,
        "flavors": get_flavors(ctx),
        "libraries": transitive_js_library_outputs,
        "miscDirPath": misc_dir_path,
        "platform": ctx.attr._platform,
        "release": ctx.attr._is_release,
        "sourceMapPath": source_map,
    }

    if ram_bundle_command:
        job_args["ramBundle"] = ram_bundle_command

    command_args_file = ctx.actions.write_json(
        "{}_bundle_command_args".format(base_dir),
        job_args,
    )

    run_worker_command(
        ctx = ctx,
        worker_tool = ctx.attr.worker,
        command_args_file = fixup_command_args(
            ctx,
            command_args_file,
        ) if ctx.attr.extra_json else command_args_file,
        identifier = base_dir,
        category = job_args["command"],
        hidden_artifacts = [
            bundle_dir_output.as_output(),
            assets_dir.as_output(),
            misc_dir_path.as_output(),
            source_map.as_output(),
            extra_data_args,
        ] + transitive_js_library_outputs,
    )

    return JsBundleInfo(
        bundle_name = bundle_name,
        built_js = bundle_dir_output,
        source_map = source_map,
        res = assets_dir,
        misc = misc_dir_path,
        dependencies_file = dependencies_file,
    )

def _get_fallback_transform_profile(ctx: "context") -> str.type:
    if ctx.attr.fallback_transform_profile in TRANSFORM_PROFILES:
        return ctx.attr.fallback_transform_profile

    if ctx.attr.fallback_transform_profile == "default" or ctx.attr.fallback_transform_profile == None:
        return "transform-profile-default"

    fail("Invalid fallback_transform_profile attribute {}!".format(ctx.attr.fallback_transform_profile))

def _get_default_providers(js_bundle_info: JsBundleInfo.type) -> ["provider"]:
    return [DefaultInfo(default_outputs = [js_bundle_info.built_js])]

def _get_android_resource_info(ctx: "context", js_bundle_info: JsBundleInfo.type, identifier: str.type) -> "AndroidResourceInfo":
    aapt2_compile_output = aapt2_compile(
        ctx,
        js_bundle_info.res,
        ctx.attr._android_toolchain[AndroidToolchainInfo],
        identifier = identifier,
    )
    expect(ctx.attr.android_package != None, "Must provide android_package for android builds!")
    r_dot_java_package = ctx.actions.write("{}_{}".format(identifier, JAVA_PACKAGE_FILENAME), ctx.attr.android_package)
    return AndroidResourceInfo(
        aapt2_compile_output = aapt2_compile_output,
        assets = js_bundle_info.built_js,
        manifest_file = None,
        r_dot_java_package = r_dot_java_package,
        res = js_bundle_info.res,
        text_symbols = get_text_symbols(ctx, js_bundle_info.res, [], identifier),
    )

def _get_extra_providers(ctx: "context", js_bundle_info: JsBundleInfo.type, identifier: str.type) -> ["provider"]:
    providers = [js_bundle_info]
    if ctx.attr._platform == "android":
        resource_info = _get_android_resource_info(ctx, js_bundle_info, identifier)
        providers.append(resource_info)
        providers.append(merge_android_packageable_info(ctx.actions, ctx.attr.deps, resource_info = resource_info))

    return providers

def js_bundle_impl(ctx: "context") -> ["provider"]:
    sub_targets = {}
    default_outputs = []
    extra_unnamed_output_providers = None
    bundle_name = get_bundle_name(ctx, "{}.js".format(ctx.attr.name))
    for transform_profile in TRANSFORM_PROFILES:
        dep_infos = map_idx(JsLibraryInfo, [dep[DefaultInfo].sub_targets[transform_profile] for dep in ctx.attr.deps])
        transitive_js_library_outputs = dedupe(flatten([dep_info.transitive_outputs for dep_info in dep_infos]))
        dependencies_file = _build_dependencies_file(ctx, transform_profile, transitive_js_library_outputs)

        for ram_bundle_name, ram_bundle_command in RAM_BUNDLE_TYPES.items():
            js_bundle_info = _build_js_bundle(ctx, bundle_name, ram_bundle_name, ram_bundle_command, transform_profile, transitive_js_library_outputs, dependencies_file)

            simple_name = transform_profile if not ram_bundle_name else "{}-{}".format(ram_bundle_name, transform_profile)
            built_js_providers = _get_default_providers(js_bundle_info)
            extra_providers = _get_extra_providers(ctx, js_bundle_info, simple_name)
            misc_providers = [DefaultInfo(default_outputs = [js_bundle_info.misc])]
            source_map_providers = [DefaultInfo(default_outputs = [js_bundle_info.source_map])]
            dependencies_providers = [DefaultInfo(default_outputs = [js_bundle_info.dependencies_file])]

            sub_targets[simple_name] = built_js_providers + extra_providers
            sub_targets["{}-misc".format(simple_name)] = misc_providers
            sub_targets["{}-source_map".format(simple_name)] = source_map_providers
            sub_targets["{}-dependencies".format(simple_name)] = dependencies_providers

            fallback_transform_profile = _get_fallback_transform_profile(ctx)
            if transform_profile == fallback_transform_profile:
                if not ram_bundle_name:
                    default_outputs.append(js_bundle_info.built_js)
                    expect(extra_unnamed_output_providers == None, "Extra unnamed output providers should only be set once!")
                    extra_unnamed_output_providers = extra_providers
                    sub_targets["misc"] = misc_providers
                    sub_targets["source_map"] = source_map_providers
                    sub_targets["dependencies"] = dependencies_providers
                else:
                    sub_targets[ram_bundle_name] = built_js_providers + extra_providers
                    sub_targets["{}-misc".format(ram_bundle_name)] = misc_providers
                    sub_targets["{}-source_map".format(ram_bundle_name)] = source_map_providers
                    sub_targets["{}-dependencies".format(ram_bundle_name)] = dependencies_providers

    expect(len(default_outputs) == 1, "Should get exactly one default output!")
    expect(extra_unnamed_output_providers != None, "Should set extra unnamed output providers once!")
    return [
        DefaultInfo(default_outputs = default_outputs, sub_targets = sub_targets),
    ] + extra_unnamed_output_providers
