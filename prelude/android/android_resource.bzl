load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")
load(":android_providers.bzl", "AndroidResourceInfo", "ExportedAndroidResourceInfo", "merge_android_packageable_info")
load(":android_toolchain.bzl", "AndroidToolchainInfo")

JAVA_PACKAGE_FILENAME = "java_package.txt"

def _convert_to_artifact_dir(ctx: "context", attr: ["dependency", "dict", "artifact", None], attr_name: str.type) -> ["artifact", None]:
    if type(attr) == "dependency":
        expect(len(attr[DefaultInfo].default_outputs) == 1, "Expect one default output from build dep of attr {}!".format(attr_name))
        return attr[DefaultInfo].default_outputs[0]
    elif type(attr) == "dict" and len(attr) > 0:
        return ctx.actions.symlinked_dir("{}_dir".format(attr_name), attr)
    else:
        return attr

def android_resource_impl(ctx: "context") -> ["provider"]:
    # TODO(T100007184) filter res/assets by ignored filenames
    sub_targets = {}
    providers = []
    default_outputs = []

    res = _convert_to_artifact_dir(ctx, ctx.attr.res, "res")
    assets = _convert_to_artifact_dir(ctx, ctx.attr.assets, "assets")

    if res:
        aapt2_compile_output = aapt2_compile(ctx, res, ctx.attr._android_toolchain[AndroidToolchainInfo])

        sub_targets["aapt2_compile"] = [DefaultInfo(default_outputs = [aapt2_compile_output])]

        r_dot_txt_output = get_text_symbols(ctx, res, ctx.attr.deps)
        default_outputs.append(r_dot_txt_output)

        r_dot_java_package = _get_package(ctx, ctx.attr.package, ctx.attr.manifest)
        resource_info = AndroidResourceInfo(
            aapt2_compile_output = aapt2_compile_output,
            assets = assets,
            has_resources = True,
            manifest_file = ctx.attr.manifest,
            r_dot_java_package = r_dot_java_package,
            text_symbols = r_dot_txt_output,
        )
    else:
        resource_info = AndroidResourceInfo(
            aapt2_compile_output = None,
            assets = assets,
            has_resources = False,
            manifest_file = ctx.attr.manifest,
            r_dot_java_package = None,
            text_symbols = None,
        )
    providers.append(resource_info)
    providers.append(merge_android_packageable_info(ctx.actions, ctx.attr.deps, manifest = ctx.attr.manifest, resource_info = resource_info))
    providers.append(DefaultInfo(default_outputs = default_outputs, sub_targets = sub_targets))

    return providers

def aapt2_compile(
        ctx: "context",
        resources_dir: "artifact",
        android_toolchain: "AndroidToolchainInfo",
        skip_crunch_pngs: bool.type = False,
        identifier: [str.type, None] = None) -> "artifact":
    aapt2_command = cmd_args(android_toolchain.aapt2)
    aapt2_command.add("compile")
    aapt2_command.add("--legacy")
    if skip_crunch_pngs:
        aapt2_command.add("--no-crunch")
    aapt2_command.add(["--dir", resources_dir])
    aapt2_output = ctx.actions.declare_output("{}_resources.flata".format(identifier) if identifier else "resources.flata")
    aapt2_command.add("-o", aapt2_output.as_output())

    ctx.actions.run(aapt2_command, category = "aapt2_compile", identifier = identifier)

    return aapt2_output

def _get_package(ctx: "context", package: [str.type, None], manifest: ["artifact", None]) -> "artifact":
    if package:
        return ctx.actions.write(JAVA_PACKAGE_FILENAME, package)
    else:
        expect(manifest != None, "if package is not declared then a manifest must be")
        return extract_package_from_manifest(ctx, manifest)

def extract_package_from_manifest(ctx: "context", manifest: "artifact") -> "artifact":
    r_dot_java_package = ctx.actions.declare_output(JAVA_PACKAGE_FILENAME)
    extract_package_cmd = cmd_args(ctx.attr._android_toolchain[AndroidToolchainInfo].manifest_utils[RunInfo])
    extract_package_cmd.add(["--manifest-path", manifest])
    extract_package_cmd.add(["--package-output", r_dot_java_package.as_output()])

    ctx.actions.run(extract_package_cmd, category = "android_extract_package")

    return r_dot_java_package

def get_text_symbols(
        ctx: "context",
        res: "artifact",
        deps: ["dependency"],
        identifier: [str.type, None] = None):
    mini_aapt_cmd = cmd_args(ctx.attr._android_toolchain[AndroidToolchainInfo].mini_aapt[RunInfo])

    mini_aapt_cmd.add(["--resource-paths", res])

    dep_symbol_paths = cmd_args()
    dep_symbols = _get_dep_symbols(deps)
    dep_symbol_paths.add(dep_symbols)

    dep_symbol_paths_file, dep_symbol_macro_files = ctx.actions.write("{}_dep_symbol_paths_file".format(identifier) if identifier else "dep_symbol_paths_file", dep_symbol_paths, allow_args = True)

    mini_aapt_cmd.add(["--dep-symbol-paths", dep_symbol_paths_file])
    mini_aapt_cmd.hidden(dep_symbol_macro_files)
    mini_aapt_cmd.hidden(dep_symbols)

    text_symbols = ctx.actions.declare_output("{}_R.txt".format(identifier) if identifier else "R.txt")
    mini_aapt_cmd.add(["--output-path", text_symbols.as_output()])

    ctx.actions.run(mini_aapt_cmd, category = "mini_aapt", identifier = identifier)

    return text_symbols

def _get_dep_symbols(deps: ["dependency"]) -> ["artifact"]:
    dep_symbols = []
    for dep in deps:
        android_resource_info = dep[AndroidResourceInfo]
        exported_android_resource_info = dep[ExportedAndroidResourceInfo]
        expect(android_resource_info != None or exported_android_resource_info != None, "Dependencies of `android_resource` rules should be `android_resource`s or `android_library`s")
        if android_resource_info and android_resource_info.text_symbols:
            dep_symbols.append(android_resource_info.text_symbols)
        if exported_android_resource_info:
            dep_symbols += [resource_info.text_symbols for resource_info in exported_android_resource_info.resource_infos if resource_info.text_symbols]

    return dedupe(dep_symbols)
