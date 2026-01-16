# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//java:java_providers.bzl", "derive_compiling_deps_wrapper", "get_global_code_info", "get_java_packaging_info")
load("@prelude//java:java_toolchain.bzl", "JavaToolchainInfo")
load("@prelude//utils:argfile.bzl", "argfile")
load("@prelude//utils:expect.bzl", "expect")
load(":android_providers.bzl", "AndroidResourceInfo", "AndroidResourceRDotInfo", "ExportedAndroidResourceInfo", "RESOURCE_PRIORITY_NORMAL", "merge_android_packageable_info")
load(":android_toolchain.bzl", "AndroidToolchainInfo")
load(":r_dot_java.bzl", "get_dummy_r_dot_java")

JAVA_PACKAGE_FILENAME = "java_package.txt"

def _convert_to_artifact_dir(
        ctx: AnalysisContext,
        attr: [Dependency, dict, Artifact, None],
        attr_name: str) -> Artifact | None:
    if isinstance(attr, Dependency):
        expect(len(attr[DefaultInfo].default_outputs) == 1, "Expect one default output from build dep of attr {}!".format(attr_name))
        return attr[DefaultInfo].default_outputs[0]
    elif type(attr) == "dict":
        return None if len(attr) == 0 else ctx.actions.symlinked_dir(attr_name, attr, has_content_based_path = True)
    else:
        return attr

def android_resource_impl(ctx: AnalysisContext) -> list[Provider]:
    if ctx.attrs._build_only_native_code:
        return [DefaultInfo()]

    sub_targets = {}
    providers = []
    default_output = None

    res = _convert_to_artifact_dir(ctx, ctx.attrs.res, "res")
    assets = _convert_to_artifact_dir(ctx, ctx.attrs.assets, "assets")

    if res:
        aapt2_compile_output = aapt2_compile(ctx, res, ctx.attrs._android_toolchain[AndroidToolchainInfo])

        sub_targets["aapt2_compile"] = [DefaultInfo(default_output = aapt2_compile_output)]

        r_dot_txt_output = get_text_symbols(ctx, res, ctx.attrs.deps)
        default_output = r_dot_txt_output

        r_dot_java_package = _get_package(ctx, ctx.attrs.package, ctx.attrs.manifest)
        resource_info = AndroidResourceInfo(
            raw_target = ctx.label.raw_target(),
            aapt2_compile_output = aapt2_compile_output,
            allowlisted_locales = ctx.attrs.allowlisted_locales,
            allow_strings_as_assets_resource_filtering = not ctx.attrs.has_whitelisted_strings,
            assets = assets,
            manifest_file = ctx.attrs.manifest,
            specified_r_dot_java_package = ctx.attrs.package,
            r_dot_java_package = r_dot_java_package,
            res = res,
            res_priority = RESOURCE_PRIORITY_NORMAL,
            text_symbols = r_dot_txt_output,
        )
    else:
        resource_info = AndroidResourceInfo(
            raw_target = ctx.label.raw_target(),
            aapt2_compile_output = None,
            allowlisted_locales = ctx.attrs.allowlisted_locales,
            allow_strings_as_assets_resource_filtering = not ctx.attrs.has_whitelisted_strings,
            assets = assets,
            manifest_file = ctx.attrs.manifest,
            specified_r_dot_java_package = None,
            r_dot_java_package = None,
            res = None,
            res_priority = RESOURCE_PRIORITY_NORMAL,
            text_symbols = None,
        )
    providers.append(resource_info)
    providers.append(merge_android_packageable_info(ctx.label, ctx.actions, ctx.attrs.deps, manifest = ctx.attrs.manifest, resource_info = resource_info))
    providers.append(get_java_packaging_info(ctx, ctx.attrs.deps))

    # Generate R.jar for autodeps support if we have resources
    if res and resource_info.text_symbols:
        android_toolchain = ctx.attrs._android_toolchain[AndroidToolchainInfo]
        dummy_r_dot_java_info = get_dummy_r_dot_java(
            ctx,
            android_toolchain.merge_android_resources[RunInfo],
            [resource_info],
            None,
        )
        android_resource_r_dot_info = AndroidResourceRDotInfo(
            dummy_r_dot_java = dummy_r_dot_java_info.library_output.abi,
        )
        providers.append(android_resource_r_dot_info)

    providers.append(DefaultInfo(default_output = default_output, sub_targets = sub_targets))
    compiling_deps = derive_compiling_deps_wrapper(ctx.actions, None, ctx.attrs.deps)
    providers.append(get_global_code_info(ctx, ctx.attrs.deps, ctx.attrs.deps, None, compiling_deps, [compiling_deps] if compiling_deps else [], ctx.attrs._java_toolchain[JavaToolchainInfo].global_code_config))

    return providers

def aapt2_compile(
        ctx: AnalysisContext,
        resources_dir: Artifact,
        android_toolchain: AndroidToolchainInfo,
        skip_crunch_pngs: bool = False,
        identifier: [str, None] = None) -> Artifact:
    aapt2_command = [cmd_args(android_toolchain.aapt2)]
    aapt2_command.append("compile")
    aapt2_command.append("--legacy")
    if skip_crunch_pngs:
        aapt2_command.append("--no-crunch")
    aapt2_command.extend(["--dir", resources_dir])
    aapt2_output = ctx.actions.declare_output(
        "{}_resources.flata".format(identifier) if identifier else "resources.flata",
        has_content_based_path = True,
    )
    aapt2_command.extend(["-o", aapt2_output.as_output()])

    ctx.actions.run(cmd_args(aapt2_command), category = "aapt2_compile", identifier = identifier)

    return aapt2_output

def _get_package(ctx: AnalysisContext, package: [str, None], manifest: Artifact | None) -> Artifact:
    if package:
        return ctx.actions.write(JAVA_PACKAGE_FILENAME, package, has_content_based_path = True)
    else:
        expect(manifest != None, "if package is not declared then a manifest must be")
        return extract_package_from_manifest(ctx, manifest)

def extract_package_from_manifest(ctx: AnalysisContext, manifest: Artifact) -> Artifact:
    r_dot_java_package = ctx.actions.declare_output(JAVA_PACKAGE_FILENAME, has_content_based_path = True)
    extract_package_cmd = cmd_args(
        ctx.attrs._android_toolchain[AndroidToolchainInfo].manifest_utils[RunInfo],
        "--manifest-path",
        manifest,
        "--package-output",
        r_dot_java_package.as_output(),
    )

    ctx.actions.run(extract_package_cmd, category = "android_extract_package")

    return r_dot_java_package

def get_text_symbols(
        ctx: AnalysisContext,
        res: Artifact,
        deps: list[Dependency],
        identifier: [str, None] = None):
    mini_aapt_cmd = cmd_args(ctx.attrs._android_toolchain[AndroidToolchainInfo].mini_aapt[RunInfo])

    mini_aapt_cmd.add(["--resource-paths", res])

    dep_symbols = _get_dep_symbols(deps)
    dep_symbol_paths_file = argfile(
        actions = ctx.actions,
        name = "{}_dep_symbol_paths_file".format(identifier) if identifier else "dep_symbol_paths_file",
        args = dep_symbols,
        has_content_based_path = True,
    )

    mini_aapt_cmd.add(["--dep-symbol-paths", dep_symbol_paths_file])

    text_symbols = ctx.actions.declare_output("{}_R.txt".format(identifier) if identifier else "R.txt", has_content_based_path = True)
    mini_aapt_cmd.add(["--output-path", text_symbols.as_output()])

    ctx.actions.run(mini_aapt_cmd, category = "mini_aapt", identifier = identifier)

    return text_symbols

def _get_dep_symbols(deps: list[Dependency]) -> list[Artifact]:
    dep_symbols = []
    for dep in deps:
        android_resource_info = dep.get(AndroidResourceInfo)
        exported_android_resource_info = dep.get(ExportedAndroidResourceInfo)
        expect(android_resource_info != None or exported_android_resource_info != None, "Dependencies of `android_resource` rules should be `android_resource`s or `android_library`s ({})", dep)
        if android_resource_info and android_resource_info.text_symbols:
            dep_symbols.append(android_resource_info.text_symbols)
        if exported_android_resource_info:
            dep_symbols += [resource_info.text_symbols for resource_info in exported_android_resource_info.resource_infos if resource_info.text_symbols]

    return dedupe(dep_symbols)
