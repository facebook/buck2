# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load(":apple_bundle_utility.bzl", "get_bundle_min_target_version", "get_bundle_resource_processing_options")
load(":apple_core_data_types.bzl", "AppleCoreDataSpec")
load(":apple_sdk.bzl", "get_apple_sdk_name")
load(":resource_groups.bzl", "create_resource_graph")

def apple_core_data_impl(ctx: AnalysisContext) -> list[Provider]:
    spec = AppleCoreDataSpec(
        module = ctx.attrs.module,
        path = ctx.attrs.path,
    )
    graph = create_resource_graph(
        ctx = ctx,
        labels = ctx.attrs.labels,
        deps = [],
        exported_deps = [],
        core_data_spec = spec,
    )
    return [DefaultInfo(), graph]

def compile_apple_core_data(ctx: AnalysisContext, specs: list[AppleCoreDataSpec], product_name: str) -> Artifact | None:
    if len(specs) == 0:
        return None

    output = ctx.actions.declare_output("AppleCoreDataCompiled")

    # Aggregate all the coredata momc and mapc commands together
    tool_commands = []
    for spec in specs:
        tool, output_path = _get_model_args(ctx, spec)
        tool_command = _get_tool_command(ctx, spec, product_name, tool, output_path)
        tool_commands.append(tool_command)

    # Sandboxing and fs isolation on RE machines results in Xcode tools failing
    # when those are working in freshly created directories in buck-out.
    # See https://fb.workplace.com/groups/1042353022615812/permalink/1872164996301273/
    # As a workaround create a directory in tmp, use it for Xcode tools, then
    # copy the result to buck-out.
    wrapper_script, _ = ctx.actions.write(
        "tool_wrapper.sh",
        [
            cmd_args("set -euo pipefail"),
            cmd_args('export TMPDIR="$(mktemp -d)"'),
            cmd_args(tool_commands),
            cmd_args(output, format = 'mkdir -p {} && cp -r "$TMPDIR"/ {}'),
        ],
        allow_args = True,
    )
    combined_command = cmd_args(["/bin/sh", wrapper_script]).hidden(tool_commands + [output.as_output()])
    processing_options = get_bundle_resource_processing_options(ctx)
    ctx.actions.run(combined_command, prefer_local = processing_options.prefer_local, allow_cache_upload = processing_options.allow_cache_upload, category = "apple_core_data")
    return output

def _get_model_args(ctx: AnalysisContext, core_data_spec: AppleCoreDataSpec):
    toolchain = ctx.attrs._apple_toolchain[AppleToolchainInfo]

    if core_data_spec.path.extension == ".xcmappingmodel":
        filename = paths.replace_extension(core_data_spec.path.basename, ".cdm")
        return toolchain.mapc, cmd_args("$TMPDIR/" + filename)
    else:
        return toolchain.momc, cmd_args("$TMPDIR")

def _get_tool_command(ctx: AnalysisContext, core_data_spec: AppleCoreDataSpec, product_name: str, tool: RunInfo, output: cmd_args) -> cmd_args:
    return cmd_args([
        tool,
        "--sdkroot",
        ctx.attrs._apple_toolchain[AppleToolchainInfo].sdk_path,
        "--" + get_apple_sdk_name(ctx) + "-deployment-target",
        get_bundle_min_target_version(ctx, ctx.attrs.binary),
        "--module",
        core_data_spec.module if core_data_spec.module else product_name,
        cmd_args(core_data_spec.path, format = "./{}"),
        output,
    ], delimiter = " ").hidden(core_data_spec.path)
