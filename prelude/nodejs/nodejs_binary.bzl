# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:command_alias.bzl", "command_alias")
load("@prelude//os_lookup:defs.bzl", "Os", "OsLookup")
load("@prelude//zip_file:zip_file_toolchain.bzl", "ZipFileToolchainInfo")
load(":nodejs_package.bzl", "get_bundled_node", "package_standalone")
load(":nodejs_providers.bzl", "build_node_modules", "get_nodejs_dep_infos", "get_transitive_outputs")
load(":nodejs_toolchain.bzl", "NodejsToolchainInfo")

def build_nodejs_command(ctx: AnalysisContext, bundled_node: (str, Artifact) | None = None) -> struct:
    dep_infos = get_nodejs_dep_infos(ctx.attrs.deps, consumer_label = ctx.label)
    runtime = None
    entry = None
    if dep_infos or bundled_node:
        staged = {}
        for src in [ctx.attrs.main] + ctx.attrs.srcs:
            existing = staged.get(src.short_path)
            if existing != None and existing != src:
                fail("{}: `main`/`srcs` entries '{}' and '{}' both stage to '{}'".format(ctx.label, existing, src, src.short_path))
            staged[src.short_path] = src
        name = "_node_modules_"
        if bundled_node:
            node_name, node_dist = bundled_node
            if node_name in staged:
                fail(
                    "{}: `main`/`srcs` entry '{}' and the bundled Node binary '{}' both stage to '{}'".format(
                        ctx.label, staged[node_name], node_dist, node_name
                    )
                )
            staged[node_name] = node_dist
            name = "__standalone__/" + ctx.label.name
        runtime = build_node_modules(
            ctx.actions,
            name,
            get_transitive_outputs(ctx.actions, deps = dep_infos),
            root_files = staged,
        )
        entry = cmd_args(runtime, format = "{}/" + ctx.attrs.main.short_path)

    is_windows = ctx.attrs._target_os_type[OsLookup].os == Os("windows")
    env = dict(ctx.attrs.env)
    if dep_infos:
        delimiter = ";" if is_windows else ":"
        node_modules_path = cmd_args(runtime, format = "{}/node_modules")
        if "NODE_PATH" in env:
            env["NODE_PATH"] = cmd_args(env["NODE_PATH"], node_modules_path, delimiter = delimiter)
        else:
            env["NODE_PATH"] = node_modules_path

    node = ctx.attrs._nodejs_toolchain[NodejsToolchainInfo].node
    args = cmd_args()
    args.add(ctx.attrs.node_args)
    if entry != None:
        args.add(entry)
    else:
        args.add(ctx.attrs.main)
        if ctx.attrs.srcs:
            args.add(cmd_args(hidden = ctx.attrs.srcs))
    args.add(ctx.attrs.args)

    output = command_alias(
        actions = ctx.actions,
        path = None,
        target_os = ctx.attrs._target_os_type[OsLookup],
        base = node,
        args = args,
        env = env,
        labels = ctx.attrs.labels,
    )
    return struct(merged = runtime, output = output, env = env)

def nodejs_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    bundled_node = None
    if ctx.attrs.package_style == "standalone":
        bundled_node = get_bundled_node(ctx.label, ctx.attrs._nodejs_toolchain[NodejsToolchainInfo].node_dist, ctx.attrs._target_os_type)
    command = build_nodejs_command(ctx, bundled_node)
    default_output = command.merged
    if bundled_node:
        default_output = package_standalone(
            ctx.actions,
            ctx.attrs._zip_file_toolchain[ZipFileToolchainInfo].create_zip,
            command.merged,
            ctx.label.name,
        )
    return [
        DefaultInfo(default_output = default_output, other_outputs = command.output.output.other_outputs),
        RunInfo(args = command.output.cmd),
    ]
