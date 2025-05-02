# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":erlang_build.bzl", "erlang_build")
load(":erlang_dependencies.bzl", "flatten_dependencies")
load(":erlang_info.bzl", "ErlangAppInfo")
load(":erlang_toolchain.bzl", "get_primary", "get_primary_tools")

def _build_run_info(
        ctx: AnalysisContext,
        *,
        dependencies: list[Dependency],
        additional_app_paths: list[Artifact] = [],
        additional_paths: list[Artifact] = [],
        additional_args: [cmd_args, None] = None) -> Provider:
    """Builds an Erlang shell with the dependencies and additional code paths available."""
    primary_toolchain_name = get_primary(ctx)

    app_paths = [
        dep[ErlangAppInfo].app_folders[primary_toolchain_name]
        for dep in dependencies
        if ErlangAppInfo in dep and not dep[ErlangAppInfo].virtual
    ]
    app_paths.extend(additional_app_paths)

    all_shell_dependencies = flatten_dependencies(ctx, ctx.attrs.shell_libs)
    for dep in all_shell_dependencies.values():
        if dep[ErlangAppInfo].virtual:
            continue
        app_paths.append(dep[ErlangAppInfo].app_folders[primary_toolchain_name])

    tools = get_primary_tools(ctx)
    erl = cmd_args(cmd_args(tools.erl, delimiter = " "), format = "\"${REPO_ROOT}\"/{}")
    erl_args = cmd_args("exec", erl, delimiter = " \\\n")

    # add paths
    erl_args.add(cmd_args(app_paths, format = "-pa \"${REPO_ROOT}\"/{}/ebin"))
    erl_args.add(cmd_args(additional_paths, format = "-pa \"${REPO_ROOT}\"/{}"))

    # add configs
    config_files = _shell_config_files(ctx)
    erl_args.add(cmd_args(config_files, format = "-config \"${REPO_ROOT}\"/{}"))

    # add extra args
    if additional_args:
        erl_args.add(additional_args)
    erl_args.add('"$@"')

    start_shell_content = cmd_args(
        "#!/usr/bin/env bash",
        "export REPO_ROOT=$(buck2 root --kind=project)",
        erl_args,
        "",
    )

    shell_script = ctx.actions.write(
        "start_shell.sh",
        start_shell_content,
        is_executable = True,
        with_inputs = True,
    )

    return RunInfo(shell_script)

def _shell_config_files(ctx: AnalysisContext) -> list[Artifact]:
    config_files = []
    for config_dep in ctx.attrs.shell_configs:
        for artifact in config_dep[DefaultInfo].default_outputs:
            if erlang_build.utils.is_config(artifact):
                config_files.append(artifact)
        for artifact in config_dep[DefaultInfo].other_outputs:
            if erlang_build.utils.is_config(artifact):
                config_files.append(artifact)
    return config_files

erlang_shell = struct(
    build_run_info = _build_run_info,
)
