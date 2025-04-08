# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":erlang_build.bzl", "erlang_build")
load(":erlang_dependencies.bzl", "check_dependencies", "flatten_dependencies")
load(":erlang_info.bzl", "ErlangAppInfo")
load(":erlang_toolchain.bzl", "get_primary", "get_primary_tools")

def _build_run_info(
        ctx: AnalysisContext,
        *,
        dependencies: list[Dependency],
        additional_app_paths: list[Artifact] = [],
        additional_paths: list[Artifact] = [],
        additional_args: list[cmd_args] = []) -> Provider:
    """Builds an Erlang shell with the dependencies and additional code paths available."""
    primary_toolchain_name = get_primary(ctx)

    app_paths = [
        dep[ErlangAppInfo].app_folders[primary_toolchain_name]
        for dep in dependencies
        if ErlangAppInfo in dep and not dep[ErlangAppInfo].virtual
    ] + additional_app_paths

    direct_shell_dependencies = check_dependencies(ctx.attrs.shell_libs, [ErlangAppInfo])
    all_shell_dependencies = flatten_dependencies(ctx, direct_shell_dependencies)
    for dep in all_shell_dependencies.values():
        if dep[ErlangAppInfo].virtual:
            continue
        app_paths.append(dep[ErlangAppInfo].app_folders[primary_toolchain_name])

    erl_args = cmd_args([])
    for app_path in app_paths:
        erl_args.add(cmd_args(app_path, format = "-pa \"${REPO_ROOT}\"/{}/ebin", delimiter = ""))

    for additional_path in additional_paths:
        erl_args.add(cmd_args(additional_path, format = "-pa \"${REPO_ROOT}\"/{}", delimiter = ""))

    # add configs
    config_files = _shell_config_files(ctx)
    for config_file in _shell_config_files(ctx):
        erl_args.add(cmd_args(config_file, format = "-config \"${REPO_ROOT}\"/{}", delimiter = ""))

    # add extra args
    for additional_args in additional_args:
        erl_args.add(additional_args)

    erl_args.add('"$@"')

    tools = get_primary_tools(ctx)
    erl_command = cmd_args([
        "exec",
        cmd_args(["\"${REPO_ROOT}\"/", cmd_args(tools.erl, delimiter = " ")], delimiter = ""),
        erl_args,
    ])

    start_shell_content = cmd_args([
        "export REPO_ROOT=$(buck2 root --kind=project)",
        cmd_args(erl_command, delimiter = " \\\n"),
        "",
    ])

    shell_script = ctx.actions.write("start_shell.sh", start_shell_content, with_inputs = True)
    shell_cmd = cmd_args(
        ["/usr/bin/env", "bash", shell_script],
        # depend on input paths
        hidden = app_paths + additional_paths + config_files,
    )

    return RunInfo(shell_cmd)

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
