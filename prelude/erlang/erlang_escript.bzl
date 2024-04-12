# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load(":erlang_build.bzl", "erlang_build")
load(":erlang_dependencies.bzl", "ErlAppDependencies", "check_dependencies", "flatten_dependencies")
load(":erlang_info.bzl", "ErlangAppInfo")
load(":erlang_release.bzl", "build_lib_dir")
load(
    ":erlang_toolchain.bzl",
    "Toolchain",  # @unused Used as type
    "get_primary",
    "select_toolchains",
)
load(":erlang_utils.bzl", "action_identifier", "to_term_args")

def erlang_escript_impl(ctx: AnalysisContext) -> list[Provider]:
    # select the correct tools from the toolchain
    toolchain = select_toolchains(ctx)[get_primary(ctx)]

    # collect all dependencies
    dependencies = flatten_dependencies(ctx, check_dependencies(ctx.attrs.deps, [ErlangAppInfo]))

    if ctx.attrs.bundled:
        return _bundled_escript_impl(ctx, dependencies, toolchain)
    else:
        return _unbundled_escript_impl(ctx, dependencies, toolchain)

def _unbundled_escript_impl(ctx: AnalysisContext, dependencies: ErlAppDependencies, toolchain: Toolchain) -> list[Provider]:
    if ctx.attrs.resources:
        fail("resources are not supported with unbundled escripts, add them to an applications priv/ directory instead")

    escript_name = _escript_name(ctx)

    lib_dir = build_lib_dir(
        ctx,
        toolchain,
        escript_name,
        dependencies,
    )

    config_files = _escript_config_files(ctx)
    escript_trampoline = build_escript_unbundled_trampoline(ctx, toolchain, config_files)

    trampoline = {
        "run.escript": escript_trampoline,
    }

    all_outputs = {}
    for outputs in [lib_dir, trampoline]:
        all_outputs.update(outputs)

    for config_file in config_files:
        all_outputs[config_file.short_path] = config_file

    output = ctx.actions.symlinked_dir(
        escript_name,
        all_outputs,
    )

    cmd = cmd_args([
        toolchain.escript_trampoline,
        output,
        toolchain.otp_binaries.escript,
    ])

    return [DefaultInfo(default_output = output), RunInfo(cmd)]

def _bundled_escript_impl(ctx: AnalysisContext, dependencies: ErlAppDependencies, toolchain: Toolchain) -> list[Provider]:
    toolchain_name = get_primary(ctx)
    artifacts = {}

    for dep in dependencies.values():
        if ErlangAppInfo not in dep:
            # skip extra includes
            continue
        dep_info = dep[ErlangAppInfo]
        if dep_info.virtual:
            # skip virtual apps
            continue

        # add ebin
        ebin_files = dep_info.beams[toolchain_name].values() + [dep_info.app_file[toolchain_name]]
        for ebin_file in ebin_files:
            artifacts[_ebin_path(ebin_file, dep_info.name)] = ebin_file

        # priv dir
        if ctx.attrs.include_priv:
            artifacts[_priv_path(dep_info.name)] = dep_info.priv_dir[toolchain_name]

    # additional resources
    for res in ctx.attrs.resources:
        for artifact in res[DefaultInfo].default_outputs + res[DefaultInfo].other_outputs:
            if artifact.short_path in artifacts:
                fail("multiple artifacts defined for path %s", (artifact.short_path))
            artifacts[artifact.short_path] = artifact

    escript_name = _escript_name(ctx)
    output = ctx.actions.declare_output(escript_name)

    args = ctx.attrs.emu_args

    config_files = _escript_config_files(ctx)
    for config_file in config_files:
        artifacts[config_file.short_path] = config_file

    escript_trampoline = build_escript_bundled_trampoline(ctx, toolchain, config_files)
    artifacts[escript_trampoline.basename] = escript_trampoline

    args += ["-escript", "main", "erlang_escript_trampoline"]

    escript_build_spec = {
        "artifacts": artifacts,
        "emu_args": args,
        "output": output.as_output(),
    }

    spec_file = ctx.actions.write(
        "escript_build_spec.term",
        to_term_args(escript_build_spec),
    )

    create_escript(ctx, spec_file, toolchain, artifacts.values(), output, escript_name)

    escript_cmd = cmd_args(
        [
            toolchain.otp_binaries.escript,
            output,
        ],
    )

    return [
        DefaultInfo(default_output = output),
        RunInfo(escript_cmd),
    ]

def create_escript(
        ctx: AnalysisContext,
        spec_file: Artifact,
        toolchain: Toolchain,
        files: list[Artifact],
        output: Artifact,
        escript_name: str) -> None:
    """ build the escript with the escript builder tool
    """
    script = toolchain.escript_builder

    escript_build_cmd = cmd_args(
        [
            toolchain.otp_binaries.escript,
            script,
            spec_file,
        ],
    )
    escript_build_cmd.hidden(output.as_output())
    escript_build_cmd.hidden(files)

    erlang_build.utils.run_with_env(
        ctx,
        toolchain,
        escript_build_cmd,
        category = "escript",
        identifier = action_identifier(toolchain, escript_name),
    )
    return None

def _escript_name(ctx: AnalysisContext) -> str:
    if ctx.attrs.script_name:
        return ctx.attrs.script_name
    else:
        return ctx.attrs.name + ".escript"

def _main_module(ctx: AnalysisContext) -> str:
    if ctx.attrs.main_module:
        return ctx.attrs.main_module
    else:
        return ctx.attrs.name

def build_escript_unbundled_trampoline(ctx: AnalysisContext, toolchain, config_files: list[Artifact]) -> Artifact:
    data = cmd_args()

    data.add("#!/usr/bin/env escript")
    data.add("%% -*- erlang -*-")
    data.add("%%! {}".format(" ".join(ctx.attrs.emu_args)))

    data.add("-module('{}').".format(_escript_name(ctx)))
    data.add("-export([main/1]).")
    data.add("main(Args) ->")
    data.add("EscriptDir = filename:dirname(escript:script_name()),")
    _add_config_files_code_to_erl(data, config_files)
    data.add('    EBinDirs = filelib:wildcard(filename:join([EscriptDir, "lib", "*", "ebin"])),')
    data.add("    code:add_paths(EBinDirs),")
    data.add("    {}:main(Args).".format(_main_module(ctx)))
    _add_parse_bin(data)

    return ctx.actions.write(
        paths.join(erlang_build.utils.build_dir(toolchain), "run.escript"),
        data,
        is_executable = True,
    )

def build_escript_bundled_trampoline(ctx: AnalysisContext, toolchain, config_files: list[Artifact]) -> Artifact:
    data = cmd_args()

    data.add("-module('erlang_escript_trampoline').")
    data.add("-export([main/1]).")
    data.add("main(Args) ->")
    data.add("EscriptDir = escript:script_name(),")
    _add_config_files_code_to_erl(data, config_files)
    data.add("    {}:main(Args).".format(_main_module(ctx)))
    _add_parse_bin(data)
    escript_trampoline_erl = ctx.actions.write(
        paths.join(erlang_build.utils.build_dir(toolchain), "erlang_escript_trampoline.erl"),
        data,
    )
    my_output = ctx.actions.declare_output("erlang_escript_trampoline.beam")

    ctx.actions.run(
        cmd_args(
            toolchain.otp_binaries.erlc,
            "-o",
            cmd_args(my_output.as_output()).parent(),
            escript_trampoline_erl,
        ),
        category = "erlc_escript_trampoline",
    )

    return my_output

def _ebin_path(file: Artifact, app_name: str) -> str:
    return paths.join(app_name, "ebin", file.basename)

def _priv_path(app_name: str) -> str:
    return paths.join(app_name, "priv")

def _escript_config_files(ctx: AnalysisContext) -> list[Artifact]:
    config_files = []
    for config_dep in ctx.attrs.configs:
        for artifact in config_dep[DefaultInfo].default_outputs + config_dep[DefaultInfo].other_outputs:
            (_, ext) = paths.split_extension(artifact.short_path)
            if ext == ".config":
                config_files.append(artifact)
    return config_files

def _add_config_files_code_to_erl(cmd: cmd_args, config_files: list[Artifact]) -> None:
    cmd.add("ConfigFiles = [")
    for i in range(0, len(config_files)):
        cmd.add(cmd_args("\"", config_files[i].short_path, "\"", delimiter = ""))
        if i < len(config_files) - 1:
            cmd.add(",")
    cmd.add("],")
    cmd.add("[begin ")
    cmd.add("{ok, AppConfigBin, _FullName} = erl_prim_loader:get_file(filename:join(EscriptDir, ConfigFile)),")
    cmd.add("{ok, AppConfig} = parse_bin(AppConfigBin), ")
    cmd.add(" ok = application:set_env(AppConfig, [{persistent, true}])")
    cmd.add("end || ConfigFile <- ConfigFiles],")

def _add_parse_bin(cmd: cmd_args) -> None:
    cmd.add("""
parse_bin(<<"">>) ->
    [];
parse_bin(Bin) ->
        {ok, Tokens, _} = erl_scan:string(binary_to_list(Bin)),
        erl_parse:parse_term(Tokens).
        """)
