# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:paths.bzl",
    "paths",
)
load(
    ":erlang_info.bzl",
    "ErlangMultiVersionToolchainInfo",
    "ErlangOTPBinariesInfo",
    "ErlangParseTransformInfo",
    "ErlangToolchainInfo",
    "Tool",
    "Tools",
)

Toolchain = ErlangToolchainInfo

ToolchainUtillInfo = provider(
    # @unsorted-dict-items
    fields = {
        "app_src_script": provider_field(Artifact),
        "boot_script_builder": provider_field(Artifact),
        "core_parse_transforms": provider_field(list[Dependency]),
        "dependency_analyzer": provider_field(Artifact),
        "dependency_finalizer": provider_field(Artifact),
        "erlc_trampoline": provider_field(Artifact),
        "escript_trampoline": provider_field(Artifact),
        "escript_builder": provider_field(Artifact),
        "release_variables_builder": provider_field(Artifact),
        "include_erts": provider_field(Artifact),
        "utility_modules": provider_field(list[Artifact]),
    },
)

def select_toolchains(ctx: AnalysisContext) -> dict[str, Toolchain]:
    """helper returning toolchains"""
    return ctx.attrs._toolchain[ErlangMultiVersionToolchainInfo].toolchains

def get_primary(ctx: AnalysisContext) -> str:
    return ctx.attrs._toolchain[ErlangMultiVersionToolchainInfo].primary

def get_primary_tools(ctx: AnalysisContext) -> Tools:
    return (get_primary_toolchain(ctx)).otp_binaries

def get_primary_toolchain(ctx: AnalysisContext) -> Toolchain:
    return (select_toolchains(ctx)[get_primary(ctx)])

def _multi_version_toolchain_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchains = {}
    for toolchain in ctx.attrs.targets:
        toolchain_info = toolchain[ErlangToolchainInfo]
        toolchains[toolchain_info.name] = toolchain_info
    return [
        DefaultInfo(),
        ErlangMultiVersionToolchainInfo(
            toolchains = toolchains,
            primary = ctx.attrs.targets[0][ErlangToolchainInfo].name,
        ),
    ]

multi_version_toolchain_rule = rule(
    impl = _multi_version_toolchain_impl,
    attrs = {
        "targets": attrs.list(attrs.dep()),
    },
    is_toolchain_rule = True,
)

def _config_erlang_toolchain_impl(ctx: AnalysisContext) -> list[Provider]:
    """ rule for erlang toolchain
    """

    # split the options string to get a list of options
    erl_opts = ctx.attrs.erl_opts.split()
    emu_flags = ctx.attrs.emu_flags.split()

    # get otp binaries
    binaries_info = ctx.attrs.otp_binaries[ErlangOTPBinariesInfo]
    erl = cmd_args([binaries_info.erl] + emu_flags)
    erlc = cmd_args(binaries_info.erlc, hidden = binaries_info.erl)
    escript = cmd_args(binaries_info.escript, hidden = binaries_info.erl)
    otp_binaries = Tools(
        name = ctx.attrs.name,
        erl = erl,
        erlc = erlc,
        escript = escript,
        _tools_binaries = binaries_info,
    )

    # extract utility artefacts
    utils = ctx.attrs.toolchain_utilities[ToolchainUtillInfo]

    core_parse_transforms = _gen_parse_transforms(
        ctx,
        otp_binaries.erlc,
        utils.core_parse_transforms,
    )

    parse_transforms = _gen_parse_transforms(
        ctx,
        otp_binaries.erlc,
        ctx.attrs.parse_transforms,
    )
    intersection = [key for key in parse_transforms if key in core_parse_transforms]
    if len(intersection):
        fail("conflicting parse_transform with core parse_transform found: %s" % (repr(intersection),))

    utility_modules = _gen_util_beams(ctx, utils.utility_modules, otp_binaries.erlc)

    app_src_script = _gen_toolchain_script(ctx, utils.app_src_script, otp_binaries)
    boot_script_builder = _gen_toolchain_script(ctx, utils.boot_script_builder, otp_binaries)
    dependency_analyzer = _gen_toolchain_script(ctx, utils.dependency_analyzer, otp_binaries)
    dependency_finalizer = _gen_toolchain_script(ctx, utils.dependency_finalizer, otp_binaries)
    escript_builder = _gen_toolchain_script(ctx, utils.escript_builder, otp_binaries)
    release_variables_builder = _gen_toolchain_script(ctx, utils.release_variables_builder, otp_binaries)
    include_erts = _gen_toolchain_script(ctx, utils.include_erts, otp_binaries)

    return [
        DefaultInfo(),
        ErlangToolchainInfo(
            name = ctx.attrs.name,
            app_src_script = app_src_script,
            boot_script_builder = boot_script_builder,
            dependency_analyzer = dependency_analyzer,
            dependency_finalizer = dependency_finalizer,
            erl_opts = erl_opts,
            env = ctx.attrs.env,
            emu_flags = emu_flags,
            erlc_trampoline = utils.erlc_trampoline,
            escript_trampoline = utils.escript_trampoline,
            escript_builder = escript_builder,
            otp_binaries = otp_binaries,
            release_variables_builder = release_variables_builder,
            include_erts = include_erts,
            core_parse_transforms = core_parse_transforms,
            parse_transforms = parse_transforms,
            parse_transforms_filters = ctx.attrs.parse_transforms_filters,
            utility_modules = utility_modules,
        ),
    ]

def _configured_otp_binaries_impl(ctx: AnalysisContext) -> list[Provider]:
    name = ctx.attrs.name
    tools = get_primary_tools(ctx)
    bin_dir = ctx.actions.symlinked_dir(
        name,
        {
            "erl": tools._tools_binaries.erl,
            "erlc": tools._tools_binaries.erlc,
            "escript": tools._tools_binaries.escript,
        },
    )
    return [
        DefaultInfo(
            default_output = bin_dir,
            sub_targets = {
                "erl": [DefaultInfo(default_output = tools._tools_binaries.erl), RunInfo(tools.erl)],
                "erlc": [DefaultInfo(default_output = tools._tools_binaries.erlc), RunInfo(tools.erlc)],
                "escript": [DefaultInfo(default_output = tools._tools_binaries.escript), RunInfo(tools.escript)],
            },
        ),
    ]

configured_otp_binaries = rule(
    impl = _configured_otp_binaries_impl,
    attrs = {
        "_toolchain": attrs.dep(),
    },
)

def _gen_parse_transforms(ctx: AnalysisContext, erlc: Tool, parse_transforms: list[Dependency]) -> dict[str, (Artifact, Artifact)]:
    transforms = {}
    for dep in parse_transforms:
        src = dep[ErlangParseTransformInfo].source
        extra = dep[ErlangParseTransformInfo].extra_files
        module_name, _ = paths.split_extension(src.basename)
        if module_name in transforms:
            fail("ambiguous global parse_transforms defined: %s", (module_name,))
        transforms[module_name] = _gen_parse_transform_beam(ctx, src, extra, erlc)
    return transforms

def _gen_parse_transform_beam(
        ctx: AnalysisContext,
        src: Artifact,
        extra: list[Artifact],
        erlc: Tool) -> (Artifact, Artifact):
    name, _ext = paths.split_extension(src.basename)

    # install resources
    resource_dir = ctx.actions.symlinked_dir(
        paths.join(name, "resources"),
        {infile.basename: infile for infile in extra},
    )

    # build beam
    output = ctx.actions.declare_output(name, name + ".beam")
    _compile_toolchain_module(ctx, src, output.as_output(), erlc)

    return output, resource_dir

default_toolchain_script_args_pre = cmd_args(
    "+A0",
    "+S1:1",
    "+sbtu",
    "+MMscs",
    "8",
    "+MMsco",
    "false",
    "-mode",
    "minimal",
    "-noinput",
    "-noshell",
    "-eval",
)
default_toolchain_script_args_post = cmd_args("-s", "erlang", "halt", "--")

def _gen_toolchain_script(ctx: AnalysisContext, script: Artifact, tools: Tools) -> Tool:
    name, _ext = paths.split_extension(script.basename)
    out = ctx.actions.declare_output(name, name + ".beam")
    _compile_toolchain_module(ctx, script, out.as_output(), tools.erlc)
    eval = cmd_args(name, ":main(init:get_plain_arguments())", delimiter = "")
    return cmd_args(
        tools.erl,
        cmd_args(out, parent = 1, prepend = "-pa"),
        default_toolchain_script_args_pre,
        eval,
        default_toolchain_script_args_post,
    )

config_erlang_toolchain_rule = rule(
    impl = _config_erlang_toolchain_impl,
    attrs = {
        "core_parse_transforms": attrs.list(attrs.dep(), default = ["@prelude//erlang/toolchain:transform_project_root"]),
        "emu_flags": attrs.string(default = ""),
        "env": attrs.dict(key = attrs.string(), value = attrs.string(), default = {}),
        "erl_opts": attrs.string(default = ""),
        "otp_binaries": attrs.dep(),
        "parse_transforms": attrs.list(attrs.dep()),
        "parse_transforms_filters": attrs.dict(key = attrs.string(), value = attrs.list(attrs.string())),
        "toolchain_utilities": attrs.dep(default = "@prelude//erlang/toolchain:toolchain_utilities"),
    },
)

def _gen_util_beams(
        ctx: AnalysisContext,
        sources: list[Artifact],
        erlc: Tool) -> Artifact:
    beams = []
    for src in sources:
        output = ctx.actions.declare_output(paths.join(
            "__build",
            paths.replace_extension(src.basename, ".beam"),
        ))
        _compile_toolchain_module(ctx, src, output.as_output(), erlc)
        beams.append(output)

    beam_dir = ctx.actions.symlinked_dir(
        "utility_modules",
        {beam.basename: beam for beam in beams},
    )

    return beam_dir

def _compile_toolchain_module(
        ctx: AnalysisContext,
        src: Artifact,
        out: OutputArtifact,
        erlc: Tool):
    # NOTE: since we do NOT define +debug_info, this is hermetic
    ctx.actions.run(
        [erlc, "+deterministic", "-o", cmd_args(out, parent = 1), src],
        category = "erlc",
        identifier = src.short_path,
    )

# Parse Transform

def erlang_otp_binaries_impl(ctx: AnalysisContext):
    erl = ctx.attrs.erl
    erlc = ctx.attrs.erlc
    escript = ctx.attrs.escript
    return [
        DefaultInfo(),
        ErlangOTPBinariesInfo(
            erl = erl,
            erlc = erlc,
            escript = escript,
        ),
    ]

erlang_parse_transform = rule(
    impl = lambda ctx: [
        DefaultInfo(),
        ErlangParseTransformInfo(
            source = ctx.attrs.src,
            extra_files = ctx.attrs.extra_files,
        ),
    ],
    attrs = {
        "extra_files": attrs.list(attrs.source(), default = []),
        "src": attrs.source(),
    },
)

def _toolchain_utils(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        ToolchainUtillInfo(
            app_src_script = ctx.attrs.app_src_script,
            boot_script_builder = ctx.attrs.boot_script_builder,
            core_parse_transforms = ctx.attrs.core_parse_transforms,
            dependency_analyzer = ctx.attrs.dependency_analyzer,
            dependency_finalizer = ctx.attrs.dependency_finalizer,
            erlc_trampoline = ctx.attrs.erlc_trampoline,
            escript_trampoline = ctx.attrs.escript_trampoline,
            escript_builder = ctx.attrs.escript_builder,
            release_variables_builder = ctx.attrs.release_variables_builder,
            include_erts = ctx.attrs.include_erts,
            utility_modules = ctx.attrs.utility_modules,
        ),
    ]

toolchain_utilities = rule(
    impl = _toolchain_utils,
    attrs = {
        "app_src_script": attrs.source(),
        "boot_script_builder": attrs.source(),
        "core_parse_transforms": attrs.list(attrs.dep()),
        "dependency_analyzer": attrs.source(),
        "dependency_finalizer": attrs.source(),
        "erlc_trampoline": attrs.source(),
        "escript_builder": attrs.source(),
        "escript_trampoline": attrs.source(),
        "include_erts": attrs.source(),
        "release_variables_builder": attrs.source(),
        "utility_modules": attrs.list(attrs.source()),
    },
)
