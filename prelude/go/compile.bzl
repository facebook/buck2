# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")

# @unused this comment is to make the linter happy.  The linter thinks
# GoCoverageMode is unused despite it being used in the function signature of
# multiple functions.
load(":coverage.bzl", "GoCoverageMode", "cover_srcs")
load(
    ":packages.bzl",
    "GoPkg",  # @Unused used as type
    "make_importcfg",
    "merge_pkgs",
    "pkg_artifacts",
)
load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_cmd_args")

# Provider wrapping packages used for compiling.
GoPkgCompileInfo = provider(fields = {
    "pkgs": provider_field(typing.Any, default = None),  # dict[str, GoPkg]
})

# Provider for test targets that test a library. Contains information for
# compiling the test and library code together as expected by go.
GoTestInfo = provider(
    # @unsorted-dict-items
    fields = {
        "deps": provider_field(typing.Any, default = None),  # [Dependency]
        "srcs": provider_field(typing.Any, default = None),  # ["source"]
        "pkg_name": provider_field(typing.Any, default = None),  # str
    },
)

def get_inherited_compile_pkgs(deps: list[Dependency]) -> dict[str, GoPkg]:
    return merge_pkgs([d[GoPkgCompileInfo].pkgs for d in deps if GoPkgCompileInfo in d])

def get_filtered_srcs(ctx: AnalysisContext, srcs: list[Artifact], tests: bool = False, force_disable_cgo: bool = False) -> cmd_args:
    """
    Filter the input sources based on build pragma
    """

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]

    # Delegate to `go list` to filter out srcs with incompatible `// +build`
    # pragmas.
    filtered_srcs = ctx.actions.declare_output("__filtered_srcs__.txt")
    srcs_dir = ctx.actions.symlinked_dir(
        "__srcs__",
        {src.short_path: src for src in srcs},
    )
    filter_cmd = get_toolchain_cmd_args(go_toolchain, go_root = True, force_disable_cgo = force_disable_cgo)
    filter_cmd.add(go_toolchain.filter_srcs)
    filter_cmd.add(cmd_args(go_toolchain.go, format = "--go={}"))
    if tests:
        filter_cmd.add("--tests")
    filter_cmd.add(cmd_args(",".join(go_toolchain.tags + ctx.attrs._tags), format = "--tags={}"))
    filter_cmd.add(cmd_args(filtered_srcs.as_output(), format = "--output={}"))
    filter_cmd.add(srcs_dir)
    ctx.actions.run(filter_cmd, category = "go_filter_srcs")

    # Add filtered srcs to compile command.
    return cmd_args(filtered_srcs, format = "@{}").hidden(srcs).hidden(srcs_dir)

def _assemble_cmd(
        ctx: AnalysisContext,
        pkg_name: str,
        flags: list[str] = [],
        shared: bool = False) -> cmd_args:
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    cmd = cmd_args()
    cmd.add(go_toolchain.assembler)
    cmd.add(go_toolchain.assembler_flags)
    cmd.add(flags)
    cmd.add("-p", pkg_name)
    if shared:
        cmd.add("-shared")

    return cmd

def _compile_cmd(
        ctx: AnalysisContext,
        pkg_name: str,
        pkgs: dict[str, Artifact] = {},
        deps: list[Dependency] = [],
        flags: list[str] = [],
        shared: bool = False,
        race: bool = False) -> cmd_args:
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]

    cmd = cmd_args()
    cmd.add(go_toolchain.compiler)
    cmd.add(go_toolchain.compiler_flags)
    cmd.add("-p", pkg_name)
    cmd.add("-pack")
    cmd.add("-nolocalimports")
    cmd.add(flags)
    cmd.add("-buildid=")

    # Add shared/static flags.
    if shared:
        cmd.add("-shared")

    if race:
        cmd.add("-race")

    # Add Go pkgs inherited from deps to compiler search path.
    all_pkgs = merge_pkgs([
        pkgs,
        pkg_artifacts(get_inherited_compile_pkgs(deps)),
    ])

    importcfg = make_importcfg(ctx, pkg_name, all_pkgs, with_importmap = True)

    cmd.add("-importcfg", importcfg)

    return cmd

def compile(
        ctx: AnalysisContext,
        pkg_name: str,
        srcs: cmd_args,
        pkgs: dict[str, Artifact] = {},
        deps: list[Dependency] = [],
        compile_flags: list[str] = [],
        assemble_flags: list[str] = [],
        shared: bool = False,
        race: bool = False,
        coverage_mode: GoCoverageMode | None = None) -> GoPkg:
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    output = ctx.actions.declare_output(paths.basename(pkg_name) + ".a")

    cmd = get_toolchain_cmd_args(go_toolchain)
    cmd.add(go_toolchain.compile_wrapper)
    cmd.add(cmd_args(output.as_output(), format = "--output={}"))
    cmd.add(cmd_args(_compile_cmd(ctx, pkg_name, pkgs, deps, compile_flags, shared = shared, race = race), format = "--compiler={}"))
    cmd.add(cmd_args(_assemble_cmd(ctx, pkg_name, assemble_flags, shared = shared), format = "--assembler={}"))
    cmd.add(cmd_args(go_toolchain.packer, format = "--packer={}"))
    if ctx.attrs.embedcfg:
        cmd.add(cmd_args(ctx.attrs.embedcfg, format = "--embedcfg={}"))

    argsfile = ctx.actions.declare_output(pkg_name + ".go.argsfile")

    coverage_vars = None
    if coverage_mode != None:
        if race and coverage_mode != GoCoverageMode("atomic"):
            fail("`coverage_mode` must be `atomic` when `race=True`")
        cov_res = cover_srcs(ctx, pkg_name, coverage_mode, srcs, shared)
        srcs = cov_res.srcs
        coverage_vars = cov_res.variables

    srcs_args = cmd_args(srcs)
    ctx.actions.write(argsfile.as_output(), srcs_args, allow_args = True)

    cmd.add(cmd_args(argsfile, format = "@{}").hidden([srcs_args]))

    identifier = paths.basename(pkg_name)
    if shared:
        identifier += "[shared]"
    if coverage_mode:
        identifier += "[coverage_" + coverage_mode.value + "]"

    ctx.actions.run(cmd, category = "go_compile", identifier = identifier)

    return GoPkg(pkg = output, coverage_vars = coverage_vars)
