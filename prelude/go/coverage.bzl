# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_env_vars")

GoCoverageMode = enum(
    "set",
    "count",
    "atomic",
)

# The result of running `go tool cover` on the input sources.
GoCoverResult = record(
    # All sources after annotating non-`_test.go` sources.  This will be a
    # combination of the original `*_test.go` sources and the annotated non-
    # `*_test.go` sources.
    srcs = field(cmd_args),
    # Coverage variables we used when annotating non-test sources.
    variables = field(cmd_args),
)

def cover_srcs(ctx: AnalysisContext, pkg_name: str, mode: GoCoverageMode, srcs: cmd_args, shared: bool) -> GoCoverResult:
    path = pkg_name + "_static_" + mode.value
    if shared:
        path = pkg_name + "shared_" + mode.value
    out_covered_src_dir = ctx.actions.declare_output("__covered_" + path + "_srcs__", dir = True)
    out_srcs_argsfile = ctx.actions.declare_output("covered_" + path + "_srcs.txt")
    out_coverage_vars_argsfile = ctx.actions.declare_output("coverage_" + path + "_vars.txt")

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    cmd = cmd_args()
    cmd.add(go_toolchain.cover_srcs)
    cmd.add("--cover", go_toolchain.cover)
    cmd.add("--coverage-mode", mode.value)
    cmd.add("--coverage-var-argsfile", out_coverage_vars_argsfile.as_output())
    cmd.add("--covered-srcs-dir", out_covered_src_dir.as_output())
    cmd.add("--out-srcs-argsfile", out_srcs_argsfile.as_output())
    cmd.add("--pkg-name", pkg_name)
    cmd.add(srcs)
    ctx.actions.run(cmd, category = "go_cover", identifier = path)

    return GoCoverResult(
        srcs = cmd_args(out_srcs_argsfile, format = "@{}", hidden = [out_covered_src_dir, srcs]),
        variables = cmd_args(out_coverage_vars_argsfile, format = "@{}"),
    )

def cover_srcs_v2(
        ctx: AnalysisContext,
        pkg_name: str,
        pkg_import_path: str,
        go_files: list[Artifact],
        coverage_mode: GoCoverageMode | None) -> (list[Artifact], str | cmd_args, Artifact | None):
    if coverage_mode == None:
        return go_files, "", None

    if len(go_files) == 0:
        return go_files, "", None

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env = get_toolchain_env_vars(go_toolchain)

    cover_meta_file = ctx.actions.declare_output("cover_meta.json")
    out_config_file = ctx.actions.declare_output("out_config.json")

    # Based on https://pkg.go.dev/cmd/internal/cov/covcmd#CoverPkgConfig
    pkgcfg = {
        "EmitMetaFile": cover_meta_file,
        "Granularity": "perblock",
        "Local": False,
        "ModulePath": "",
        "OutConfig": out_config_file,
        "PkgName": pkg_name,
        "PkgPath": pkg_import_path,
    }
    pkgcfg_file = ctx.actions.write_json("pkg.cfg", pkgcfg)

    var = "Var_" + sha256(pkg_import_path)
    instrum_vars_file = ctx.actions.declare_output("with_instrumentation", "instrum_vars.go")
    instrum_go_files = [
        ctx.actions.declare_output("with_instrumentation", go_file.short_path)
        for go_file in go_files
    ]
    instrum_all_files = [instrum_vars_file] + instrum_go_files
    file_to_var = {
        go_file.short_path: var
        for go_file in go_files
    }
    outfilelist = ctx.actions.write("outfilelist.txt", cmd_args(instrum_all_files, ""))

    cover_cmd = [
        go_toolchain.cover,
        ["-mode", coverage_mode.value],
        ["-var", var],
        cmd_args(["-outfilelist", outfilelist], hidden = [f.as_output() for f in instrum_all_files]),
        cmd_args(["-pkgcfg", pkgcfg_file], hidden = [cover_meta_file.as_output(), out_config_file.as_output()]),
        go_files,
    ]

    ctx.actions.run(cover_cmd, env = env, category = "go_cover", identifier = pkg_import_path)

    coverage_vars_out = ""
    if len(file_to_var) > 0:
        # convert file_to_var to argsfile for compatibility with python implementation
        cover_pkg = "{}:{}".format(pkg_import_path, ",".join(["{}={}".format(name, var) for name, var in file_to_var.items()]))
        coverage_vars_out = cmd_args("--cover-pkgs", cover_pkg)

    return instrum_all_files, coverage_vars_out, out_config_file
