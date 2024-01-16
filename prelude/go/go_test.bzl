# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
)
load(
    "@prelude//utils:utils.bzl",
    "map_val",
    "value_or",
)
load("@prelude//test/inject_test_run_info.bzl", "inject_test_run_info")
load(":compile.bzl", "GoTestInfo", "compile", "get_filtered_srcs", "get_inherited_compile_pkgs")
load(":coverage.bzl", "GoCoverageMode", "cover_srcs")
load(":link.bzl", "link")
load(":packages.bzl", "go_attr_pkg_name", "pkg_artifact", "pkg_coverage_vars")
load(":toolchain.bzl", "GoToolchainInfo", "evaluate_cgo_enabled")

def _gen_test_main(
        ctx: AnalysisContext,
        pkg_name: str,
        coverage_mode: [GoCoverageMode, None],
        coverage_vars: dict[str, cmd_args],
        srcs: cmd_args) -> Artifact:
    """
    Generate a `main.go` which calls tests from the given sources.
    """
    output = ctx.actions.declare_output("main.go")
    cmd = cmd_args()
    cmd.add(ctx.attrs._testmaingen[RunInfo])
    if ctx.attrs.coverage_mode:
        cmd.add(cmd_args(ctx.attrs.coverage_mode, format = "--cover-mode={}"))
    cmd.add(cmd_args(output.as_output(), format = "--output={}"))
    cmd.add(cmd_args(pkg_name, format = "--import-path={}"))
    if coverage_mode != None:
        cmd.add("--cover-mode", coverage_mode.value)
    for _, vars in coverage_vars.items():
        cmd.add(vars)
    cmd.add(srcs)
    ctx.actions.run(cmd, category = "go_test_main_gen")
    return output

def is_subpackage_of(other_pkg_name: str, pkg_name: str) -> bool:
    return pkg_name == other_pkg_name or other_pkg_name.startswith(pkg_name + "/")

def go_test_impl(ctx: AnalysisContext) -> list[Provider]:
    deps = ctx.attrs.deps
    srcs = ctx.attrs.srcs
    pkg_name = go_attr_pkg_name(ctx)

    # Copy the srcs, deps and pkg_name from the target library when set. The
    # library code gets compiled together with the tests.
    if ctx.attrs.library:
        lib = ctx.attrs.library[GoTestInfo]
        srcs += lib.srcs
        deps += lib.deps

        # TODO: should we assert that pkg_name != None here?
        pkg_name = lib.pkg_name

    srcs = get_filtered_srcs(ctx, srcs, tests = True)

    # If coverage is enabled for this test, we need to preprocess the sources
    # with the Go cover tool.
    coverage_mode = None
    coverage_vars = {}
    pkgs = {}
    if ctx.attrs.coverage_mode != None:
        coverage_mode = GoCoverageMode(ctx.attrs.coverage_mode)
        cov_res = cover_srcs(ctx, pkg_name, coverage_mode, srcs, False)
        srcs = cov_res.srcs
        coverage_vars[pkg_name] = cov_res.variables

        # Get all packages that are linked to the test (i.e. the entire dependency tree)
        for name, pkg in get_inherited_compile_pkgs(deps).items():
            if ctx.label != None and is_subpackage_of(name, ctx.label.package):
                artifact = pkg_artifact(pkg, False, coverage_mode)
                vars = pkg_coverage_vars("", pkg, False, coverage_mode)
                coverage_vars[name] = vars
                pkgs[name] = artifact

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    cgo_enabled = evaluate_cgo_enabled(go_toolchain, ctx.attrs.cgo_enabled)

    # Compile all tests into a package.
    tests = compile(
        ctx,
        pkg_name,
        srcs,
        cgo_enabled = cgo_enabled,
        deps = deps,
        pkgs = pkgs,
        compile_flags = ctx.attrs.compiler_flags,
        coverage_mode = coverage_mode,
    )

    # Generate a main function which runs the tests and build that into another
    # package.
    gen_main = _gen_test_main(ctx, pkg_name, coverage_mode, coverage_vars, srcs)
    pkgs[pkg_name] = tests
    main = compile(ctx, "main", cmd_args(gen_main), cgo_enabled = cgo_enabled, pkgs = pkgs, coverage_mode = coverage_mode)

    # Link the above into a Go binary.
    (bin, runtime_files, external_debug_info) = link(
        ctx = ctx,
        main = main,
        pkgs = pkgs,
        cgo_enabled = cgo_enabled,
        deps = deps,
        link_style = value_or(map_val(LinkStyle, ctx.attrs.link_style), LinkStyle("static")),
        linker_flags = ctx.attrs.linker_flags,
        shared = False,
        coverage_mode = coverage_mode,
    )

    run_cmd = cmd_args(bin).hidden(runtime_files, external_debug_info)

    # As per v1, copy in resources next to binary.
    for resource in ctx.attrs.resources:
        run_cmd.hidden(ctx.actions.copy_file(resource.short_path, resource))

    return inject_test_run_info(
        ctx,
        ExternalRunnerTestInfo(
            type = "go",
            command = [run_cmd],
            env = ctx.attrs.env,
            labels = ctx.attrs.labels,
            contacts = ctx.attrs.contacts,
            # FIXME: Consider setting to true
            run_from_project_root = False,
        ),
    ) + [
        DefaultInfo(
            default_output = bin,
            other_outputs = [gen_main] + runtime_files + external_debug_info,
        ),
    ]
