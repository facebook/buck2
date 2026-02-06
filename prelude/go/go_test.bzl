# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
)
load("@prelude//test:inject_test_run_info.bzl", "inject_test_run_info")
load(
    "@prelude//tests:re_utils.bzl",
    "get_re_executors_from_props",
)
load(
    "@prelude//utils:utils.bzl",
    "map_val",
    "value_or",
)
load(":cgo_builder.bzl", "get_cgo_build_context")
load(":compile.bzl", "GoTestInfo", "get_inherited_compile_pkgs")
load(":coverage.bzl", "GoCoverageMode")
load(":link.bzl", "GoBuildMode", "link")
load(":package_builder.bzl", "build_package")
load(":packages.bzl", "go_attr_pkg_name")
load(":toolchain.bzl", "evaluate_cgo_enabled")

def _gen_test_main(
        ctx: AnalysisContext,
        pkg_name: str,
        coverage_mode: [GoCoverageMode, None],
        coverage_vars: dict[str, cmd_args],
        test_go_files: cmd_args) -> Artifact:
    """
    Generate a `main.go` which calls tests from the given sources.
    """
    output = ctx.actions.declare_output("main.go", has_content_based_path = True)
    cmd = []
    cmd.append(ctx.attrs._testmaingen[RunInfo])

    # if ctx.attrs.coverage_mode:
    # cmd.append(cmd_args(ctx.attrs.coverage_mode, format = "--cover-mode={}"))
    cmd.append(cmd_args(output.as_output(), format = "--output={}"))
    cmd.append(cmd_args(pkg_name, format = "--import-path={}"))
    if coverage_mode != None:
        cmd.extend(["--cover-mode", coverage_mode.value])
    for _, vars in coverage_vars.items():
        cmd.append(vars)
    cmd.append(test_go_files)
    ctx.actions.run(cmd_args(cmd), category = "go_test_main_gen")
    return output

def is_subpackage_of(other_pkg_name: str, pkg_name: str) -> bool:
    return pkg_name == other_pkg_name or other_pkg_name.startswith(pkg_name + "/")

def go_test_impl(ctx: AnalysisContext) -> list[Provider]:
    cxx_toolchain_available = CxxToolchainInfo in ctx.attrs._cxx_toolchain
    pkg_name = go_attr_pkg_name(ctx)
    cgo_enabled = evaluate_cgo_enabled(cxx_toolchain_available, ctx.attrs.cgo_enabled)

    deps = ctx.attrs.deps
    srcs = ctx.attrs.srcs

    # Copy the srcs, deps and pkg_name from the target library when set. The
    # library code gets compiled together with the tests.
    if ctx.attrs.target_under_test:
        lib = ctx.attrs.target_under_test[GoTestInfo]
        srcs += lib.srcs
        deps += lib.deps

        # TODO: should we assert that pkg_name != None here?
        pkg_name = lib.pkg_name

    # If coverage is enabled for this test, we need to preprocess the sources
    # with the Go cover tool.
    coverage_mode = GoCoverageMode(ctx.attrs._coverage_mode) if ctx.attrs._coverage_mode else None
    cgo_build_context = get_cgo_build_context(ctx)
    coverage_vars = {}
    pkgs = {}

    # Compile all tests into a package.
    tests, tests_pkg_info = build_package(
        ctx = ctx,
        pkg_name = pkg_name,
        main = False,
        srcs = srcs,
        package_root = ctx.attrs.package_root,
        cgo_build_context = cgo_build_context,
        deps = deps,
        pkgs = pkgs,
        compiler_flags = ctx.attrs.compiler_flags,
        build_tags = ctx.attrs._build_tags,
        coverage_mode = coverage_mode,
        embedcfg = ctx.attrs.embedcfg,
        embed_srcs = ctx.attrs.embed_srcs,
        with_tests = True,
        cgo_enabled = cgo_enabled,
    )

    if coverage_mode != None:
        coverage_vars[pkg_name] = tests.coverage_vars

        # Get all packages that are linked to the test (i.e. the entire dependency tree)
        for name, pkg in get_inherited_compile_pkgs(deps).items():
            if ctx.label != None and is_subpackage_of(name, ctx.label.package):
                coverage_vars[name] = pkg.coverage_vars
                pkgs[name] = pkg

    pkgs[pkg_name] = tests

    # Generate a 'main.go' file (test runner) which runs the actual tests from the package above.
    # Build the it as a separate package (<foo>.test) - which imports and invokes the test package.
    gen_main = _gen_test_main(ctx, pkg_name, coverage_mode, coverage_vars, tests.test_go_files)
    main, _ = build_package(
        ctx = ctx,
        pkg_name = pkg_name + ".test",
        main = True,
        srcs = [gen_main],
        package_root = "",
        cgo_build_context = None,
        pkgs = pkgs,
        coverage_mode = None,
        cgo_gen_dir_name = "cgo_gen_test_main",
        cgo_enabled = cgo_enabled,
    )

    # Link the above into a Go binary.
    (bin, runtime_files, external_debug_info) = link(
        ctx = ctx,
        main = main,
        cgo_enabled = cgo_enabled,
        pkgs = pkgs,
        deps = deps,
        link_style = value_or(map_val(LinkStyle, ctx.attrs.link_style), LinkStyle("static")),
        build_mode = GoBuildMode(value_or(ctx.attrs.build_mode, "exe")),
        linker_flags = ctx.attrs.linker_flags,
        external_linker_flags = ctx.attrs.external_linker_flags,
    )

    # As per v1, copy in resources next to binary.
    copied_resources = []
    for resource in ctx.attrs.resources:
        copied_resources.append(ctx.actions.copy_file(resource.short_path, resource))

    run_cmd = cmd_args(bin, hidden = [runtime_files, external_debug_info] + copied_resources)

    # Setup RE executors based on the `remote_execution` param.
    re_executor, executor_overrides = get_re_executors_from_props(ctx)

    return inject_test_run_info(
        ctx,
        ExternalRunnerTestInfo(
            type = "go",
            command = [run_cmd],
            env = ctx.attrs.env,
            labels = ctx.attrs.labels,
            contacts = ctx.attrs.contacts,
            default_executor = re_executor,
            executor_overrides = executor_overrides,
            # FIXME: Consider setting to true
            run_from_project_root = re_executor != None,
            use_project_relative_paths = re_executor != None,
        ),
    ) + [
        DefaultInfo(
            default_output = bin,
            other_outputs = [gen_main] + runtime_files + external_debug_info,
        ),
        tests_pkg_info,
    ]
