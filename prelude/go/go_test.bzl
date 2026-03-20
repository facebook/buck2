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
load(":compile.bzl", "GoTestInfo")
load(":coverage.bzl", "GoCoverageMode")
load(":link.bzl", "GoBuildMode", "get_inherited_link_pkgs", "link")
load(":package_builder.bzl", "GoBuildConfig", "GoSourceInputs", "declare_package_build")
load(":packages.bzl", "go_attr_pkg_name")
load(":toolchain.bzl", "evaluate_cgo_enabled")

def _gen_test_main(
        ctx: AnalysisContext,
        pkg_import_path: str,
        coverage_mode: [GoCoverageMode, None],
        cover_packages: list[str],  # packages those are included for coverage
        test_go_files_argsfile: Artifact) -> Artifact:
    """
    Generate a `main.go` which calls tests from the given sources.
    """
    cover_pkgs_argsfile = ctx.actions.declare_output("cover_pkgs_argsfile", has_content_based_path = True)
    ctx.actions.write(cover_pkgs_argsfile, [["--cover-pkgs", pkg] for pkg in cover_packages])

    output = ctx.actions.declare_output("main.go", has_content_based_path = True)
    cmd = []
    cmd.append(ctx.attrs._testmaingen[RunInfo])

    # if ctx.attrs.coverage_mode:
    cmd.append(cmd_args(output.as_output(), format = "--output={}"))
    cmd.append(cmd_args(pkg_import_path, format = "--import-path={}"))
    if coverage_mode != None:
        cmd.extend(["--cover-mode", coverage_mode.value])
    cmd.append(cmd_args(cover_pkgs_argsfile, format = "@{}"))
    cmd.append(cmd_args(test_go_files_argsfile, format = "@{}"))
    ctx.actions.run(cmd_args(cmd), category = "go_test_main_gen")
    return output

def is_subpackage_of(other_pkg_import_path: str, pkg_import_path: str) -> bool:
    return pkg_import_path == other_pkg_import_path or other_pkg_import_path.startswith(pkg_import_path + "/")

def go_test_impl(ctx: AnalysisContext) -> list[Provider]:
    cxx_toolchain_available = CxxToolchainInfo in ctx.attrs._cxx_toolchain
    pkg_import_path = go_attr_pkg_name(ctx)
    cgo_enabled = evaluate_cgo_enabled(cxx_toolchain_available, ctx.attrs.cgo_enabled)

    deps = ctx.attrs.deps
    srcs = ctx.attrs.srcs

    # Copy the srcs, deps and pkg_import_path from the target library when set. The
    # library code gets compiled together with the tests.
    if ctx.attrs.target_under_test:
        lib = ctx.attrs.target_under_test[GoTestInfo]
        srcs += lib.srcs
        deps += lib.deps

        # TODO: should we assert that pkg_import_path != None here?
        pkg_import_path = lib.pkg_import_path

    # If coverage is enabled for this test, we need to preprocess the sources
    # with the Go cover tool.
    coverage_mode = GoCoverageMode(ctx.attrs._coverage_mode) if ctx.attrs._coverage_mode else None
    cgo_build_context = get_cgo_build_context(ctx)
    pkgs = {}

    # Compile all tests into a package.
    tests, tests_pkg_info, test_go_files_argsfile = declare_package_build(
        ctx = ctx,
        pkg_import_path = pkg_import_path,
        main = False,
        sources = GoSourceInputs(
            srcs = srcs,
            embed_srcs = ctx.attrs.embed_srcs,
            package_root = ctx.attrs.package_root,
        ),
        cgo_build_context = cgo_build_context,
        config = GoBuildConfig(
            compiler_flags = ctx.attrs.compiler_flags,
            build_tags = ctx.attrs._build_tags,
            coverage_mode = coverage_mode,
            with_tests = True,
            cgo_enabled = cgo_enabled,
        ),
        pkgs = pkgs,
        deps = deps,
    )

    cover_packages = []
    if coverage_mode != None:
        cover_packages.append(pkg_import_path)

        # Get all packages that are linked to the test (i.e. the entire dependency tree)
        for import_path in get_inherited_link_pkgs(deps):
            cover_packages.append(import_path)

    pkgs[pkg_import_path] = tests

    # Generate a 'main.go' file (test runner) which runs the actual tests from the package above.
    # Build the it as a separate package (<foo>.test) - which imports and invokes the test package.
    gen_main = _gen_test_main(ctx, pkg_import_path, coverage_mode, cover_packages, test_go_files_argsfile)
    main, _, _ = declare_package_build(
        ctx = ctx,
        pkg_import_path = pkg_import_path + ".test",
        main = True,
        sources = GoSourceInputs(
            srcs = [gen_main],
            package_root = "",
        ),
        cgo_build_context = None,
        config = GoBuildConfig(
            cgo_enabled = cgo_enabled,
        ),
        pkgs = pkgs,
        cgo_gen_dir_name = "cgo_gen_test_main",
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
