# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//test:inject_test_run_info.bzl", "inject_test_run_info")
load(
    "@prelude//tests:re_utils.bzl",
    "get_re_executors_from_props",
)
load("@prelude//utils:utils.bzl", "from_named_set", "value_or")
load(":interface.bzl", "EntryPointKind")
load(":make_py_package.bzl", "PexProviders", "make_default_info")
load(
    ":manifest.bzl",
    "get_srcs_from_manifest",
)
load(":python.bzl", "PythonLibraryInfo")
load(":python_binary.bzl", "python_executable")
load(":python_library.bzl", "py_attr_resources", "qualify_srcs")

def _write_test_modules_list(
        ctx: AnalysisContext,
        srcs: dict[str, Artifact]) -> (str, Artifact):
    """
    Generate a python source file with a list of all test modules.
    """
    name = "__test_modules__.py"
    contents = "TEST_MODULES = [\n"
    for dst in srcs:
        root, ext = paths.split_extension(dst)
        if ext != ".py":
            fail("test sources must end with .py")
        module = root.replace("/", ".")
        contents += "    \"{}\",\n".format(module)
    contents += "]\n"
    return name, ctx.actions.write(name, contents)

def python_test_executable(ctx: AnalysisContext) -> PexProviders:
    main_module = value_or(ctx.attrs.main_module, "__test_main__")

    srcs = qualify_srcs(ctx.label, ctx.attrs.base_module, from_named_set(ctx.attrs.srcs))
    if ctx.attrs.implicit_test_library != None:
        top_level_manifest = list(ctx.attrs.implicit_test_library[PythonLibraryInfo].manifests.traverse(ordering = "preorder"))[0]
        srcs.update(qualify_srcs(ctx.label, ctx.attrs.base_module, from_named_set(get_srcs_from_manifest(top_level_manifest.srcs))))

    test_modules_name, test_modules_path = _write_test_modules_list(ctx, srcs)
    srcs[test_modules_name] = test_modules_path

    # Add in default test runner.
    srcs["__test_main__.py"] = ctx.attrs._test_main

    resources_map, standalone_resources_map = py_attr_resources(ctx)
    standalone_resources = qualify_srcs(ctx.label, ctx.attrs.base_module, standalone_resources_map)
    resources = qualify_srcs(ctx.label, ctx.attrs.base_module, resources_map)

    return python_executable(
        ctx,
        (EntryPointKind("module"), main_module),
        srcs,
        resources,
        standalone_resources,
        compile = value_or(ctx.attrs.compile, False),
        allow_cache_upload = False,
    )

def python_test_impl(ctx: AnalysisContext) -> list[Provider]:
    pex = python_test_executable(ctx)
    test_cmd = pex.run_cmd

    # Setup RE executors based on the `remote_execution` param.
    re_executor, executor_overrides = get_re_executors_from_props(ctx)
    test_env = ctx.attrs.env
    if pex.dbg_source_db:
        test_env["PYTHON_SOURCE_MAP"] = pex.dbg_source_db

    return inject_test_run_info(
        ctx,
        ExternalRunnerTestInfo(
            type = "pyunit",
            command = [test_cmd],
            env = test_env,
            labels = ctx.attrs.labels,
            contacts = ctx.attrs.contacts,
            default_executor = re_executor,
            executor_overrides = executor_overrides,
            # We implicitly make this test via the project root, instead of
            # the cell root (e.g. fbcode root).
            run_from_project_root = re_executor != None,
            use_project_relative_paths = re_executor != None,
        ),
    ) + [make_default_info(pex)]
