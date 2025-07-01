# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load("@prelude//python:compute_providers.bzl", "ExecutableType")
load("@prelude//utils:utils.bzl", "from_named_set", "value_or")
load(":interface.bzl", "EntryPointKind")
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

def python_test_executable(ctx: AnalysisContext) -> list[Provider] | Promise:
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
        executable_type = ExecutableType("test"),
    )

def python_test_impl(ctx: AnalysisContext) -> list[Provider] | Promise:
    return python_test_executable(ctx)
