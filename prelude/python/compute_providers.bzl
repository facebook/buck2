# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//test:inject_test_run_info.bzl", "inject_test_run_info")
load(
    "@prelude//tests:re_utils.bzl",
    "get_re_executors_from_props",
)
load(":make_py_package.bzl", "PexProviders", "make_default_info", "make_run_info")

ExecutableType = enum(
    "binary",
    "test",
)

def compute_providers(ctx: AnalysisContext, exe: PexProviders, executable_type: ExecutableType) -> list[Provider]:
    if executable_type == ExecutableType("binary"):
        return compute_binary_providers(ctx, exe)
    elif executable_type == ExecutableType("test"):
        return compute_test_providers(ctx, exe)
    else:
        fail("Unknown executable type: " + executable_type)

def compute_binary_providers(ctx: AnalysisContext, exe: PexProviders) -> list[Provider]:
    return [
        make_default_info(exe),
        make_run_info(exe, ctx.attrs.run_with_inplace),
    ]

def compute_test_providers(ctx: AnalysisContext, exe: PexProviders) -> list[Provider]:
    test_cmd = exe.run_cmd

    # Setup RE executors based on the `remote_execution` param.
    re_executor, executor_overrides = get_re_executors_from_props(ctx)
    test_env = ctx.attrs.env
    if exe.dbg_source_db:
        test_env["PYTHON_SOURCE_MAP"] = exe.dbg_source_db

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
    ) + [make_default_info(exe)]
