# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//utils:utils.bzl", "flatten")

PythonBootstrapSources = provider(fields = {"srcs": provider_field(typing.Any, default = None)})

PythonBootstrapToolchainInfo = provider(fields = {"interpreter": provider_field(typing.Any, default = None)})

def python_bootstrap_library_impl(ctx: AnalysisContext) -> list[Provider]:
    tree = {src.short_path: src for src in ctx.attrs.srcs}
    output = ctx.actions.symlinked_dir("__{}__".format(ctx.attrs.name), tree)
    return [
        DefaultInfo(default_output = output),
        PythonBootstrapSources(srcs = dedupe(flatten([ctx.attrs.srcs] + [dep[PythonBootstrapSources].srcs for dep in ctx.attrs.deps]))),
    ]

def python_bootstrap_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    """
    Declares a Python binary that is intended to be used in scripts that
    bootstrap other aspects of the Buck2 prelude. Python bootstrap binaries do
    not use the Python toolchain and, as such, are highly restricted in what
    they can and can't do. In particular, bootstrap binaries can only depend on
    bootstrap libraries and can only consist of a single file.
    """
    copy_deps = ctx.attrs.copy_deps
    run_tree_inputs = {}
    run_tree_recorded_deps = {}  # For a better error message when files collide
    for dep in ctx.attrs.deps:
        dep_srcs = dep[PythonBootstrapSources].srcs
        for src in dep_srcs:
            if src.short_path in run_tree_recorded_deps and src != run_tree_inputs[src.short_path]:
                original_dep = run_tree_recorded_deps[src.short_path]
                fail("dependency `{}` and `{}` both declare a source file named `{}`, consider renaming one of these files to avoid collision".format(original_dep.label, dep.label, src.short_path))
            run_tree_inputs[src.short_path] = src
            run_tree_recorded_deps[src.short_path] = dep

    main = ctx.attrs.main
    if main.short_path in run_tree_inputs:
        original_dep = run_tree_recorded_deps[main.short_path].label
        fail("dependency `{}` declares a source file with the same name as main file; consider renaming it".format(original_dep))

    run_tree_inputs[main.short_path] = main
    if copy_deps:
        run_tree = ctx.actions.copied_dir("__%s__" % ctx.attrs.name, run_tree_inputs)
    else:
        run_tree = ctx.actions.symlinked_dir("__%s__" % ctx.attrs.name, run_tree_inputs)

    interpreter = ctx.attrs._python_bootstrap_toolchain[PythonBootstrapToolchainInfo].interpreter
    main_artifact = run_tree.project(main.short_path)
    run_args = cmd_args(
        [interpreter, run_tree.project(main.short_path)],
        hidden = run_tree,
    )
    return [
        DefaultInfo(default_output = main_artifact, other_outputs = [run_tree]),
        RunInfo(args = run_args),
    ]
