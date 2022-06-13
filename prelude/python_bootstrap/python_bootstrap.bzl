PythonBootstrapSources = provider(fields = ["srcs"])

PythonBootstrapToolchainInfo = provider(fields = ["interpreter"])

def python_bootstrap_library_impl(ctx: "context") -> ["provider"]:
    src_dir = ctx.actions.declare_output("__%s__", ctx.attr.name)
    tree = {}
    for src in ctx.attr.srcs:
        tree[src.short_path] = src
    output = ctx.actions.symlinked_dir(src_dir, tree)
    return [
        DefaultInfo(default_outputs = [output]),
        PythonBootstrapSources(srcs = ctx.attr.srcs),
    ]

def python_bootstrap_binary_impl(ctx: "context") -> ["provider"]:
    """
    Declares a Python binary that is intended to be used in scripts that
    bootstrap other aspects of the Buck2 prelude. Python bootstrap binaries do
    not use the Python toolchain and, as such, are highly restricted in what
    they can and can't do. In particular, bootstrap binaries can only depend on
    bootstrap libraries and can only consist of a single file.
    """
    run_tree_inputs = {}
    run_tree_recorded_deps = {}  # For a better error message when files collide
    for dep in ctx.attr.deps:
        dep_srcs = dep[PythonBootstrapSources].srcs
        for src in dep_srcs:
            if src.short_path in run_tree_recorded_deps:
                original_dep = run_tree_recorded_deps[src.short_path]
                fail("dependency `{}` and `{}` both declare a source file named `{}`, consider renaming one of these files to avoid collision".format(original_dep.label, dep.label, src.short_path))
            run_tree_inputs[src.short_path] = src
            run_tree_recorded_deps[src.short_path] = dep

    run_tree = ctx.actions.symlinked_dir("__%s__" % ctx.attr.name, run_tree_inputs)
    output = ctx.actions.copy_file(ctx.attr.main.short_path, ctx.attr.main)
    interpreter = ctx.attr._python_bootstrap_toolchain[PythonBootstrapToolchainInfo].interpreter
    run_args = cmd_args([
        "/usr/bin/env",
    ])
    run_args.add(cmd_args(run_tree, format = "PYTHONPATH={}"))
    run_args.add(interpreter)
    run_args.add(output)
    return [DefaultInfo(default_outputs = [output]), RunInfo(args = run_args)]
