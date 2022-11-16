load("@prelude//os_lookup:defs.bzl", "OsLookup")

PythonBootstrapSources = provider(fields = ["srcs"])

PythonBootstrapToolchainInfo = provider(fields = ["interpreter", "windows_interpreter"])

def python_bootstrap_library_impl(ctx: "context") -> ["provider"]:
    src_dir = ctx.actions.declare_output("__%s__", ctx.attrs.name)
    tree = {}
    for src in ctx.attrs.srcs:
        tree[src.short_path] = src
    output = ctx.actions.symlinked_dir(src_dir, tree)
    return [
        DefaultInfo(default_outputs = [output]),
        PythonBootstrapSources(srcs = ctx.attrs.srcs),
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
    for dep in ctx.attrs.deps:
        dep_srcs = dep[PythonBootstrapSources].srcs
        for src in dep_srcs:
            if src.short_path in run_tree_recorded_deps:
                original_dep = run_tree_recorded_deps[src.short_path]
                fail("dependency `{}` and `{}` both declare a source file named `{}`, consider renaming one of these files to avoid collision".format(original_dep.label, dep.label, src.short_path))
            run_tree_inputs[src.short_path] = src
            run_tree_recorded_deps[src.short_path] = dep

    run_tree = ctx.actions.symlinked_dir("__%s__" % ctx.attrs.name, run_tree_inputs)
    output = ctx.actions.copy_file(ctx.attrs.main.short_path, ctx.attrs.main)

    is_windows = ctx.attrs._exec_os_type[OsLookup].platform == "windows"

    run_args = cmd_args()
    if is_windows:
        windows_interpreter = ctx.attrs._python_bootstrap_toolchain[PythonBootstrapToolchainInfo].windows_interpreter
        run_args.add(ctx.attrs._win_python_wrapper[RunInfo])
        run_args.add(run_tree)
        run_args.add(windows_interpreter)
        run_args.add(output)
    else:
        interpreter = ctx.attrs._python_bootstrap_toolchain[PythonBootstrapToolchainInfo].interpreter
        run_args.add("/usr/bin/env")
        run_args.add(cmd_args(run_tree, format = "PYTHONPATH={}"))
        run_args.add(interpreter)
        run_args.add(output)
    return [DefaultInfo(default_outputs = [output]), RunInfo(args = run_args)]
