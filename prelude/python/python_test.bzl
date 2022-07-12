load("@fbcode//buck2/prelude:paths.bzl", "paths")
load(
    "@fbcode//buck2/prelude/tests:tpx_re_legacy.bzl",
    "get_re_executor_from_labels",
)
load("@fbcode//buck2/prelude/utils:utils.bzl", "from_named_set", "value_or")
load(":python_binary.bzl", "python_executable")
load(":python_library.bzl", "py_attr_resources", "qualify_srcs")

def _write_test_modules_list(
        ctx: "context",
        srcs: {str.type: "artifact"}) -> (str.type, "artifact"):
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

def python_test_executable(ctx: "context") -> ("artifact", ["_arglike"], DefaultInfo.type):
    main_module = value_or(ctx.attrs.main_module, "__test_main__")

    srcs = qualify_srcs(ctx.label, ctx.attrs.base_module, from_named_set(ctx.attrs.srcs))

    # Generate the test modules file and add it to sources.
    test_modules_name, test_modules_path = _write_test_modules_list(ctx, srcs)
    srcs[test_modules_name] = test_modules_path

    # Add in default test runner.
    srcs["__test_main__.py"] = ctx.attrs._test_main

    resources = qualify_srcs(ctx.label, ctx.attrs.base_module, py_attr_resources(ctx))

    return python_executable(
        ctx,
        main_module,
        srcs,
        resources,
        compile = value_or(ctx.attrs.compile, False),
    )

def python_test_impl(ctx: "context") -> ["provider"]:
    output, runtime_files, source_db = python_test_executable(ctx)
    test_cmd = cmd_args(output).hidden(runtime_files)

    # Support tpx's v1 behavior, where tests can configure themselves to use
    # RE with a specific platform via labels.
    legacy_re_executor = get_re_executor_from_labels(ctx.attrs.labels)

    return [
        DefaultInfo(
            default_outputs = [output],
            other_outputs = runtime_files,
            sub_targets = {"source-db": [source_db]},
        ),
        RunInfo(test_cmd),
        ExternalRunnerTestInfo(
            type = "pyunit",
            command = [test_cmd],
            env = ctx.attrs.env,
            labels = ctx.attrs.labels,
            contacts = ctx.attrs.contacts,
            default_executor = legacy_re_executor,
            # We implicitly make this test via the project root, instead of
            # the cell root (e.g. fbcode root).
            run_from_project_root = legacy_re_executor != None,
            use_project_relative_paths = legacy_re_executor != None,
        ),
    ]
