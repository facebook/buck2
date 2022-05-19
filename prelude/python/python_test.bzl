load("@fbcode//buck2/prelude:paths.bzl", "paths")
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

def python_test_impl(ctx: "context") -> ["provider"]:
    main_module = value_or(ctx.attr.main_module, "__test_main__")

    srcs = qualify_srcs(ctx.label, ctx.attr.base_module, from_named_set(ctx.attr.srcs))

    # Generate the test modules file and add it to sources.
    test_modules_name, test_modules_path = _write_test_modules_list(ctx, srcs)
    srcs[test_modules_name] = test_modules_path

    # Add in default test runner.
    srcs["__test_main__.py"] = ctx.attr._test_main

    resources = qualify_srcs(ctx.label, ctx.attr.base_module, py_attr_resources(ctx))

    output, runtime_files, source_db = python_executable(
        ctx,
        main_module,
        srcs,
        resources,
        compile = value_or(ctx.attr.compile, False),
    )

    return [
        DefaultInfo(
            default_outputs = [output],
            other_outputs = runtime_files,
            sub_targets = {"source-db": [source_db]},
        ),
        RunInfo(cmd_args(output).hidden(runtime_files)),
        ExternalRunnerTestInfo(
            type = "pyunit",
            command = [cmd_args(output).hidden(runtime_files)],
            env = ctx.attr.env,
            labels = ctx.attr.labels,
            contacts = ctx.attr.contacts,
            use_templated_api = False,
        ),
    ]
