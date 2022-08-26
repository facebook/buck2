load(
    "@prelude//tests:re_utils.bzl",
    "get_re_executor_from_props",
)
load(
    ":needed_coverage.bzl",
    "gather_python_needed_coverage",
    "parse_python_needed_coverage_specs",
)
load(":python.bzl", "get_python_deps")
load(":python_test.bzl", "python_test_executable")

def python_needed_coverage_test_impl(ctx: "context") -> ["provider"]:
    output, runtime_files, extra = python_test_executable(ctx)

    test_env = {}
    test_env.update(ctx.attrs.env)

    test_cmd = cmd_args(output).hidden(runtime_files)

    # Pass in needed coverate flags to the test.
    needed_coverage = gather_python_needed_coverage(
        parse_python_needed_coverage_specs(ctx.attrs.needed_coverage),
        get_python_deps(ctx),
    )
    test_cmd.add("--collect-coverage")
    test_cmd.add("--coverage-include")
    test_cmd.add(",".join([
        "*/{}".format(module)
        for _, modules in needed_coverage
        for module in modules
    ]))
    for ratio, modules in needed_coverage:
        for module in modules:
            test_cmd.add("--coverage-verdict={}={}".format(module, ratio))

    # A needed coverage run just runs the entire test binary as bundle to
    # determine coverage.  Rather than special implementation in tpx, we
    # just use a simple test type to do this, which requires settings a few
    # additional flags/env-vars which the Python tpx test type would
    # otherwise handle.
    test_type = "simple"
    test_cmd.add("--output", "/dev/stdout")
    test_env["TEST_PILOT"] = "1"

    # Setup a RE executor based on the `remote_execution` param.
    re_executor = get_re_executor_from_props(ctx.attrs.remote_execution)

    return [
        DefaultInfo(
            default_outputs = [output],
            other_outputs = runtime_files,
            sub_targets = extra,
        ),
        RunInfo(test_cmd),
        ExternalRunnerTestInfo(
            type = test_type,
            command = [test_cmd],
            env = test_env,
            labels = ctx.attrs.labels,
            contacts = ctx.attrs.contacts,
            default_executor = re_executor,
            # We implicitly make this test via the project root, instead of
            # the cell root (e.g. fbcode root).
            run_from_project_root = re_executor != None,
            use_project_relative_paths = re_executor != None,
        ),
    ]
