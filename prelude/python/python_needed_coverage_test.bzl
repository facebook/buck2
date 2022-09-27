load(
    "@prelude//tests:re_utils.bzl",
    "get_re_executor_from_props",
)
load(
    ":needed_coverage.bzl",
    "parse_python_needed_coverage_specs",
)

def python_needed_coverage_test_impl(ctx: "context") -> ["provider"]:
    test_cmd = cmd_args(ctx.attrs.test[RunInfo])

    test_env = {}
    test_env.update(ctx.attrs.env)

    # Pass in needed coverate flags to the test.
    needed_coverages = parse_python_needed_coverage_specs(ctx.attrs.needed_coverage)
    test_cmd.add("--collect-coverage")
    test_cmd.add("--coverage-include")
    test_cmd.add(",".join([
        "*/{}".format(module)
        for needed_coverage in needed_coverages
        for module in needed_coverage.modules
    ]))
    for needed_coverage in needed_coverages:
        for module in needed_coverage.modules:
            test_cmd.add("--coverage-verdict={}={}".format(module, needed_coverage.ratio))

    # A needed coverage run just runs the entire test binary as bundle to
    # determine coverage.  Rather than special implementation in tpx, we
    # just use a simple test type to do this, which requires settings a few
    # additional flags/env-vars which the Python tpx test type would
    # otherwise handle.
    test_type = "simple"
    test_env["TEST_PILOT"] = "1"

    # Setup a RE executor based on the `remote_execution` param.
    re_executor = get_re_executor_from_props(ctx.attrs.remote_execution)

    return [
        DefaultInfo(),
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
