load(
    ":needed_coverage.bzl",
    "gather_python_needed_coverage",
    "parse_python_needed_coverage_specs",
)
load(":python.bzl", "get_python_deps")
load(":python_test.bzl", "python_test_executable")

def python_needed_coverage_test_impl(ctx: "context") -> ["provider"]:
    output, runtime_files, source_db = python_test_executable(ctx)

    test_env = {}
    test_env.update(ctx.attr.env)

    test_cmd = cmd_args(output).hidden(runtime_files)

    # Pass in needed coverate flags to the test.
    needed_coverage = gather_python_needed_coverage(
        parse_python_needed_coverage_specs(ctx.attr.needed_coverage),
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

    return [
        DefaultInfo(
            default_outputs = [output],
            other_outputs = runtime_files,
            sub_targets = {"source-db": [source_db]},
        ),
        RunInfo(test_cmd),
        ExternalRunnerTestInfo(
            type = test_type,
            command = [test_cmd],
            env = test_env,
            labels = ctx.attr.labels,
            contacts = ctx.attr.contacts,
        ),
    ]
