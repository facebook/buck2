load("@fbcode//buck2/prelude/java:java_library.bzl", "build_java_library")
load("@fbcode//buck2/prelude/java:java_providers.bzl", "get_all_java_packaging_deps_from_packaging_infos")
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JUnitToolchainInfo", "JavaToolchainInfo")
load("@fbcode//buck2/prelude/java/utils:java_utils.bzl", "get_path_separator")
load("@fbcode//buck2/prelude/linking:shared_libraries.bzl", "SharedLibraryInfo", "merge_shared_libraries", "traverse_shared_library_info")
load("@fbcode//buck2/prelude/utils:utils.bzl", "filter_and_map_idx")

def java_test_impl(ctx: "context") -> ["provider"]:
    (
        tests_java_library_info,
        tests_java_packaging_info,
        _shared_library_info,
        _cxx_resources_info,
        template_placeholder_info,
        default_info,
    ) = build_java_library(ctx, ctx.attr.srcs)
    external_runner_test_info, run_info = build_junit_test(ctx, tests_java_library_info, tests_java_packaging_info)

    return [
        tests_java_library_info,
        template_placeholder_info,
        default_info,
        external_runner_test_info,
        run_info,
    ]

def build_junit_test(
        ctx: "context",
        tests_java_library_info: "JavaLibraryInfo",
        tests_java_packaging_info: "JavaPackagingInfo",
        extra_cmd_args: ["cmd_args", None] = None,
        extra_classpath_entries: ["artifact"] = []) -> (ExternalRunnerTestInfo.type, RunInfo.type):
    junit_toolchain = ctx.attr._junit_toolchain[JUnitToolchainInfo]

    cmd = cmd_args(ctx.attr._java_toolchain[JavaToolchainInfo].java_for_tests)
    if extra_cmd_args:
        cmd.add(extra_cmd_args)
    cmd.add(ctx.attr.vm_args)

    classpath = [junit_toolchain.junit_test_runner_library_jar] + [
        packaging_dep.jar
        for packaging_dep in get_all_java_packaging_deps_from_packaging_infos(ctx, [tests_java_packaging_info])
    ] + extra_classpath_entries

    classpath_args = cmd_args()
    classpath_args.relative_to(ctx.label.test_cwd)
    classpath_args.add("-classpath")
    classpath_args.add(cmd_args(classpath, delimiter = get_path_separator()))
    classpath_args_file = ctx.actions.write("classpath_args_file", classpath_args)
    cmd.add(cmd_args(classpath_args_file, format = "@{}"))
    cmd.hidden(classpath_args)

    cmd.add(junit_toolchain.junit_test_runner_main_class)
    if ctx.attr.test_case_timeout_ms:
        cmd.add(["--default_test_timeout", ctx.attr.test_case_timeout_ms])

    class_names = ctx.actions.declare_output("class_names")
    list_class_names_cmd = cmd_args([
        junit_toolchain.list_class_names[RunInfo],
        "--jar",
        tests_java_library_info.library_output.full_library,
        "--output",
        class_names.as_output(),
    ])
    ctx.actions.run(list_class_names_cmd, category = "list_class_names")

    cmd.add("--test-class-names-file", class_names)

    native_libs_env = _get_native_libs_env(ctx)
    env = {}
    for d in [ctx.attr.env, native_libs_env]:
        for key, value in d.items():
            if key in env:
                fail("Duplicate key for java_test env: '{}'".format(key))
            env[key] = value

    run_info = RunInfo(args = cmd)
    test_info = ExternalRunnerTestInfo(
        type = "junit",
        command = [cmd],
        env = env,
        labels = ctx.attr.labels,
        contacts = ctx.attr.contacts,
        use_templated_api = False,
    )
    return test_info, run_info

def _get_native_libs_env(ctx: "context") -> dict.type:
    if not ctx.attr.use_cxx_libraries:
        return {}

    if ctx.attr.cxx_library_whitelist:
        shared_library_infos = filter_and_map_idx(SharedLibraryInfo, ctx.attr.cxx_library_whitelist)
    else:
        shared_library_infos = filter_and_map_idx(SharedLibraryInfo, ctx.attr.deps)

    shared_library_info = merge_shared_libraries(
        ctx.actions,
        deps = shared_library_infos,
    )

    native_linkables = traverse_shared_library_info(shared_library_info)
    cxx_library_symlink_tree_dict = {so_name: shared_lib.lib.output for so_name, shared_lib in native_linkables.items()}
    cxx_library_symlink_tree = ctx.actions.symlinked_dir("cxx_library_symlink_tree", cxx_library_symlink_tree_dict)

    return {"BUCK_LD_SYMLINK_TREE": cxx_library_symlink_tree}
