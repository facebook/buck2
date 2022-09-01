load("@prelude//java:java_library.bzl", "build_java_library")
load("@prelude//java:java_providers.bzl", "get_all_java_packaging_deps_from_packaging_infos")
load("@prelude//java:java_toolchain.bzl", "JUnitToolchainInfo", "JavaToolchainInfo")
load("@prelude//java/utils:java_utils.bzl", "get_path_separator")
load("@prelude//linking:shared_libraries.bzl", "SharedLibraryInfo", "merge_shared_libraries", "traverse_shared_library_info")
load("@prelude//utils:utils.bzl", "filter_and_map_idx")

def java_test_impl(ctx: "context") -> ["provider"]:
    java_providers = build_java_library(ctx, ctx.attrs.srcs)
    external_runner_test_info, run_info = build_junit_test(ctx, java_providers.java_library_info, java_providers.java_packaging_info)

    return [
        java_providers.java_library_info,
        java_providers.java_packaging_info,
        java_providers.template_placeholder_info,
        java_providers.default_info,
        external_runner_test_info,
        run_info,
    ]

def build_junit_test(
        ctx: "context",
        tests_java_library_info: "JavaLibraryInfo",
        tests_java_packaging_info: "JavaPackagingInfo",
        extra_cmds: list.type = [],
        extra_classpath_entries: ["artifact"] = []) -> (ExternalRunnerTestInfo.type, RunInfo.type):
    junit_toolchain = ctx.attrs._junit_toolchain[JUnitToolchainInfo]

    cmd = [ctx.attrs._java_toolchain[JavaToolchainInfo].java_for_tests] + extra_cmds + ctx.attrs.vm_args
    classpath = []

    if junit_toolchain.use_java_custom_class_loader:
        cmd.append("-Djava.system.class.loader=" + junit_toolchain.java_custom_class_loader_class)
        cmd.extend(junit_toolchain.java_custom_class_loader_vm_args)
        classpath.append(junit_toolchain.java_custom_class_loader_library_jar)

    classpath.extend([junit_toolchain.junit_test_runner_library_jar] + [
        packaging_dep.jar
        for packaging_dep in get_all_java_packaging_deps_from_packaging_infos(ctx, [tests_java_packaging_info])
        if packaging_dep.jar
    ] + extra_classpath_entries)

    labels = ctx.attrs.labels or []
    run_from_cell_root = "buck2_run_from_cell_root" in labels
    uses_java8 = "run_with_java8" in labels

    classpath_args = cmd_args()
    if run_from_cell_root:
        classpath_args.relative_to(ctx.label.cell_root)

    if uses_java8:
        # Java 8 does not support using argfiles, and these tests can have huge classpaths so we need another
        # mechanism to write the classpath to a file.
        # We add "FileClassPathRunner" to the classpath, and then write a line-separated classpath file which we pass
        # to the "FileClassPathRunner" as a system variable. The "FileClassPathRunner" then loads all the jars
        # from that file onto the classpath, and delegates running the test to the junit test runner.
        cmd.extend(["-classpath", cmd_args(junit_toolchain.junit_test_runner_library_jar)])
        classpath_args.add(cmd_args(classpath))
        classpath_args_file = ctx.actions.write("classpath_args_file", classpath_args)
        cmd.append(cmd_args(classpath_args_file, format = "-Dbuck.classpath_file={}").hidden(classpath_args))
    else:
        # Java 9+ supports argfiles, so just write the classpath to an argsfile. "FileClassPathRunner" will delegate
        # immediately to the junit test runner.
        classpath_args.add("-classpath")
        classpath_args.add(cmd_args(classpath, delimiter = get_path_separator()))
        classpath_args_file = ctx.actions.write("classpath_args_file", classpath_args)
        cmd.append(cmd_args(classpath_args_file, format = "@{}").hidden(classpath_args))

    cmd.extend(junit_toolchain.junit_test_runner_main_class_args)
    if ctx.attrs.test_case_timeout_ms:
        cmd.extend(["--default_test_timeout", ctx.attrs.test_case_timeout_ms])

    class_names = ctx.actions.declare_output("class_names")
    list_class_names_cmd = cmd_args([
        junit_toolchain.list_class_names[RunInfo],
        "--jar",
        tests_java_library_info.library_output.full_library,
        "--output",
        class_names.as_output(),
    ])
    ctx.actions.run(list_class_names_cmd, category = "list_class_names")

    cmd.extend(["--test-class-names-file", class_names])

    native_libs_env = _get_native_libs_env(ctx)
    env = {}
    for d in [ctx.attrs.env, native_libs_env]:
        for key, value in d.items():
            if key in env:
                fail("Duplicate key for java_test env: '{}'".format(key))
            env[key] = value

    run_info = RunInfo(args = cmd_args(cmd))
    test_info = ExternalRunnerTestInfo(
        type = "junit",
        command = cmd,
        env = env,
        labels = ctx.attrs.labels,
        contacts = ctx.attrs.contacts,
        run_from_project_root = not run_from_cell_root,
        use_project_relative_paths = not run_from_cell_root,
    )
    return test_info, run_info

def _get_native_libs_env(ctx: "context") -> dict.type:
    if not ctx.attrs.use_cxx_libraries:
        return {}

    if ctx.attrs.cxx_library_whitelist:
        shared_library_infos = filter_and_map_idx(SharedLibraryInfo, ctx.attrs.cxx_library_whitelist)
    else:
        shared_library_infos = filter_and_map_idx(SharedLibraryInfo, ctx.attrs.deps)

    shared_library_info = merge_shared_libraries(
        ctx.actions,
        deps = shared_library_infos,
    )

    native_linkables = traverse_shared_library_info(shared_library_info)
    cxx_library_symlink_tree_dict = {so_name: shared_lib.lib.output for so_name, shared_lib in native_linkables.items()}
    cxx_library_symlink_tree = ctx.actions.symlinked_dir("cxx_library_symlink_tree", cxx_library_symlink_tree_dict)

    return {"BUCK_LD_SYMLINK_TREE": cxx_library_symlink_tree}
