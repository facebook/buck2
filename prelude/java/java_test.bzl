# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//java:class_to_srcs.bzl",
    "JavaClassToSourceMapInfo",  # @unused Used as a type
    "merge_class_to_source_map_from_jar",
)
load("@prelude//java:java_library.bzl", "build_java_library")
load("@prelude//java:java_providers.bzl", "JavaLibraryInfo", "JavaPackagingInfo", "get_all_java_packaging_deps_tset")
load("@prelude//java:java_toolchain.bzl", "JavaTestToolchainInfo", "JavaToolchainInfo")
load("@prelude//java/utils:java_more_utils.bzl", "get_path_separator_for_exec_os")
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
    "create_shlib_symlink_tree",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)
load("@prelude//test:inject_test_run_info.bzl", "inject_test_run_info")
load(
    "@prelude//tests:re_utils.bzl",
    "get_re_executors_from_props",
)
load("@prelude//utils:argfile.bzl", "at_argfile")
load("@prelude//utils:expect.bzl", "expect")

def java_test_impl(ctx: AnalysisContext) -> list[Provider]:
    if ctx.attrs._build_only_native_code:
        return [DefaultInfo()]

    java_providers = build_java_library(ctx, ctx.attrs.srcs)
    external_runner_test_info = build_junit_test(ctx, java_providers.java_library_info, java_providers.java_packaging_info, java_providers.class_to_src_map)

    return inject_test_run_info(ctx, external_runner_test_info) + [
        java_providers.java_library_intellij_info,
        java_providers.java_library_info,
        java_providers.java_packaging_info,
        java_providers.template_placeholder_info,
        java_providers.default_info,
        java_providers.class_to_src_map,
    ]

def build_junit_test(
        ctx: AnalysisContext,
        tests_java_library_info: JavaLibraryInfo,
        tests_java_packaging_info: JavaPackagingInfo,
        tests_class_to_source_info: [JavaClassToSourceMapInfo, None] = None,
        extra_cmds: list = [],
        extra_classpath_entries: list[Artifact] = []) -> ExternalRunnerTestInfo:
    java_test_toolchain = ctx.attrs._java_test_toolchain[JavaTestToolchainInfo]
    java_toolchain = ctx.attrs._java_toolchain[JavaToolchainInfo]

    java = ctx.attrs.java[RunInfo] if ctx.attrs.java else java_toolchain.java_for_tests

    cmd = [java] + extra_cmds + ctx.attrs.vm_args + ["-XX:-MaxFDLimit"]
    if java_test_toolchain.jvm_args:
        cmd.extend(java_test_toolchain.jvm_args)

    cmd.append(cmd_args(ctx.attrs.java_agents, format = "-javaagent:{}"))

    classpath = [
        java_test_toolchain.test_runner_library_jar,
    ] + [
        get_all_java_packaging_deps_tset(ctx, java_packaging_infos = [tests_java_packaging_info])
            .project_as_args("full_jar_args", ordering = "bfs"),
    ] + extra_classpath_entries

    if ctx.attrs.unbundled_resources_root:
        classpath.append(ctx.attrs.unbundled_resources_root)

    labels = ctx.attrs.labels or []

    # Setup RE executors based on the `remote_execution` param.
    re_executor, executor_overrides = get_re_executors_from_props(ctx)

    # We implicitly make the target run from the project root if remote
    # execution options were specified.
    run_from_cell_root = "buck2_run_from_cell_root" in labels

    uses_java8 = "run_with_java8" in labels

    relative_to = {"relative_to": ctx.label.cell_root} if run_from_cell_root else {}

    if uses_java8:
        # Java 8 does not support using argfiles, and these tests can have huge classpaths so we need another
        # mechanism to write the classpath to a file.
        # We add "FileClassPathRunner" to the classpath, and then write a line-separated classpath file which we pass
        # to the "FileClassPathRunner" as a system variable. The "FileClassPathRunner" then loads all the jars
        # from that file onto the classpath, and delegates running the test to the junit test runner.
        cmd.extend(["-classpath", cmd_args(java_test_toolchain.test_runner_library_jar)])
        classpath_args = cmd_args(
            cmd_args(classpath),
            **relative_to
        )
        classpath_args_file = ctx.actions.write("classpath_args_file", classpath_args)
        cmd.append(cmd_args(
            classpath_args_file,
            format = "-Dbuck.classpath_file={}",
            hidden = classpath_args,
        ))
    else:
        # Java 9+ supports argfiles, so just write the classpath to an argsfile. "FileClassPathRunner" will delegate
        # immediately to the junit test runner.
        classpath_args = cmd_args(
            "-classpath",
            cmd_args(classpath, delimiter = get_path_separator_for_exec_os(ctx)),
            **relative_to
        )
        cmd.append(at_argfile(actions = ctx.actions, name = "classpath_args_file", args = classpath_args))

    if (ctx.attrs.test_type == "junit5"):
        cmd.extend(java_test_toolchain.junit5_test_runner_main_class_args)
    elif (ctx.attrs.test_type == "testng"):
        cmd.extend(java_test_toolchain.testng_test_runner_main_class_args)
    else:
        cmd.extend(java_test_toolchain.junit_test_runner_main_class_args)

    if ctx.attrs.test_case_timeout_ms:
        cmd.extend(["--default-test-timeout", str(ctx.attrs.test_case_timeout_ms)])

    if ctx.attrs.test_class_names_file:
        class_names = ctx.attrs.test_class_names_file
    else:
        expect(tests_java_library_info.library_output != None, "Built test library has no output, likely due to missing srcs")
        class_names = ctx.actions.declare_output("class_names")
        list_class_names_cmd = cmd_args([
            java_test_toolchain.list_class_names[RunInfo],
            "--jar",
            tests_java_library_info.library_output.full_library,
            "--sources",
            ctx.actions.write("sources.txt", ctx.attrs.srcs),
            "--output",
            class_names.as_output(),
        ], hidden = ctx.attrs.srcs)
        ctx.actions.run(list_class_names_cmd, category = "list_class_names")

    cmd.extend(["--test-class-names-file", class_names])

    native_libs_env = _get_native_libs_env(ctx)
    env = {}
    for d in [ctx.attrs.env, native_libs_env]:
        for key, value in d.items():
            if key in env:
                fail("Duplicate key for java_test env: '{}'".format(key))
            env[key] = value

    if tests_class_to_source_info != None:
        transitive_class_to_src_map = merge_class_to_source_map_from_jar(
            actions = ctx.actions,
            name = ctx.label.name + ".transitive_class_to_src.json",
            java_toolchain = java_toolchain,
            relative_to = ctx.label.cell_root if run_from_cell_root else None,
            deps = [tests_class_to_source_info],
        )
        if run_from_cell_root:
            transitive_class_to_src_map = cmd_args(transitive_class_to_src_map, relative_to = ctx.label.cell_root)
        env["JACOCO_CLASSNAME_SOURCE_MAP"] = transitive_class_to_src_map

    list_tests = java_test_toolchain.list_tests
    if list_tests != None and "tpx:supports_static_listing=true" in ctx.attrs.labels and "tpx:supports_static_listing=false" not in ctx.attrs.labels:
        list_tests_command = cmd_args([
            list_tests[RunInfo],
            "list-tests",
            "--sources-file",
            ctx.actions.write("source_files.txt", ctx.attrs.srcs, with_inputs = True),
        ])
        env["TPX_LIST_TESTS_COMMAND"] = list_tests_command

    test_info = ExternalRunnerTestInfo(
        type = "junit",
        command = cmd,
        env = env,
        labels = ctx.attrs.labels,
        contacts = ctx.attrs.contacts,
        run_from_project_root = not run_from_cell_root,
        use_project_relative_paths = not run_from_cell_root,
        default_executor = re_executor,
        executor_overrides = executor_overrides,
    )
    return test_info

def _get_native_libs_env(ctx: AnalysisContext) -> dict:
    if not ctx.attrs.use_cxx_libraries:
        return {}

    deps_to_search = ctx.attrs.cxx_library_allowlist or ctx.attrs.deps
    shared_library_infos = filter(None, [x.get(SharedLibraryInfo) for x in deps_to_search])

    shared_library_info = merge_shared_libraries(
        ctx.actions,
        deps = shared_library_infos,
    )

    cxx_library_symlink_tree = create_shlib_symlink_tree(
        actions = ctx.actions,
        out = "cxx_library_symlink_tree",
        shared_libs = traverse_shared_library_info(shared_library_info),
    )

    return {"BUCK_LD_SYMLINK_TREE": cxx_library_symlink_tree}
