# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//java:java_toolchain.bzl", "JavaToolchainInfo")
load("@prelude//java/utils:java_utils.bzl", "get_class_to_source_map_info", "get_classpath_subtarget")
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibrary",  # @unused used as type
    "SharedLibraryInfo",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)
load("@prelude//utils:expect.bzl", "expect")
load(
    ":java_providers.bzl",
    "create_template_info",
    "derive_compiling_deps",
    "get_java_packaging_info",
)

def _should_use_incremental_build(ctx: AnalysisContext):
    # use incremental build only for __unstamped jars (which includes inner.jar)
    return ctx.label.name.startswith("__unstamped") and (
        "incremental_build" in ctx.attrs.labels or read_config("java", "inc_build", "false").lower() == "true"
    )

def _is_nested_package(ctx: AnalysisContext, pkg: str) -> bool:
    return pkg == ctx.label.package or pkg.startswith(ctx.label.package + "/")

def _get_dependencies_jars(ctx: AnalysisContext, package_deps: typing.Any) -> cmd_args:
    jars = cmd_args()
    for dep in package_deps.transitive_set.traverse():
        if dep.jar and not _is_nested_package(ctx, dep.label.package):
            jars.add(dep.jar)
    return jars

def _get_incremental_jars(ctx: AnalysisContext, package_deps: typing.Any) -> cmd_args:
    jars = cmd_args()
    for dep in package_deps.transitive_set.traverse():
        if dep.jar and _is_nested_package(ctx, dep.label.package):
            jars.add(dep.jar)
    return jars

def _generate_script(generate_wrapper: bool, native_libs: list[SharedLibrary]) -> bool:
    # if `generate_wrapper` is set and no native libs then it should be a wrapper script as result,
    # otherwise fat jar will be generated (inner jar or script will be included inside a final fat jar)
    return generate_wrapper and len(native_libs) == 0

def _create_fat_jar(
        ctx: AnalysisContext,
        jars: cmd_args,
        native_libs: list[SharedLibrary],
        name_prefix: str = "",
        do_not_create_inner_jar: bool = True,
        generate_wrapper: bool = False,
        main_class: [str, None] = None,
        append_jar: [Artifact, None] = None) -> list[Artifact]:
    java_toolchain = ctx.attrs._java_toolchain[JavaToolchainInfo]
    extension = "sh" if _generate_script(generate_wrapper, native_libs) else "jar"
    output = ctx.actions.declare_output("{}{}.{}".format(name_prefix, ctx.label.name, extension))

    args = [
        java_toolchain.fat_jar[RunInfo],
        "--jar_builder_tool",
        cmd_args(java_toolchain.jar_builder, delimiter = " "),
        "--zip_scrubber_tool",
        cmd_args(java_toolchain.zip_scrubber, delimiter = " "),
        "--output",
        output.as_output(),
        "--jars_file",
        ctx.actions.write("{}jars_file".format(name_prefix), jars),
    ]

    if append_jar:
        args += ["--append_jar", append_jar]

    if native_libs:
        expect(
            java_toolchain.is_bootstrap_toolchain == False,
            "Bootstrap java toolchain could not be used for java_binary() with native code.",
        )
        args += [
            "--native_libs_file",
            ctx.actions.write("{}native_libs".format(name_prefix), [cmd_args([native_lib.soname.ensure_str(), native_lib.lib.output], delimiter = " ") for native_lib in native_libs]),
        ]
        if do_not_create_inner_jar:
            args += [
                "--do_not_create_inner_jar",
            ]
        else:
            args += [
                "--fat_jar_lib",
                java_toolchain.fat_jar_main_class_lib,
                # fat jar's main class
                "--fat_jar_main_class",
                "com.facebook.buck.jvm.java.FatJarMain",
                # native libraries directory name. Main class expects to find libraries packed inside this directory.
                "--fat_jar_native_libs_directory_name",
                "nativelibs",
            ]

    if main_class:
        args += ["--main_class", main_class]

    manifest_file = ctx.attrs.manifest_file
    if manifest_file:
        args += ["--manifest", manifest_file]

    blocklist = ctx.attrs.blacklist
    if blocklist:
        args += ["--blocklist", ctx.actions.write("{}blocklist_args".format(name_prefix), blocklist)]

    if ctx.attrs.meta_inf_directory:
        args += ["--meta_inf_directory", ctx.attrs.meta_inf_directory]

    outputs = [output]
    if generate_wrapper:
        classpath_args_output = ctx.actions.declare_output("classpath_args")
        args += [
            "--generate_wrapper",
            "--classpath_args_output",
            classpath_args_output.as_output(),
            "--java_tool",
            java_toolchain.java[RunInfo],
            "--script_marker_file_name",
            "wrapper_script",
        ]
        outputs.append(classpath_args_output)

    fat_jar_cmd = cmd_args(
        args,
        hidden = [jars] + [native_lib.lib.output for native_lib in native_libs],
    )

    ctx.actions.run(
        fat_jar_cmd,
        local_only = False,
        category = "{}fat_jar".format(name_prefix),
        allow_cache_upload = True,
    )

    if generate_wrapper == False:
        expect(
            len(outputs) == 1,
            "expected exactly one output when creating a fat jar",
        )

    # If `generate_wrapper` is not set then the result will contain only 1 item that represent fat jar artifact.
    # Else if `generate_wrapper` is set then the first item in the result list will be script or far jar, and the second one is for @classpath_args file
    return outputs

def _get_run_cmd(
        attrs: struct,
        script_mode: bool,
        main_artifact: Artifact,
        java_toolchain: JavaToolchainInfo) -> cmd_args:
    if script_mode:
        return cmd_args(["/usr/bin/env", "bash", main_artifact])
    else:
        return cmd_args([java_toolchain.java[RunInfo]] + attrs.java_args_for_run_info + ["-jar", main_artifact])

def _get_java_tool_artifacts(java_toolchain: JavaToolchainInfo) -> list[Artifact]:
    default_info = java_toolchain.java[DefaultInfo]
    return default_info.default_outputs + default_info.other_outputs

def java_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    """
     java_binary() rule implementation

    Args:
        ctx: rule analysis context
    Returns:
        list of created providers (DefaultInfo and RunInfo)
    """

    if ctx.attrs._build_only_native_code:
        return [
            DefaultInfo(default_output = ctx.actions.write("{}/unused.jar".format(ctx.label.name), [])),
            RunInfo(),
        ]

    packaging_info = get_java_packaging_info(ctx, ctx.attrs.deps, None)

    first_order_deps = derive_compiling_deps(ctx.actions, None, ctx.attrs.deps)
    first_order_libs = [dep.full_library for dep in (list(first_order_deps.traverse()) if first_order_deps else [])]

    shared_library_info = merge_shared_libraries(
        ctx.actions,
        deps = filter(None, [x.get(SharedLibraryInfo) for x in ctx.attrs.deps]),
    )
    native_deps = traverse_shared_library_info(shared_library_info)

    java_toolchain = ctx.attrs._java_toolchain[JavaToolchainInfo]
    need_to_generate_wrapper = ctx.attrs.generate_wrapper == True
    do_not_create_inner_jar = ctx.attrs.do_not_create_inner_jar == True
    packaging_jar_args = packaging_info.packaging_deps.project_as_args("full_jar_args")
    main_class = ctx.attrs.main_class

    other_outputs = []

    if _should_use_incremental_build(ctx):
        # collect all dependencies
        dependencies_jars = _get_dependencies_jars(ctx, packaging_jar_args)

        # collect nested targets
        incremental_jars = _get_incremental_jars(ctx, packaging_jar_args)

        # generate intermediary jar only with dependencies
        deps_outputs = _create_fat_jar(
            ctx,
            dependencies_jars,
            native_deps,
            name_prefix = "deps_",
        )
        other_outputs = [deps_outputs[0]]

        # generate final jar appending modules to the dependencies jar
        outputs = _create_fat_jar(
            ctx,
            incremental_jars,
            native_deps,
            do_not_create_inner_jar = do_not_create_inner_jar,
            generate_wrapper = need_to_generate_wrapper,
            main_class = main_class,
            append_jar = deps_outputs[0],
        )
    else:
        outputs = _create_fat_jar(
            ctx,
            cmd_args(packaging_jar_args),
            native_deps,
            do_not_create_inner_jar = do_not_create_inner_jar,
            generate_wrapper = need_to_generate_wrapper,
            main_class = main_class,
        )

    run_cmd = _get_run_cmd(
        attrs = ctx.attrs,
        script_mode = _generate_script(need_to_generate_wrapper, native_deps),
        main_artifact = outputs[0],
        java_toolchain = java_toolchain,
    )

    if need_to_generate_wrapper:
        classpath_file = outputs[1]
        run_cmd.add(cmd_args(hidden = [
            java_toolchain.java[RunInfo],
            classpath_file,
            packaging_jar_args,
        ]))
        other_outputs = [classpath_file] + [packaging_jar_args] + _get_java_tool_artifacts(java_toolchain)

    sub_targets = get_classpath_subtarget(ctx.actions, packaging_info)

    class_to_src_map, _, _ = get_class_to_source_map_info(
        ctx,
        outputs = None,
        deps = ctx.attrs.deps,
    )

    return [
        DefaultInfo(default_output = outputs[0], other_outputs = other_outputs, sub_targets = sub_targets),
        RunInfo(args = run_cmd),
        create_template_info(ctx, packaging_info, first_order_libs),
        class_to_src_map,
    ]
