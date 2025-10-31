# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//java:java_providers.bzl",
    "ClasspathSnapshotGranularity",
    "JavaClasspathEntry",  # @unused Used as a type
    "JavaCompileOutputs",  # @unused Used as a type
    "JavaCompilingDepsTSet",  # @unused Used as a type
    "generate_java_classpath_snapshot",
    "make_compile_outputs",
)
load("@prelude//java:java_resources.bzl", "get_resources_map")
load(
    "@prelude//java:java_toolchain.bzl",
    "AbiGenerationMode",  # @unused Used as a type
    "DepFiles",
    "JavaToolchainInfo",  # @unused Used as a type
)
load(
    "@prelude//java/plugins:java_annotation_processor.bzl",
    "AnnotationProcessorProperties",  # @unused Used as a type
)
load(
    "@prelude//java/plugins:java_plugin.bzl",
    "PluginParams",  # @unused Used as a type
)
load(
    "@prelude//java/utils:java_utils.bzl",
    "CustomJdkInfo",  # @unused Used as a type
    "build_bootclasspath",
)
load(
    "@prelude//jvm:cd_jar_creator_util.bzl",
    "BuildMode",
    "OutputPaths",
    "TargetType",
    "base_qualified_name",
    "declare_prefixed_output",
    "define_output_paths",
    "encode_command",
    "generate_abi_jars",
    "get_abi_generation_mode",
    "get_compiling_deps_tset",
    "prepare_cd_exe",
    "prepare_final_jar",
    "setup_dep_files",
)
load("@prelude//utils:expect.bzl", "expect")

def create_jar_artifact_javacd(
        ctx: AnalysisContext,
        actions_identifier: [str, None],
        abi_generation_mode: [AbiGenerationMode, None],
        java_toolchain: JavaToolchainInfo,
        label,
        output: Artifact | None,
        javac_tool: [typing.Any, None],
        srcs: list[Artifact],
        remove_classes: list[str],
        resources: list[Artifact],
        resources_root: [str, None],
        manifest_file: Artifact | None,
        annotation_processor_properties: AnnotationProcessorProperties,
        plugin_params: [PluginParams, None],
        source_level: int,
        target_level: int,
        deps: list[Dependency],
        required_for_source_only_abi: bool,
        source_only_abi_deps: list[Dependency],
        extra_arguments: cmd_args,
        additional_classpath_entries: JavaCompilingDepsTSet | None,
        additional_compiled_srcs: Artifact | None,
        custom_jdk_info: CustomJdkInfo | None,
        is_building_android_binary: bool,
        is_creating_subtarget: bool = False,
        debug_port: [int, None] = None) -> JavaCompileOutputs:
    if javac_tool != None:
        # TODO(cjhopman): We can probably handle this better. I think we should be able to just use the non-javacd path.
        fail("cannot set explicit javac on library when using javacd")

    actions = ctx.actions
    resources_map = get_resources_map(java_toolchain, label.package, resources, resources_root)

    custom_bootclasspath = custom_jdk_info.bootclasspath if custom_jdk_info else []
    bootclasspath_entries = build_bootclasspath(custom_bootclasspath, source_level, java_toolchain)
    abi_generation_mode = get_abi_generation_mode(abi_generation_mode, java_toolchain, srcs, annotation_processor_properties)

    uses_experimental_content_based_path_hashing = java_toolchain.uses_experimental_content_based_path_hashing

    should_create_class_abi = (
        not additional_compiled_srcs and
        not is_creating_subtarget and
        (abi_generation_mode == AbiGenerationMode("class") or not is_building_android_binary)
    )
    if should_create_class_abi:
        class_abi_jar = declare_prefixed_output(actions, actions_identifier, "class-abi.jar", uses_experimental_content_based_path_hashing)
        class_abi_output_dir = declare_prefixed_output(actions, actions_identifier, "class_abi_dir", uses_experimental_content_based_path_hashing, dir = True)
    else:
        class_abi_jar = None
        class_abi_output_dir = None

    output_paths = define_output_paths(actions, actions_identifier, label, uses_experimental_content_based_path_hashing)

    compiling_deps_tset = get_compiling_deps_tset(actions, deps, additional_classpath_entries)

    # external javac does not support used classes
    track_class_usage = javac_tool == None and java_toolchain.track_class_usage
    define_javacd_action = partial(
        _define_javacd_action,
        actions,
        java_toolchain,
        should_create_class_abi,
        class_abi_jar,
        class_abi_output_dir,
        srcs,
        compiling_deps_tset,
        track_class_usage,
        debug_port,
        uses_experimental_content_based_path_hashing,
    )
    library_classpath_jars_tag = actions.artifact_tag()
    command_builder = _command_builder(
        javac_tool = javac_tool,
        label = label,
        srcs = srcs,
        remove_classes = remove_classes,
        annotation_processor_properties = annotation_processor_properties,
        plugin_params = plugin_params,
        manifest_file = manifest_file,
        source_level = source_level,
        target_level = target_level,
        compiling_deps_tset = compiling_deps_tset,
        bootclasspath_entries = bootclasspath_entries,
        system_image = custom_jdk_info.system_image if custom_jdk_info else None,
        abi_generation_mode = abi_generation_mode,
        resources_map = resources_map,
        extra_arguments = extra_arguments,
    )
    command = command_builder(
        build_mode = BuildMode("LIBRARY"),
        target_type = TargetType("library"),
        output_paths = output_paths,
        classpath_jars_tag = library_classpath_jars_tag,
        incremental_metadata_ignored_inputs_tag = None,
        source_only_abi_compiling_deps = [],
        track_class_usage = track_class_usage,
    )
    used_jars_json = define_javacd_action(
        category_prefix = "",
        actions_identifier = actions_identifier,
        encoded_command = command,
        qualified_name = base_qualified_name(label),
        output_paths = output_paths,
        classpath_jars_tag = library_classpath_jars_tag,
        abi_dir = class_abi_output_dir if should_create_class_abi else None,
        target_type = TargetType("library"),
        is_creating_subtarget = is_creating_subtarget,
        source_only_abi_compiling_deps = [],
    )
    jar_postprocessor = ctx.attrs.jar_postprocessor[RunInfo] if hasattr(ctx.attrs, "jar_postprocessor") and ctx.attrs.jar_postprocessor else None
    final_jar_output = prepare_final_jar(
        actions = actions,
        actions_identifier = actions_identifier,
        output = output,
        output_paths = output_paths,
        additional_compiled_srcs = additional_compiled_srcs,
        jar_builder = java_toolchain.jar_builder,
        jar_postprocessor = jar_postprocessor,
        jar_postprocessor_runner = java_toolchain.postprocessor_runner[RunInfo] if java_toolchain.postprocessor_runner else None,
        zip_scrubber = java_toolchain.zip_scrubber,
        uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing,
    )

    if not is_creating_subtarget:
        class_abi, source_abi, source_only_abi, classpath_abi, classpath_abi_dir = generate_abi_jars(
            actions = actions,
            actions_identifier = actions_identifier,
            label = label,
            abi_generation_mode = abi_generation_mode,
            additional_compiled_srcs = additional_compiled_srcs,
            is_building_android_binary = is_building_android_binary,
            class_abi_generator = java_toolchain.class_abi_generator,
            final_jar = final_jar_output.final_jar,
            compiling_deps_tset = compiling_deps_tset,
            source_only_abi_deps = source_only_abi_deps,
            class_abi_jar = class_abi_jar,
            class_abi_output_dir = class_abi_output_dir,
            track_class_usage = track_class_usage,
            encode_abi_command = command_builder,
            define_action = define_javacd_action,
            uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing,
        )

        abi_jar_snapshot = generate_java_classpath_snapshot(ctx.actions, java_toolchain.cp_snapshot_generator, ClasspathSnapshotGranularity("CLASS_MEMBER_LEVEL"), classpath_abi, actions_identifier, getattr(ctx.attrs, "uses_content_based_path_for_jar_snapshot", False))
        result = make_compile_outputs(
            full_library = final_jar_output.final_jar,
            preprocessed_library = final_jar_output.preprocessed_jar,
            class_abi = class_abi,
            source_abi = source_abi,
            source_only_abi = source_only_abi,
            classpath_abi = classpath_abi,
            classpath_abi_dir = classpath_abi_dir,
            required_for_source_only_abi = required_for_source_only_abi,
            annotation_processor_output = output_paths.annotations,
            abi_jar_snapshot = abi_jar_snapshot,
            used_jars_json = used_jars_json,
        )
    else:
        full_jar_snapshot = generate_java_classpath_snapshot(ctx.actions, java_toolchain.cp_snapshot_generator, ClasspathSnapshotGranularity("CLASS_MEMBER_LEVEL"), final_jar_output.final_jar, actions_identifier, getattr(ctx.attrs, "uses_content_based_path_for_jar_snapshot", False))
        result = make_compile_outputs(
            full_library = final_jar_output.final_jar,
            preprocessed_library = final_jar_output.preprocessed_jar,
            required_for_source_only_abi = required_for_source_only_abi,
            annotation_processor_output = output_paths.annotations,
            abi_jar_snapshot = full_jar_snapshot,
            used_jars_json = used_jars_json,
        )
    return result

def _command_builder(
        javac_tool: [str, RunInfo, Artifact, None],
        label: Label,
        srcs: list[Artifact],
        remove_classes: list[str],
        annotation_processor_properties: AnnotationProcessorProperties,
        plugin_params: [PluginParams, None],
        manifest_file: Artifact | None,
        source_level: int,
        target_level: int,
        compiling_deps_tset: [JavaCompilingDepsTSet, None],
        bootclasspath_entries: list[Artifact],
        system_image: Artifact | None,
        abi_generation_mode: AbiGenerationMode,
        resources_map: dict[str, Artifact],
        extra_arguments: cmd_args):
    return partial(
        encode_command,
        javac_tool = javac_tool,
        label = label,
        srcs = srcs,
        remove_classes = remove_classes,
        annotation_processor_properties = annotation_processor_properties,
        plugin_params = plugin_params,
        manifest_file = manifest_file,
        source_level = source_level,
        target_level = target_level,
        compiling_deps_tset = compiling_deps_tset,
        bootclasspath_entries = bootclasspath_entries,
        system_image = system_image,
        abi_generation_mode = abi_generation_mode,
        resources_map = resources_map,
        extra_arguments = extra_arguments,
        kotlin_extra_params = None,
        provide_classpath_snapshot = False,
    )

# buildifier: disable=uninitialized
def _define_javacd_action(
        # provided by factory
        actions: AnalysisActions,
        java_toolchain: JavaToolchainInfo,
        should_create_class_abi: bool,
        class_abi_jar: [Artifact, None],
        class_abi_output_dir: [Artifact, None],
        srcs: list[Artifact],
        compiling_deps_tset: [JavaCompilingDepsTSet, None],
        track_class_usage: bool,
        debug_port: [int, None],
        uses_experimental_content_based_path_hashing: bool,
        # end of factory provided
        category_prefix: str,
        actions_identifier: [str, None],
        encoded_command: struct,
        qualified_name: str,
        output_paths: OutputPaths,
        classpath_jars_tag: ArtifactTag,
        abi_dir: Artifact | None,
        target_type: TargetType,
        is_creating_subtarget: bool = False,
        source_only_abi_compiling_deps: list[JavaClasspathEntry] = []):
    expect(java_toolchain.javacd, "java_toolchain.javacd must be set for javacd protocol")
    compiler = java_toolchain.javacd
    exe, local_only = prepare_cd_exe(
        qualified_name,
        java = java_toolchain.java[RunInfo],
        class_loader_bootstrapper = java_toolchain.class_loader_bootstrapper,
        compiler = compiler,
        main_class = java_toolchain.javacd_main_class,
        worker = java_toolchain.javacd_worker[WorkerInfo] if java_toolchain.javacd_worker else None,
        remote_worker = java_toolchain.javacd_remote_worker[WorkerInfo] if java_toolchain.javacd_remote_worker else None,
        target_specified_debug_port = debug_port,
        toolchain_specified_debug_port = java_toolchain.javacd_debug_port,
        toolchain_specified_debug_target = java_toolchain.javacd_debug_target,
        extra_jvm_args = java_toolchain.javacd_jvm_args,
        extra_jvm_args_target = java_toolchain.javacd_jvm_args_target,
    )

    post_build_params = {}
    args = cmd_args()
    if target_type == TargetType("library") and should_create_class_abi:
        post_build_params["shouldCreateClassAbi"] = True
        post_build_params["libraryJar"] = output_paths.jar.as_output()
        post_build_params["abiJar"] = class_abi_jar.as_output()
        post_build_params["abiOutputDir"] = class_abi_output_dir.as_output()

    if target_type == TargetType("source_abi") or target_type == TargetType("source_only_abi"):
        post_build_params["abiJar"] = output_paths.jar.as_output()
        post_build_params["abiOutputDir"] = abi_dir.as_output()

    dep_files = {}
    used_jars_json_output = None
    if not is_creating_subtarget and srcs and (java_toolchain.dep_files == DepFiles("per_jar") or java_toolchain.dep_files == DepFiles("per_class")) and track_class_usage:
        abi_to_abi_dir_map = None
        if java_toolchain.dep_files == DepFiles("per_class"):
            if target_type == TargetType("source_only_abi"):
                abi_as_dir_deps = [dep for dep in source_only_abi_compiling_deps if dep.abi_as_dir]
                abi_to_abi_dir_map = [cmd_args(dep.abi, dep.abi_as_dir, delimiter = " ") for dep in abi_as_dir_deps]
                args.add(classpath_jars_tag.tag_artifacts(cmd_args(hidden = [dep.abi_as_dir for dep in abi_as_dir_deps])))
            elif compiling_deps_tset:
                abi_to_abi_dir_map = compiling_deps_tset.project_as_args("abi_to_abi_dir")
                args.add(classpath_jars_tag.tag_artifacts(cmd_args(hidden = compiling_deps_tset.project_as_args("abi_dirs"))))
        used_classes_json_outputs = [cmd_args(output_paths.jar.as_output(), format = "{}/used-classes.json", parent = 1)]
        used_jars_json_output = declare_prefixed_output(actions, actions_identifier, "jar/used-jars.json", uses_experimental_content_based_path_hashing)
        setup_dep_files(
            actions,
            actions_identifier,
            post_build_params,
            classpath_jars_tag,
            used_classes_json_outputs,
            used_jars_json_output,
            abi_to_abi_dir_map,
            uses_experimental_content_based_path_hashing,
        )

        dep_files["classpath_jars"] = classpath_jars_tag

    java_build_command = struct(
        buildCommand = encoded_command,
        postBuildParams = post_build_params,
    )

    proto = declare_prefixed_output(actions, actions_identifier, "jar_command.proto.json", uses_experimental_content_based_path_hashing)
    if dep_files:
        # This is a little bit convoluted due to the way that content-based paths affect argfiles.
        # If an unused tagged input changes, we don't want to re-run the action, but if it is a
        # content-based input that is written to the argfile, then the argfile will also change
        # and that would cause a re-run.
        #
        # We therefore write the argfile twice: the "real" argfile, which is used in the action
        # and tagged as unused so that it is not used for dep-file comparison, and an argfile
        # that uses placeholders instead of content-based paths, which is not tagged for dep-files
        # and therefore causes a dep-file miss if it changes.
        proto_dep_files_placeholder = declare_prefixed_output(actions, actions_identifier, "jar_command_for_dep_files.proto.json", uses_experimental_content_based_path_hashing)

        proto_for_args = classpath_jars_tag.tag_artifacts(actions.write_json(proto, java_build_command))
        proto_with_inputs_for_dep_files = actions.write_json(proto_dep_files_placeholder, java_build_command, with_inputs = True, use_dep_files_placeholder_for_content_based_paths = True)
        args.add(cmd_args(hidden = proto_with_inputs_for_dep_files))
    else:
        proto_for_args = actions.write_json(proto, java_build_command, with_inputs = True)

    args.add(
        "--action-id",
        qualified_name,
        "--command-file",
        proto_for_args,
    )

    actions.run(
        args,
        env = {
            "BUCK_CLASSPATH": compiler,
            "JAVACD_ABSOLUTE_PATHS_ARE_RELATIVE_TO_CWD": "1",
        },
        category = "{}javacd_jar".format(category_prefix),
        identifier = actions_identifier or "",
        dep_files = dep_files,
        allow_dep_file_cache_upload = True,
        allow_cache_upload = True,
        exe = exe,
        local_only = local_only,
        low_pass_filter = False,
        weight = 2,
        error_handler = java_toolchain.java_error_handler,
    )

    return used_jars_json_output
