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
    "JavaLibraryInfo",
    "generate_java_classpath_snapshot",
    "make_compile_outputs",
)
load("@prelude//java:java_resources.bzl", "get_resources_map")
load("@prelude//java:java_toolchain.bzl", "AbiGenerationMode", "DepFiles", "JavaToolchainInfo")
load(
    "@prelude//java/plugins:java_annotation_processor.bzl",
    "AnnotationProcessorProperties",  # @unused Used as type
)
load(
    "@prelude//java/plugins:java_plugin.bzl",
    "PluginParams",  # @unused Used as type
)
load(
    "@prelude//java/utils:java_utils.bzl",
    "CustomJdkInfo",  # @unused Used as type
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
    "get_compiling_deps_tset",
    "prepare_cd_exe",
    "prepare_final_jar",
    "setup_dep_files",
)
load("@prelude//kotlin:kotlin_toolchain.bzl", "KotlinToolchainInfo")
load("@prelude//kotlin:kotlin_utils.bzl", "get_kotlinc_compatible_target")
load("@prelude//utils:expect.bzl", "expect")
load("@prelude//utils:utils.bzl", "map_idx")

def create_jar_artifact_kotlincd(
        actions: AnalysisActions,
        actions_identifier: [str, None],
        abi_generation_mode: [AbiGenerationMode, None],
        java_toolchain: JavaToolchainInfo,
        kotlin_toolchain: KotlinToolchainInfo,
        javac_tool: [str, RunInfo, Artifact, None],
        label: Label,
        srcs: list[Artifact],
        remove_classes: list[str],
        resources: list[Artifact],
        resources_root: [str, None],
        annotation_processor_properties: AnnotationProcessorProperties,
        plugin_params: [PluginParams, None],
        manifest_file: Artifact | None,
        source_level: int,
        target_level: int,
        deps: list[Dependency],
        required_for_source_only_abi: bool,
        source_only_abi_deps: list[Dependency],
        extra_arguments: cmd_args,
        additional_classpath_entries: JavaCompilingDepsTSet | None,
        bootclasspath_entries: list[Artifact],
        custom_jdk_info: CustomJdkInfo | None,
        is_building_android_binary: bool,
        friend_paths: list[Dependency],
        kotlin_compiler_plugins: list[(Dependency, dict[str, str])],
        extra_kotlinc_arguments: list,
        incremental: bool,
        incremental_qe_applied: bool,
        enable_used_classes: bool,
        language_version: str,
        is_creating_subtarget: bool = False,
        optional_dirs: list[OutputArtifact] = [],
        jar_postprocessor: [RunInfo, None] = None,
        debug_port: [int, None] = None) -> (JavaCompileOutputs, Artifact):
    resources_map = get_resources_map(
        java_toolchain = java_toolchain,
        package = label.package,
        resources = resources,
        resources_root = resources_root,
    )

    expect(abi_generation_mode != AbiGenerationMode("source"), "abi_generation_mode: source is not supported in kotlincd")
    actual_abi_generation_mode = abi_generation_mode or AbiGenerationMode("class") if srcs else AbiGenerationMode("none")

    output_paths = define_output_paths(actions, actions_identifier, label)

    should_create_class_abi = \
        not is_creating_subtarget and \
        (actual_abi_generation_mode == AbiGenerationMode("class") or not is_building_android_binary) and \
        kotlin_toolchain.jvm_abi_gen_plugin != None
    if should_create_class_abi:
        class_abi_jar = declare_prefixed_output(actions, actions_identifier, "class-abi.jar")
        class_abi_output_dir = declare_prefixed_output(actions, actions_identifier, "class_abi_dir", dir = True)
        jvm_abi_gen = cmd_args(output_paths.jar.as_output(), format = "{}/jvm-abi-gen.jar", parent = 1)
        should_use_jvm_abi_gen = True
    else:
        class_abi_jar = None
        class_abi_output_dir = None
        jvm_abi_gen = None
        should_use_jvm_abi_gen = False

    should_kotlinc_run_incrementally = kotlin_toolchain.enable_incremental_compilation and incremental
    incremental_state_dir = declare_prefixed_output(actions, actions_identifier, "incremental_state", dir = True)

    compiling_deps_tset = get_compiling_deps_tset(actions, deps, additional_classpath_entries)

    # external javac does not support used classes
    track_class_usage = enable_used_classes and javac_tool == None and kotlin_toolchain.track_class_usage_plugin != None

    define_kotlincd_action = partial(
        _define_kotlincd_action,
        actions,
        java_toolchain,
        kotlin_toolchain,
        srcs,
        should_create_class_abi,
        class_abi_jar,
        jvm_abi_gen,
        class_abi_output_dir,
        optional_dirs,
        track_class_usage,
        compiling_deps_tset,
        debug_port,
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
        abi_generation_mode = actual_abi_generation_mode,
        resources_map = resources_map,
        extra_arguments = extra_arguments,
    )

    kotlin_extra_params = _encode_kotlin_extra_params(
        kotlin_toolchain = kotlin_toolchain,
        kotlin_compiler_plugins = kotlin_compiler_plugins,
        extra_kotlinc_arguments = extra_kotlinc_arguments,
        bootclasspath_entries = bootclasspath_entries,
        friend_paths = friend_paths,
        target_level = target_level,
        should_use_jvm_abi_gen = should_use_jvm_abi_gen,
        actual_abi_generation_mode = actual_abi_generation_mode,
        should_kotlinc_run_incrementally = should_kotlinc_run_incrementally,
        incremental_state_dir = incremental_state_dir,
        should_incremental_kotlinc_run_qe = incremental_qe_applied,
        language_version = language_version,
    )

    library_command_builder = command_builder(
        kotlin_extra_params = kotlin_extra_params,
        provide_classpath_snapshot = should_kotlinc_run_incrementally,
    )
    command = library_command_builder(
        build_mode = BuildMode("LIBRARY"),
        target_type = TargetType("library"),
        output_paths = output_paths,
        classpath_jars_tag = library_classpath_jars_tag,
        source_only_abi_compiling_deps = [],
        track_class_usage = track_class_usage,
    )
    proto, used_jars_json = define_kotlincd_action(
        category_prefix = "",
        actions_identifier = actions_identifier,
        encoded_command = command,
        qualified_name = base_qualified_name(label),
        output_paths = output_paths,
        classpath_jars_tag = library_classpath_jars_tag,
        abi_dir = class_abi_output_dir if should_create_class_abi else None,
        target_type = TargetType("library"),
        is_creating_subtarget = is_creating_subtarget,
        incremental_state_dir = incremental_state_dir,
        should_action_run_incrementally = should_kotlinc_run_incrementally,
    )

    final_jar_output = prepare_final_jar(
        actions = actions,
        actions_identifier = actions_identifier,
        output = None,
        output_paths = output_paths,
        additional_compiled_srcs = None,
        jar_builder = java_toolchain.jar_builder,
        jar_postprocessor = jar_postprocessor,
        jar_postprocessor_runner = java_toolchain.postprocessor_runner[RunInfo] if java_toolchain.postprocessor_runner else None,
        zip_scrubber = java_toolchain.zip_scrubber,
    )

    if not is_creating_subtarget:
        kotlin_extra_params = _encode_kotlin_extra_params(
            kotlin_toolchain = kotlin_toolchain,
            kotlin_compiler_plugins = kotlin_compiler_plugins,
            extra_kotlinc_arguments = extra_kotlinc_arguments,
            bootclasspath_entries = bootclasspath_entries,
            friend_paths = friend_paths,
            target_level = target_level,
            should_use_jvm_abi_gen = should_use_jvm_abi_gen,
            actual_abi_generation_mode = actual_abi_generation_mode,
            should_kotlinc_run_incrementally = False,
            incremental_state_dir = None,
            should_incremental_kotlinc_run_qe = False,
            language_version = language_version,
        )
        abi_command_builder = command_builder(
            kotlin_extra_params = kotlin_extra_params,
            provide_classpath_snapshot = False,
        )

        # kotlincd does not support source abi
        class_abi, _, source_only_abi, classpath_abi, classpath_abi_dir = generate_abi_jars(
            actions = actions,
            actions_identifier = actions_identifier,
            label = label,
            abi_generation_mode = actual_abi_generation_mode,
            additional_compiled_srcs = None,
            is_building_android_binary = is_building_android_binary,
            class_abi_generator = java_toolchain.class_abi_generator,
            final_jar = final_jar_output.final_jar,
            compiling_deps_tset = compiling_deps_tset,
            source_only_abi_deps = source_only_abi_deps,
            class_abi_jar = class_abi_jar,
            class_abi_output_dir = class_abi_output_dir,
            track_class_usage = True,
            encode_abi_command = abi_command_builder,
            define_action = define_kotlincd_action,
        )
        abi_jar_snapshot = generate_java_classpath_snapshot(actions, java_toolchain.cp_snapshot_generator, ClasspathSnapshotGranularity("CLASS_MEMBER_LEVEL"), classpath_abi, actions_identifier)
        return make_compile_outputs(
            full_library = final_jar_output.final_jar,
            preprocessed_library = final_jar_output.preprocessed_jar,
            class_abi = class_abi,
            source_only_abi = source_only_abi,
            classpath_abi = classpath_abi,
            classpath_abi_dir = classpath_abi_dir,
            required_for_source_only_abi = required_for_source_only_abi,
            annotation_processor_output = output_paths.annotations,
            incremental_state_dir = incremental_state_dir,
            abi_jar_snapshot = abi_jar_snapshot,
            used_jars_json = used_jars_json,
        ), proto
    else:
        full_jar_snapshot = generate_java_classpath_snapshot(actions, java_toolchain.cp_snapshot_generator, ClasspathSnapshotGranularity("CLASS_MEMBER_LEVEL"), final_jar_output.final_jar, actions_identifier)
        return make_compile_outputs(
            full_library = final_jar_output.final_jar,
            preprocessed_library = final_jar_output.preprocessed_jar,
            required_for_source_only_abi = required_for_source_only_abi,
            annotation_processor_output = output_paths.annotations,
            abi_jar_snapshot = full_jar_snapshot,
            used_jars_json = used_jars_json,
        ), proto

def _encode_kotlin_extra_params(
        kotlin_toolchain: KotlinToolchainInfo,
        kotlin_compiler_plugins: list[(Dependency, dict[str, str])],
        extra_kotlinc_arguments: list,
        bootclasspath_entries: list[Artifact],
        friend_paths: list[Dependency],
        target_level: int,
        should_use_jvm_abi_gen: bool,
        actual_abi_generation_mode: AbiGenerationMode,
        should_kotlinc_run_incrementally: bool,
        incremental_state_dir: Artifact | None,
        should_incremental_kotlinc_run_qe: bool,
        language_version: str):
    kosabiPluginOptionsMap = {}
    if kotlin_toolchain.kosabi_stubs_gen_plugin != None:
        kosabiPluginOptionsMap["kosabi_stubs_gen_plugin"] = kotlin_toolchain.kosabi_stubs_gen_plugin

    if kotlin_toolchain.kosabi_source_modifier_plugin != None:
        kosabiPluginOptionsMap["kosabi_source_modifier_plugin"] = kotlin_toolchain.kosabi_source_modifier_plugin

    if kotlin_toolchain.kosabi_applicability_plugin != None:
        kosabiPluginOptionsMap["kosabi_applicability_plugin"] = kotlin_toolchain.kosabi_applicability_plugin

    if kotlin_toolchain.kosabi_jvm_abi_gen_plugin != None:
        kosabiPluginOptionsMap["kosabi_jvm_abi_gen_plugin"] = kotlin_toolchain.kosabi_jvm_abi_gen_plugin

    return struct(
        extraClassPaths = bootclasspath_entries,
        standardLibraryClassPath = kotlin_toolchain.kotlin_stdlib[JavaLibraryInfo].library_output.full_library,
        annotationProcessingClassPath = kotlin_toolchain.annotation_processing_jar[JavaLibraryInfo].library_output.full_library,
        jvmAbiGenPlugin = kotlin_toolchain.jvm_abi_gen_plugin,
        kotlinCompilerPlugins = {plugin[DefaultInfo].default_outputs[0]: {"params": plugin_options} for plugin, plugin_options in kotlin_compiler_plugins},
        kosabiPluginOptions = struct(**kosabiPluginOptionsMap),
        friendPaths = [friend_path.library_output.abi for friend_path in map_idx(JavaLibraryInfo, friend_paths) if friend_path.library_output],
        kotlinHomeLibraries = kotlin_toolchain.kotlin_home_libraries,
        jvmTarget = get_kotlinc_compatible_target(str(target_level)),
        kosabiJvmAbiGenEarlyTerminationMessagePrefix = "exception: java.lang.RuntimeException: Terminating compilation. We're done with ABI.",
        shouldUseJvmAbiGen = should_use_jvm_abi_gen,
        shouldVerifySourceOnlyAbiConstraints = actual_abi_generation_mode == AbiGenerationMode("source_only"),
        shouldGenerateAnnotationProcessingStats = True,
        extraKotlincArguments = extra_kotlinc_arguments,
        depTrackerPlugin = kotlin_toolchain.track_class_usage_plugin,
        shouldKotlincRunIncrementally = should_kotlinc_run_incrementally,
        incrementalStateDir = incremental_state_dir.as_output() if incremental_state_dir else None,
        shouldIncrementalKotlicRunQe = should_incremental_kotlinc_run_qe,
        shouldUseStandaloneKosabi = kotlin_toolchain.kosabi_standalone,
        languageVersion = language_version,
    )

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
        _encode_kotlin_command,
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
    )

def _encode_kotlin_command(
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
        extra_arguments: cmd_args,
        kotlin_extra_params: [struct, None],
        provide_classpath_snapshot: bool):
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
        kotlin_extra_params = kotlin_extra_params,
        provide_classpath_snapshot = provide_classpath_snapshot,
    )

# buildifier: disable=uninitialized
# buildifier: disable=unused-variable
def _define_kotlincd_action(
        # provided by factory
        actions: AnalysisActions,
        java_toolchain: JavaToolchainInfo,
        kotlin_toolchain: KotlinToolchainInfo,
        srcs: list[Artifact],
        should_create_class_abi: bool,
        class_abi_jar: [Artifact, None],
        jvm_abi_gen: [cmd_args, None],
        class_abi_output_dir: [Artifact, None],
        optional_dirs: list[OutputArtifact],
        track_class_usage: bool,
        compiling_deps_tset: [JavaCompilingDepsTSet, None],
        debug_port: [int, None],
        # end of factory provided
        category_prefix: str,
        actions_identifier: [str, None],
        encoded_command: struct,
        qualified_name: str,
        output_paths: OutputPaths,
        classpath_jars_tag: ArtifactTag,
        abi_dir: Artifact | None,
        target_type: TargetType,
        source_only_abi_compiling_deps: list[JavaClasspathEntry] = [],
        is_creating_subtarget: bool = False,
        incremental_state_dir: Artifact | None = None,
        should_action_run_incrementally: bool = False):
    _unused = source_only_abi_compiling_deps

    proto = declare_prefixed_output(actions, actions_identifier, "jar_command.proto.json")

    compiler = kotlin_toolchain.kotlinc[DefaultInfo].default_outputs[0]
    exe, local_only = prepare_cd_exe(
        qualified_name,
        java = java_toolchain.java[RunInfo],
        class_loader_bootstrapper = kotlin_toolchain.class_loader_bootstrapper,
        compiler = compiler,
        main_class = kotlin_toolchain.kotlincd_main_class,
        worker = kotlin_toolchain.kotlincd_worker[WorkerInfo] if kotlin_toolchain.kotlincd_worker else None,
        target_specified_debug_port = debug_port,
        toolchain_specified_debug_port = kotlin_toolchain.kotlincd_debug_port,
        toolchain_specified_debug_target = kotlin_toolchain.kotlincd_debug_target,
        extra_jvm_args = kotlin_toolchain.kotlincd_jvm_args,
        extra_jvm_args_target = kotlin_toolchain.kotlincd_jvm_args_target,
    )

    args = cmd_args()
    post_build_params = {}
    if target_type == TargetType("library") and should_create_class_abi:
        post_build_params["shouldCreateClassAbi"] = True
        post_build_params["libraryJar"] = output_paths.jar.as_output()
        post_build_params["abiJar"] = class_abi_jar.as_output()
        post_build_params["jvmAbiGen"] = jvm_abi_gen
        post_build_params["abiOutputDir"] = class_abi_output_dir.as_output()

    if target_type == TargetType("source_abi") or target_type == TargetType("source_only_abi"):
        post_build_params["abiJar"] = output_paths.jar.as_output()
        post_build_params["abiOutputDir"] = abi_dir.as_output()

    if optional_dirs:
        post_build_params["optionalDirs"] = optional_dirs

    if incremental_state_dir:
        post_build_params["incrementalStateDir"] = incremental_state_dir.as_output()

    dep_files = {}
    used_jars_json_output = None
    if not is_creating_subtarget and srcs and (kotlin_toolchain.dep_files == DepFiles("per_jar") or kotlin_toolchain.dep_files == DepFiles("per_class")) and target_type == TargetType("library") and track_class_usage:
        used_classes_json_outputs = [
            cmd_args(output_paths.jar.as_output(), format = "{}/used-classes.json", parent = 1),
            cmd_args(output_paths.jar.as_output(), format = "{}/kotlin-used-classes.json", parent = 1),
        ]
        used_jars_json_output = declare_prefixed_output(actions, actions_identifier, "jar/used-jars.json")
        args = setup_dep_files(
            actions,
            actions_identifier,
            args,
            post_build_params,
            classpath_jars_tag,
            used_classes_json_outputs,
            used_jars_json_output,
            compiling_deps_tset.project_as_args("abi_to_abi_dir") if kotlin_toolchain.dep_files == DepFiles("per_class") and compiling_deps_tset else None,
        )

        dep_files["classpath_jars"] = classpath_jars_tag
    kotlin_build_command = struct(
        buildCommand = encoded_command,
        postBuildParams = post_build_params,
    )
    proto_with_inputs = actions.write_json(proto, kotlin_build_command, with_inputs = True)

    args.add(
        "--action-id",
        qualified_name,
        "--command-file",
        proto_with_inputs,
    )

    if should_action_run_incrementally:
        args.add(
            "--incremental-metadata-file",
            _create_incremental_proto(actions, actions_identifier, kotlin_build_command, kotlin_toolchain.kotlin_version),
        )

    incremental_run_params = {
        "metadata_env_var": "ACTION_METADATA",
        "metadata_path": "action_metadata.json",
        "no_outputs_cleanup": True,
    } if should_action_run_incrementally else {}
    actions.run(
        args,
        env = {
            "BUCK_CLASSPATH": compiler,
            "JAVACD_ABSOLUTE_PATHS_ARE_RELATIVE_TO_CWD": "1",
        },
        category = "{}kotlincd_jar".format(category_prefix),
        identifier = actions_identifier,
        dep_files = dep_files,
        allow_dep_file_cache_upload = True,
        allow_cache_upload = True,
        exe = exe,
        local_only = local_only,
        low_pass_filter = False,
        weight = 2,
        error_handler = kotlin_toolchain.kotlin_error_handler,
        **incremental_run_params
    )
    return proto, used_jars_json_output

def _create_incremental_proto(actions: AnalysisActions, actions_identifier: [str, None], kotlin_build_command: struct, kotlin_version: str):
    incremental_meta_data_output = declare_prefixed_output(actions, actions_identifier, "incremental_metadata.proto.json")
    incremental_meta_data = struct(
        version = 1,
        track_class_usage = kotlin_build_command.buildCommand.baseJarCommand.trackClassUsage,
        should_use_jvm_abi_gen = kotlin_build_command.buildCommand.kotlinExtraParams.shouldUseJvmAbiGen,
        kotlin_version = kotlin_version,
    )
    return actions.write_json(incremental_meta_data_output, incremental_meta_data, with_inputs = True)
