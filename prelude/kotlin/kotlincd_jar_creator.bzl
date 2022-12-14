# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//java:java_providers.bzl",
    "JavaLibraryInfo",
    "create_abi",
    "make_compile_outputs",
)
load("@prelude//java:java_resources.bzl", "get_resources_map")
load("@prelude//java:java_toolchain.bzl", "AbiGenerationMode")
load(
    "@prelude//jvm:cd_jar_creator_util.bzl",
    "OutputPaths",
    "TargetType",
    "add_java_7_8_bootclasspath",
    "add_output_paths_to_cmd_args",
    "base_qualified_name",
    "declare_prefixed_output",
    "define_output_paths",
    "encode_base_jar_command",
    "encode_path",
    "prepare_final_jar",
)
load("@prelude//utils:utils.bzl", "expect", "map_idx")

def create_jar_artifact_kotlincd(
        actions: "actions",
        actions_prefix: [str.type, None],
        abi_generation_mode: [AbiGenerationMode.type, None],
        java_toolchain: "JavaToolchainInfo",
        kotlin_toolchain: "KotlinToolchainInfo",
        label: "label",
        srcs: ["artifact"],
        remove_classes: [str.type],
        resources: ["artifact"],
        resources_root: [str.type, None],
        ap_params: ["AnnotationProcessorParams"],
        plugin_params: ["PluginParams", None],
        source_level: int.type,
        target_level: int.type,
        deps: ["dependency"],
        required_for_source_only_abi: bool.type,
        source_only_abi_deps: ["dependency"],
        extra_arguments: ["string"],
        additional_classpath_entries: ["artifact"],
        bootclasspath_entries: ["artifact"],
        is_building_android_binary: bool.type,
        friend_paths: ["dependency"],
        kotlin_compiler_plugins: dict.type,
        extra_kotlinc_arguments: [str.type]) -> "JavaCompileOutputs":
    resources_map = get_resources_map(
        java_toolchain = java_toolchain,
        package = label.package,
        resources = resources,
        resources_root = resources_root,
    )
    bootclasspath_entries = add_java_7_8_bootclasspath(
        target_level = target_level,
        bootclasspath_entries = bootclasspath_entries,
        java_toolchain = java_toolchain,
    )

    actual_abi_generation_mode = abi_generation_mode or AbiGenerationMode("class")
    expect(
        actual_abi_generation_mode == AbiGenerationMode("class"),
        "abi_generation_mode:{} is not supported in kotlincd".format(abi_generation_mode),
    )

    if actions_prefix:
        actions_prefix += "_"
    output_paths = define_output_paths(actions, actions_prefix)
    path_to_class_hashes_out = declare_prefixed_output(actions, actions_prefix, "classes.txt")

    def encode_kotlin_extra_params(kotlin_compiler_plugins):
        return struct(
            extraClassPaths = [encode_path(path.library_output.full_library) for path in map_idx(JavaLibraryInfo, kotlin_toolchain.kotlinc_classpath)] + [encode_path(path) for path in bootclasspath_entries],
            standardLibraryClassPath = encode_path(kotlin_toolchain.kotlin_stdlib[JavaLibraryInfo].library_output.full_library),
            annotationProcessingClassPath = encode_path(kotlin_toolchain.annotation_processing_jar[JavaLibraryInfo].library_output.full_library),
            kotlinCompilerPlugins = {plugin: {"params": plugin_options} if plugin_options else {} for plugin, plugin_options in kotlin_compiler_plugins.items()},
            friendPaths = [encode_path(friend_path.library_output.abi) for friend_path in map_idx(JavaLibraryInfo, friend_paths)],
            kotlinHomeLibraries = [encode_path(target) for target in kotlin_toolchain.kotlin_home_libraries],
            jvmTarget = "1.8",
            kosabiJvmAbiGenEarlyTerminationMessagePrefix = "exception: java.lang.RuntimeException: Terminating compilation. We're done with ABI.",
            shouldGenerateAnnotationProcessingStats = True,
            extraKotlincArguments = extra_kotlinc_arguments,
        )

    def encode_build_target_value_extra_params():
        return struct(
            basePathForBaseName = encode_path(label.path),
            shortNameAndFlavorPostfix = label.name,
            shortName = label.name,
            cellRelativeBasePath = encode_path(label.path),
            buckPaths = struct(
                configuredBuckOut = encode_path("buck-out/v2"),
                includeTargetConfigHash = True,
            ),
        )

    def encode_library_command(
            output_paths: OutputPaths.type,
            path_to_class_hashes: "artifact") -> struct.type:
        target_type = TargetType("library")
        build_target_value_extra_params = encode_build_target_value_extra_params()
        base_jar_command = encode_base_jar_command(
            target_type,
            output_paths,
            remove_classes,
            label,
            actions,
            deps,
            additional_classpath_entries,
            source_only_abi_deps,
            bootclasspath_entries,
            source_level,
            target_level,
            actual_abi_generation_mode,
            srcs,
            resources_map,
            ap_params = ap_params,
            plugin_params = plugin_params,
            extra_arguments = extra_arguments,
            track_class_usage = False,
            build_target_value_extra_params = build_target_value_extra_params,
        )

        return struct(
            baseCommandParams = struct(
                withDownwardApi = True,
                hasAnnotationProcessing = True,
            ),
            libraryJarCommand = struct(
                kotlinExtraParams = encode_kotlin_extra_params(kotlin_compiler_plugins),
                baseJarCommand = base_jar_command,
                libraryJarBaseCommand = struct(
                    pathToClasses = encode_path(output_paths.jar.as_output()),
                    rootOutput = encode_path(output_paths.jar_parent.as_output()),
                    pathToClassHashes = encode_path(path_to_class_hashes.as_output()),
                    annotationsPath = encode_path(output_paths.annotations.as_output()),
                ),
            ),
        )

    # buildifier: disable=uninitialized
    def define_kotlincd_action(
            actions_prefix: str.type,
            encoded_command: struct.type,
            qualified_name: str.type,
            output_paths: OutputPaths.type,
            path_to_class_hashes: ["artifact", None]):
        proto = declare_prefixed_output(actions, actions_prefix, "jar_command.proto.json")
        classpath_jars_tag = actions.artifact_tag()
        proto_with_inputs = classpath_jars_tag.tag_inputs(actions.write_json(proto, encoded_command, with_inputs = True))

        cmd = cmd_args([
            kotlin_toolchain.kotlinc[RunInfo],
            "--action-id",
            qualified_name,
            "--command-file",
            proto_with_inputs,
        ])
        cmd = add_output_paths_to_cmd_args(cmd, output_paths, path_to_class_hashes)

        event_pipe_out = declare_prefixed_output(actions, actions_prefix, "events.data")
        actions.run(
            cmd,
            env = {
                "BUCK_EVENT_PIPE": event_pipe_out.as_output(),
                "JAVACD_ABSOLUTE_PATHS_ARE_RELATIVE_TO_CWD": "1",
            },
            category = "{}kotlincd_jar".format(actions_prefix),
        )

    command = encode_library_command(output_paths, path_to_class_hashes_out)
    define_kotlincd_action(
        actions_prefix = actions_prefix,
        encoded_command = command,
        qualified_name = base_qualified_name(label),
        output_paths = output_paths,
        path_to_class_hashes = path_to_class_hashes_out,
    )

    final_jar = prepare_final_jar(
        actions = actions,
        actions_prefix = actions_prefix,
        output = None,
        output_paths = output_paths,
        additional_compiled_srcs = None,
        jar_builder = java_toolchain.jar_builder,
    )

    class_abi = None
    classpath_abi = None
    if not is_building_android_binary:
        class_abi = create_abi(actions, java_toolchain.class_abi_generator, final_jar)
        classpath_abi = class_abi

    return make_compile_outputs(
        full_library = final_jar,
        class_abi = class_abi,
        classpath_abi = classpath_abi,
        required_for_source_only_abi = required_for_source_only_abi,
        annotation_processor_output = output_paths.annotations,
    )
