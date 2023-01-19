# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//java:java_providers.bzl",
    "JavaLibraryInfo",
    "make_compile_outputs",
)
load("@prelude//java:java_resources.bzl", "get_resources_map")
load("@prelude//java:java_toolchain.bzl", "AbiGenerationMode")
load(
    "@prelude//jvm:cd_jar_creator_util.bzl",
    "OutputPaths",
    "TargetType",
    "add_output_paths_to_cmd_args",
    "base_qualified_name",
    "declare_prefixed_output",
    "define_output_paths",
    "encode_base_jar_command",
    "encode_jar_params",
    "generate_abi_jars",
    "prepare_final_jar",
)
load("@prelude//utils:utils.bzl", "expect", "map_idx")

buckPaths = struct(
    configuredBuckOut = "buck-out/v2",
    includeTargetConfigHash = True,
)

def create_jar_artifact_kotlincd(
        actions: "actions",
        actions_identifier: [str.type, None],
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

    expect(abi_generation_mode != AbiGenerationMode("source"), "abi_generation_mode: source is not supported in kotlincd")
    actual_abi_generation_mode = abi_generation_mode or AbiGenerationMode("class") if srcs else AbiGenerationMode("none")

    output_paths = define_output_paths(actions, actions_identifier)
    path_to_class_hashes_out = declare_prefixed_output(actions, actions_identifier, "classes.txt")

    def encode_kotlin_extra_params(kotlin_compiler_plugins):
        return struct(
            extraClassPaths = bootclasspath_entries,
            standardLibraryClassPath = kotlin_toolchain.kotlin_stdlib[JavaLibraryInfo].library_output.full_library,
            annotationProcessingClassPath = kotlin_toolchain.annotation_processing_jar[JavaLibraryInfo].library_output.full_library,
            kotlinCompilerPlugins = {plugin: {"params": plugin_options} if plugin_options else {} for plugin, plugin_options in kotlin_compiler_plugins.items()},
            kosabiPluginOptions = struct(
                kosabi_stubs_gen_plugin = kotlin_toolchain.kosabi_stubs_gen_plugin,
                kosabi_applicability_plugin = kotlin_toolchain.kosabi_applicability_plugin,
                kosabi_jvm_abi_gen_plugin = kotlin_toolchain.kosabi_jvm_abi_gen_plugin,
            ),
            friendPaths = [friend_path.library_output.abi for friend_path in map_idx(JavaLibraryInfo, friend_paths)],
            kotlinHomeLibraries = kotlin_toolchain.kotlin_home_libraries,
            jvmTarget = "1.8",
            kosabiJvmAbiGenEarlyTerminationMessagePrefix = "exception: java.lang.RuntimeException: Terminating compilation. We're done with ABI.",
            shouldUseJvmAbiGen = True,
            shouldVerifySourceOnlyAbiConstraints = actual_abi_generation_mode == AbiGenerationMode("source_only"),
            shouldGenerateAnnotationProcessingStats = True,
            extraKotlincArguments = extra_kotlinc_arguments,
            shouldRemoveKotlinCompilerFromClassPath = True,
        )

    def encode_build_target_value_extra_params():
        encoded_base_path = label.path
        short_name = label.name
        return struct(
            basePathForBaseName = encoded_base_path,
            cellRelativeBasePath = encoded_base_path,
            shortNameAndFlavorPostfix = short_name,
            shortName = short_name,
            buckPaths = buckPaths,
        )

    kotlin_extra_params = encode_kotlin_extra_params(kotlin_compiler_plugins)
    build_target_value_extra_params = encode_build_target_value_extra_params()

    def encode_library_command(
            output_paths: OutputPaths.type,
            path_to_class_hashes: "artifact") -> struct.type:
        target_type = TargetType("library")
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
            track_class_usage = True,
            build_target_value_extra_params = build_target_value_extra_params,
        )

        return struct(
            baseCommandParams = struct(
                withDownwardApi = True,
                hasAnnotationProcessing = True,
            ),
            libraryJarCommand = struct(
                kotlinExtraParams = kotlin_extra_params,
                baseJarCommand = base_jar_command,
                libraryJarBaseCommand = struct(
                    pathToClasses = output_paths.jar.as_output(),
                    rootOutput = output_paths.jar_parent.as_output(),
                    pathToClassHashes = path_to_class_hashes.as_output(),
                    annotationsPath = output_paths.annotations.as_output(),
                ),
            ),
        )

    def encode_abi_command(output_paths: OutputPaths.type, target_type: TargetType.type) -> struct.type:
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
            ap_params,
            plugin_params,
            extra_arguments,
            True,
            build_target_value_extra_params,
        )
        abi_params = encode_jar_params(remove_classes, output_paths)
        abi_command = struct(
            kotlinExtraParams = kotlin_extra_params,
            baseJarCommand = base_jar_command,
            abiJarParameters = abi_params,
        )

        return struct(
            baseCommandParams = struct(
                withDownwardApi = True,
            ),
            abiJarCommand = abi_command,
        )

    # buildifier: disable=uninitialized
    def define_kotlincd_action(
            category_prefix: str.type,
            actions_identifier: [str.type, None],
            encoded_command: struct.type,
            qualified_name: str.type,
            output_paths: OutputPaths.type,
            path_to_class_hashes: ["artifact", None]):
        proto = declare_prefixed_output(actions, actions_identifier, "jar_command.proto.json")
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

        event_pipe_out = declare_prefixed_output(actions, actions_identifier, "events.data")
        actions.run(
            cmd,
            env = {
                "BUCK_EVENT_PIPE": event_pipe_out.as_output(),
                "JAVACD_ABSOLUTE_PATHS_ARE_RELATIVE_TO_CWD": "1",
            },
            category = "{}kotlincd_jar".format(category_prefix),
            identifier = actions_identifier,
        )

    command = encode_library_command(output_paths, path_to_class_hashes_out)
    define_kotlincd_action(
        category_prefix = "",
        actions_identifier = actions_identifier,
        encoded_command = command,
        qualified_name = base_qualified_name(label),
        output_paths = output_paths,
        path_to_class_hashes = path_to_class_hashes_out,
    )

    final_jar = prepare_final_jar(
        actions = actions,
        actions_identifier = actions_identifier,
        output = None,
        output_paths = output_paths,
        additional_compiled_srcs = None,
        jar_builder = java_toolchain.jar_builder,
    )

    # kotlincd does not support source abi
    class_abi, _, source_only_abi, classpath_abi = generate_abi_jars(
        actions = actions,
        actions_identifier = actions_identifier,
        label = label,
        abi_generation_mode = actual_abi_generation_mode,
        additional_compiled_srcs = None,
        is_building_android_binary = is_building_android_binary,
        class_abi_generator = java_toolchain.class_abi_generator,
        final_jar = final_jar,
        encode_abi_command = encode_abi_command,
        define_action = define_kotlincd_action,
    )
    return make_compile_outputs(
        full_library = final_jar,
        class_abi = class_abi,
        source_only_abi = source_only_abi,
        classpath_abi = classpath_abi,
        required_for_source_only_abi = required_for_source_only_abi,
        annotation_processor_output = output_paths.annotations,
    )
