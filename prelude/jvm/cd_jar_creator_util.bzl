# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//java:java_providers.bzl",
    "JavaClasspathEntry",
    "JavaCompilingDepsTSet",
    "JavaLibraryInfo",
    "create_abi",
    "derive_compiling_deps",
)
load("@prelude//java:java_toolchain.bzl", "AbiGenerationMode", "JavaToolchainInfo")
load("@prelude//java/plugins:java_annotation_processor.bzl", "AnnotationProcessorProperties")  # @unused Used as type
load(
    "@prelude//java/plugins:java_plugin.bzl",
    "PluginParams",  # @unused Used as type
)
load("@prelude//java/utils:java_utils.bzl", "declare_prefixed_name")
load("@prelude//utils:expect.bzl", "expect")

def declare_prefixed_output(actions: AnalysisActions, prefix: [str, None], output: str, dir: bool = False) -> Artifact:
    return actions.declare_output(declare_prefixed_name(output, prefix), dir = dir)

# The library and the toolchain can both set a specific abi generation
# mode. The toolchain's setting is effectively the "highest" form of abi
# that the toolchain supports and then the same for the target and we will choose
# the "highest" that both support.
def _resolve_abi_generation_mode(abi_generation_mode: [AbiGenerationMode, None], java_toolchain: JavaToolchainInfo) -> AbiGenerationMode:
    if abi_generation_mode == None:
        return java_toolchain.abi_generation_mode
    for mode in [AbiGenerationMode("none"), AbiGenerationMode("class"), AbiGenerationMode("source"), AbiGenerationMode("source_only")]:
        if mode in (java_toolchain.abi_generation_mode, abi_generation_mode):
            return mode
    fail("resolving abi generation mode failed. had `{}` and `{}`".format(java_toolchain.abi_generation_mode, abi_generation_mode))

def get_abi_generation_mode(
        abi_generation_mode: [AbiGenerationMode, None],
        java_toolchain: JavaToolchainInfo,
        srcs: list[Artifact],
        annotation_processor_properties: AnnotationProcessorProperties) -> AbiGenerationMode:
    resolved_mode = AbiGenerationMode("none") if not srcs else _resolve_abi_generation_mode(abi_generation_mode, java_toolchain)
    if resolved_mode == AbiGenerationMode("source_only"):
        def plugins_support_source_only_abi():
            for ap in annotation_processor_properties.annotation_processors:
                if ap.affects_abi and not ap.supports_source_only_abi:
                    return False
            return True

        if not plugins_support_source_only_abi():
            resolved_mode = AbiGenerationMode("source")
    return resolved_mode

# We need to construct a complex protobuf message. We do it by constructing
# a bunch of nested structs and then use write_json to get a json-encoded
# protobuf message.
#
# The definition is in xplat/toolchains/android/sdk/src/com/facebook/buck/cd/resources/proto/javacd.proto
# and xplat/toolchains/android/sdk/src/com/facebook/buck/cd/resources/proto/kotlincd.proto and is, sadly,
# poorly documented.

# Our protobuf format mostly encodes paths in RelPath/AbsPath structs with a single "path" field.
# Note that we don't actually use abspath and instead enable JAVACD_ABSOLUTE_PATHS_ARE_RELATIVE_TO_CWD
TargetType = enum("library", "source_abi", "source_only_abi")
BuildMode = enum("LIBRARY", "ABI")

def encode_abi_generation_mode(mode: AbiGenerationMode) -> str:
    return {
        AbiGenerationMode("none"): "NONE",
        AbiGenerationMode("class"): "CLASS",
        AbiGenerationMode("source"): "SOURCE",
        AbiGenerationMode("source_only"): "SOURCE_ONLY",
    }[mode]

def encode_target_type(target_type: TargetType) -> str:
    if target_type == TargetType("library"):
        return "LIBRARY"
    if target_type == TargetType("source_abi"):
        return "SOURCE_ABI"
    if target_type == TargetType("source_only_abi"):
        return "SOURCE_ONLY_ABI"
    fail()

OutputPaths = record(
    jar = Artifact,
    classes = Artifact,
    annotations = Artifact,
)

def qualified_name_with_subtarget(label: Label) -> str:
    if label.sub_target:
        return "{}:{}[{}]".format(label.path, label.name, label.sub_target[0])
    return "{}:{}".format(label.path, label.name)

# Converted to str so that we get the right result when written as json.
def base_qualified_name(label: Label) -> str:
    return "{}:{}".format(label.path, label.name)

def get_qualified_name(label: Label, target_type: TargetType) -> str:
    # These should match the names for subtargets in java_library.bzl
    return {
        TargetType("library"): base_qualified_name(label),
        TargetType("source_abi"): base_qualified_name(label) + "[source-abi]",
        TargetType("source_only_abi"): base_qualified_name(label) + "[source-only-abi]",
    }[target_type]

def define_output_paths(actions: AnalysisActions, prefix: [str, None], label: Label) -> OutputPaths:
    # currently, javacd requires that at least some outputs are in the root
    # output dir. so we put all of them there. If javacd is updated we
    # could consolidate some of these into one subdir.
    return OutputPaths(
        jar = declare_prefixed_output(actions, prefix, "jar/{}.jar".format(label.name)),
        classes = declare_prefixed_output(actions, prefix, "__classes__", dir = True),
        annotations = declare_prefixed_output(actions, prefix, "__gen__", dir = True),
    )

def encode_output_paths(label: Label, paths: OutputPaths, target_type: TargetType) -> struct:
    paths = struct(
        classesDir = paths.classes.as_output(),
        outputJarDirPath = cmd_args(paths.jar.as_output(), parent = 1),
        annotationPath = paths.annotations.as_output(),
        outputJarPath = paths.jar.as_output(),
    )

    return struct(
        libraryPaths = paths if target_type == TargetType("library") else None,
        sourceAbiPaths = paths if target_type == TargetType("source_abi") else None,
        sourceOnlyAbiPaths = paths if target_type == TargetType("source_only_abi") else None,
        libraryTargetFullyQualifiedName = base_qualified_name(label),
    )

def encode_jar_params(remove_classes: list[str], output_paths: OutputPaths, manifest_file: Artifact | None) -> struct:
    return struct(
        jarPath = output_paths.jar.as_output(),
        removeEntryPredicate = struct(
            patterns = remove_classes,
        ),
        entriesToJar = [output_paths.classes.as_output()],
        manifestFile = manifest_file,
        duplicatesLogLevel = "FINE",
    )

def command_abi_generation_mode(target_type: TargetType, abi_generation_mode: [AbiGenerationMode, None]) -> [AbiGenerationMode, None]:
    # We want the library target to have the real generation mode, but the source one's just use their own.
    # The generation mode will be used elsewhere to setup the target's classpath entry to use the correct abi.
    if target_type == TargetType("source_abi"):
        return AbiGenerationMode("source")
    if target_type == TargetType("source_only_abi"):
        return AbiGenerationMode("source_only")
    return abi_generation_mode

def get_compiling_deps_tset(
        actions: AnalysisActions,
        deps: list[Dependency],
        additional_classpath_entries: JavaCompilingDepsTSet | None) -> [JavaCompilingDepsTSet, None]:
    compiling_deps_tset = derive_compiling_deps(actions, None, deps)
    if additional_classpath_entries:
        if compiling_deps_tset == None:
            compiling_deps_tset = additional_classpath_entries
        else:
            compiling_deps_tset = actions.tset(JavaCompilingDepsTSet, children = [compiling_deps_tset, additional_classpath_entries])

    return compiling_deps_tset

def _get_source_only_abi_compiling_deps(compiling_deps_tset: [JavaCompilingDepsTSet, None], source_only_abi_deps: list[Dependency]) -> list[JavaClasspathEntry]:
    source_only_abi_compiling_deps = []
    if compiling_deps_tset:
        source_only_abi_deps_filter = {}
        for d in source_only_abi_deps:
            info = d.get(JavaLibraryInfo)
            if not info:
                fail("source_only_abi_deps must produce a JavaLibraryInfo but '{}' does not, please remove it".format(d.label))
            if info.library_output:
                source_only_abi_deps_filter[info.library_output.abi] = True

        def filter_compiling_deps(dep):
            return dep.abi in source_only_abi_deps_filter or dep.required_for_source_only_abi

        source_only_abi_compiling_deps = [compiling_dep for compiling_dep in list(compiling_deps_tset.traverse()) if filter_compiling_deps(compiling_dep)]
    return source_only_abi_compiling_deps

# buildifier: disable=unused-variable
def encode_ap_params(annotation_processor_properties: AnnotationProcessorProperties, target_type: TargetType) -> [struct, None]:
    # buck1 oddly only inspects annotation processors, not plugins for
    # abi/source-only abi related things, even though the plugin rules
    # support the flags. we apply it to both.
    encoded_ap_params = None
    if annotation_processor_properties.annotation_processors:
        encoded_ap_params = struct(
            parameters = annotation_processor_properties.annotation_processor_params,
            pluginProperties = [],
        )
        for ap in annotation_processor_properties.annotation_processors:
            # We should also filter out non-abi-affecting APs for source-abi, but buck1 doesn't and so we have lots that depend on not filtering them there.
            if target_type == TargetType("source_only_abi") and not ap.affects_abi:
                continue
            if ap.deps or ap.processors:
                encoded_ap_params.pluginProperties.append(
                    struct(
                        canReuseClassLoader = not ap.isolate_class_loader,
                        doesNotAffectAbi = not ap.affects_abi,
                        supportsAbiGenerationFromSource = ap.supports_source_only_abi,
                        runsOnJavaOnly = ap.runs_on_java_only,
                        processorNames = ap.processors,
                        classpath = ap.deps.project_as_json("javacd_json") if ap.deps else [],
                        pathParams = {},
                    ),
                )
    return encoded_ap_params

def encode_plugin_params(plugin_params: [PluginParams, None]) -> [struct, None]:
    encoded_plugin_params = None
    if plugin_params:
        encoded_plugin_params = struct(
            parameters = [],
            pluginProperties = [
                encode_plugin_properties(processor, arguments, plugin_params)
                for processor, arguments in plugin_params.processors
            ],
        )
    return encoded_plugin_params

def encode_plugin_properties(processor: str, arguments: cmd_args, plugin_params: PluginParams) -> struct:
    return struct(
        canReuseClassLoader = False,
        doesNotAffectAbi = False,
        supportsAbiGenerationFromSource = False,
        runsOnJavaOnly = False,
        processorNames = [processor],
        classpath = plugin_params.deps.project_as_json("javacd_json") if plugin_params.deps else [],
        pathParams = {},
        arguments = arguments,
    )

def encode_base_jar_command(
        javac_tool: [str, RunInfo, Artifact, None],
        target_type: TargetType,
        output_paths: OutputPaths,
        remove_classes: list[str],
        label: Label,
        compiling_deps_tset: [JavaCompilingDepsTSet, None],
        classpath_jars_tag: ArtifactTag,
        bootclasspath_entries: list[Artifact],
        source_level: int,
        target_level: int,
        abi_generation_mode: [AbiGenerationMode, None],
        srcs: list[Artifact],
        resources_map: dict[str, Artifact],
        annotation_processor_properties: AnnotationProcessorProperties,
        plugin_params: [PluginParams, None],
        manifest_file: Artifact | None,
        extra_arguments: cmd_args,
        source_only_abi_compiling_deps: list[JavaClasspathEntry],
        track_class_usage: bool,
        provide_classpath_snapshot: bool = False) -> struct:
    jar_parameters = encode_jar_params(remove_classes, output_paths, manifest_file)
    qualified_name = get_qualified_name(label, target_type)
    if target_type == TargetType("source_only_abi"):
        compiling_classpath = classpath_jars_tag.tag_artifacts([dep.abi for dep in source_only_abi_compiling_deps])
        compiling_classpath_snapshot = {}
    else:
        expect(len(source_only_abi_compiling_deps) == 0)
        compiling_deps_list = filter(None, list(compiling_deps_tset.traverse(ordering = "topological"))) if compiling_deps_tset else []
        compiling_classpath = classpath_jars_tag.tag_artifacts(
            [dep.abi for dep in compiling_deps_list],
        )

        # The snapshot inputs are tagged for association with dep_files, but they are not marked as used,
        # as they serve the incremental compiler's internal needs,
        # which are utilized after the build system has determined whether a rebuild is necessary.
        compiling_classpath_snapshot = classpath_jars_tag.tag_artifacts({dep.abi: dep.abi_jar_snapshot or "" for dep in compiling_deps_list}) if provide_classpath_snapshot else {}

    build_target_value = struct(
        fullyQualifiedName = qualified_name,
        type = encode_target_type(target_type),
    )
    if javac_tool:
        resolved_javac = {
            "externalJavac": {
                "commandPrefix": [javac_tool],
                "shortName": str(javac_tool),
            },
        }
    else:
        resolved_javac = {"jsr199Javac": {}}
    resolved_java_options = struct(
        bootclasspathList = bootclasspath_entries,
        languageLevelOptions = struct(
            sourceLevel = source_level,
            targetLevel = target_level,
        ),
        debug = True,
        javaAnnotationProcessorParams = encode_ap_params(annotation_processor_properties, target_type),
        standardJavacPluginParams = encode_plugin_params(plugin_params),
        extraArguments = extra_arguments,
    )

    return struct(
        outputPathsValue = encode_output_paths(label, output_paths, target_type),
        compileTimeClasspathPaths = compiling_classpath,
        compileTimeClasspathSnapshotPaths = compiling_classpath_snapshot,
        javaSrcs = srcs,
        # We use "class" abi compatibility to match buck1 (other compatibility modes are used for abi verification.
        abiCompatibilityMode = encode_abi_generation_mode(AbiGenerationMode("class")),
        abiGenerationMode = encode_abi_generation_mode(command_abi_generation_mode(target_type, abi_generation_mode)),
        trackClassUsage = track_class_usage,
        configuredBuckOut = "buck-out/v2",
        buildTargetValue = build_target_value,
        resourcesMap = [
            {
                "key": v,
                "value": cmd_args([output_paths.classes.as_output(), "/", k], delimiter = ""),
            }
            for (k, v) in resources_map.items()
        ],
        resolvedJavac = resolved_javac,
        resolvedJavacOptions = resolved_java_options,
        jarParameters = jar_parameters,
        pathToClasses = output_paths.jar.as_output(),
        annotationsPath = output_paths.annotations.as_output(),
    )

def setup_dep_files(
        actions: AnalysisActions,
        actions_identifier: [str, None],
        cmd: cmd_args,
        post_build_params: dict,
        classpath_jars_tag: ArtifactTag,
        used_classes_json_outputs: list[cmd_args],
        used_jars_json_output: Artifact,
        abi_to_abi_dir_map: [TransitiveSetArgsProjection, list[cmd_args], None],
        hidden = ["artifact"]) -> cmd_args:
    dep_file = declare_prefixed_output(actions, actions_identifier, "dep_file.txt")

    new_cmd_args = []
    new_cmd_hidden = []
    new_cmd_args.append(cmd)
    post_build_params["usedClasses"] = used_classes_json_outputs
    post_build_params["depFile"] = classpath_jars_tag.tag_artifacts(dep_file.as_output())
    post_build_params["usedJarsFile"] = used_jars_json_output.as_output()

    if abi_to_abi_dir_map:
        abi_to_abi_dir_map_file = declare_prefixed_output(actions, actions_identifier, "abi_to_abi_dir_map")
        actions.write(abi_to_abi_dir_map_file, abi_to_abi_dir_map)
        post_build_params["jarToJarDirMap"] = abi_to_abi_dir_map_file
        if isinstance(abi_to_abi_dir_map, TransitiveSetArgsProjection):
            new_cmd_hidden.append(classpath_jars_tag.tag_artifacts(abi_to_abi_dir_map))
        for hidden_artifact in hidden:
            new_cmd_hidden.append(classpath_jars_tag.tag_artifacts(hidden_artifact))

    return cmd_args(hidden = new_cmd_hidden)

FORCE_PERSISTENT_WORKERS = read_root_config("build", "require_persistent_workers", "false").lower() == "true"

def prepare_cd_exe(
        qualified_name: str,
        java: RunInfo,
        class_loader_bootstrapper: Artifact,
        compiler: Artifact,
        main_class: str,
        worker: WorkerInfo,
        target_specified_debug_port: [int, None],
        toolchain_specified_debug_port: [int, None],
        toolchain_specified_debug_target: [Label, None],
        extra_jvm_args: list[str],
        extra_jvm_args_target: list[Label]) -> tuple:
    local_only = False
    jvm_args = ["-XX:-MaxFDLimit"]

    # The variables 'extra_jvm_args' and 'extra_jvm_args_target' are generally used, but they are primarily designed for profiling use-cases.
    # The following section is configured with the profiling use-case in mind.
    if extra_jvm_args_target:
        if len(extra_jvm_args_target) == 1:
            # If there's only one target to profile, we want to isolate its compilation.
            # This target should be built in its own action, allowing the worker (if available) to handle the remaining targets.
            if qualified_name == qualified_name_with_subtarget(extra_jvm_args_target[0]):
                jvm_args = jvm_args + extra_jvm_args
                local_only = True  # This flag ensures the target is not run on the worker.
        else:
            # If there are multiple targets to profile, they should be built on the worker to generate a single profiling data set.
            # The remaining targets should be built individually, either locally or on the Remote Execution (RE).
            local_only = True  # By default, targets are not run on the worker.
            for target in extra_jvm_args_target:
                # If the current target matches the qualified name with subtarget, it is selected for profiling.
                if qualified_name == qualified_name_with_subtarget(target):
                    jvm_args = jvm_args + extra_jvm_args
                    local_only = False  # This flag allows the target to run on the worker.
                    break
    else:
        # If no specific target is provided, the extra JVM arguments are added to all targets that run on worker, local machine or RE.
        jvm_args = jvm_args + extra_jvm_args

    # Allow JVM compiler daemon to access internal jdk.compiler APIs
    jvm_args += [
        "--add-exports=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED",
        "--add-exports=jdk.compiler/com.sun.tools.javac.comp=ALL-UNNAMED",
        "--add-exports=jdk.compiler/com.sun.tools.javac.file=ALL-UNNAMED",
        "--add-exports=jdk.compiler/com.sun.tools.javac.jvm=ALL-UNNAMED",
        "--add-exports=jdk.compiler/com.sun.tools.javac.main=ALL-UNNAMED",
        "--add-exports=jdk.compiler/com.sun.tools.javac.model=ALL-UNNAMED",
        "--add-exports=jdk.compiler/com.sun.tools.javac.processing=ALL-UNNAMED",
        "--add-exports=jdk.compiler/com.sun.tools.javac.parser=ALL-UNNAMED",
        "--add-exports=jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED",
        "--add-opens=jdk.compiler/com.sun.tools.javac.api=ALL-UNNAMED",
        "--add-opens=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED",
        "--add-opens=jdk.compiler/com.sun.tools.javac.jvm=ALL-UNNAMED",
        "--add-opens=jdk.compiler/com.sun.tools.javac.processing=ALL-UNNAMED",
        "--add-opens=jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED",
    ]

    if target_specified_debug_port:
        debug_port = target_specified_debug_port
    elif toolchain_specified_debug_port and qualified_name == qualified_name_with_subtarget(toolchain_specified_debug_target):
        debug_port = toolchain_specified_debug_port
    else:
        debug_port = None

    if debug_port:
        # Do not use a worker when debugging is enabled
        local_only = True
        jvm_args.extend(["-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address={}".format(debug_port)])

    non_worker_args = cmd_args([java, jvm_args, "-cp", compiler, "-jar", class_loader_bootstrapper, main_class])

    if local_only:
        return RunInfo(args = non_worker_args), True
    else:
        worker_run_info = WorkerRunInfo(
            # Specifies the command to compile using a non-worker process, on RE or if workers are disabled
            exe = non_worker_args,
            # Specifies the command to initialize a new worker process.
            # This is used for local execution if `build.use_persistent_workers=True`
            worker = worker,
        )
        return worker_run_info, FORCE_PERSISTENT_WORKERS

FinalJarOutput = record(
    final_jar = Artifact,
    # The same as final_jar unless there is a jar_postprocessor.
    preprocessed_jar = Artifact,
)

# If there's additional compiled srcs, we need to merge them in and if the
# caller specified an output artifact we need to make sure the jar is in that
# location.
def prepare_final_jar(
        actions: AnalysisActions,
        actions_identifier: [str, None],
        output: Artifact | None,
        output_paths: OutputPaths,
        additional_compiled_srcs: Artifact | None,
        jar_builder: RunInfo,
        jar_postprocessor: [RunInfo, None],
        jar_postprocessor_runner: RunInfo,
        zip_scrubber: RunInfo) -> FinalJarOutput:
    def make_output(jar: Artifact) -> FinalJarOutput:
        if jar_postprocessor:
            postprocessed_jar = postprocess_jar(actions, zip_scrubber, jar_postprocessor, jar_postprocessor_runner, jar, actions_identifier)
            return FinalJarOutput(final_jar = postprocessed_jar, preprocessed_jar = jar)
        else:
            return FinalJarOutput(final_jar = jar, preprocessed_jar = jar)

    if not additional_compiled_srcs:
        output_jar = output_paths.jar
        if output:
            actions.copy_file(output.as_output(), output_paths.jar)
            output_jar = output

        return make_output(output_jar)

    merged_jar = output
    if not merged_jar:
        merged_jar = declare_prefixed_output(actions, actions_identifier, "merged.jar")
    files_to_merge = [output_paths.jar, additional_compiled_srcs]
    files_to_merge_file = actions.write(declare_prefixed_name("files_to_merge.txt", actions_identifier), files_to_merge)
    actions.run(
        cmd_args([
            jar_builder,
            "--output",
            merged_jar.as_output(),
            "--entries-to-jar",
            files_to_merge_file,
        ], hidden = files_to_merge),
        category = "merge_additional_srcs",
        identifier = actions_identifier,
    )

    return make_output(merged_jar)

def encode_command(
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
        abi_generation_mode: AbiGenerationMode,
        resources_map: dict[str, Artifact],
        extra_arguments: cmd_args,
        kotlin_extra_params: [struct, None],
        provide_classpath_snapshot: bool,
        build_mode: BuildMode,
        target_type: TargetType,
        output_paths: OutputPaths,
        classpath_jars_tag: ArtifactTag,
        source_only_abi_compiling_deps: list[JavaClasspathEntry],
        track_class_usage: bool) -> struct:
    base_jar_command = encode_base_jar_command(
        javac_tool,
        target_type,
        output_paths,
        remove_classes,
        label,
        compiling_deps_tset,
        classpath_jars_tag,
        bootclasspath_entries,
        source_level,
        target_level,
        abi_generation_mode,
        srcs,
        resources_map,
        annotation_processor_properties = annotation_processor_properties,
        plugin_params = plugin_params,
        manifest_file = manifest_file,
        extra_arguments = cmd_args(extra_arguments),
        source_only_abi_compiling_deps = source_only_abi_compiling_deps,
        track_class_usage = track_class_usage,
        provide_classpath_snapshot = provide_classpath_snapshot,
    )

    if kotlin_extra_params:
        return struct(
            buildMode = build_mode,
            baseJarCommand = base_jar_command,
            kotlinExtraParams = kotlin_extra_params,
        )
    else:
        return struct(
            buildMode = build_mode,
            baseJarCommand = base_jar_command,
        )

# buildifier: disable=unused-variable
def generate_abi_jars(
        actions: AnalysisActions,
        actions_identifier: [str, None],
        label: Label,
        abi_generation_mode: [AbiGenerationMode, None],
        additional_compiled_srcs: Artifact | None,
        is_building_android_binary: bool,
        class_abi_generator: Dependency,
        final_jar: Artifact,
        compiling_deps_tset: [JavaCompilingDepsTSet, None],
        source_only_abi_deps: list[Dependency],
        class_abi_jar: Artifact | None,
        class_abi_output_dir: Artifact | None,
        track_class_usage: bool,
        encode_abi_command: typing.Callable,
        define_action: typing.Callable) -> tuple:
    class_abi = None
    source_abi = None
    source_only_abi = None
    classpath_abi = None
    classpath_abi_dir = None

    # If we are merging additional compiled_srcs, we can't produce source/source-only abis. Otherwise we
    # always generation the source/source-only abis and setup the classpath entry to use the appropriate
    # abi. This allows us to build/inspect/debug source/source-only abi for rules that don't have it enabled.
    if not additional_compiled_srcs:
        if abi_generation_mode == AbiGenerationMode("source") or not is_building_android_binary:
            source_abi_identifier = declare_prefixed_name("source_abi", actions_identifier)
            source_abi_target_type = TargetType("source_abi")
            source_abi_qualified_name = get_qualified_name(label, source_abi_target_type)
            source_abi_output_paths = define_output_paths(actions, source_abi_identifier, label)
            source_abi_classpath_jars_tag = actions.artifact_tag()
            source_abi_dir = declare_prefixed_output(actions, source_abi_identifier, "source-abi-dir", dir = True)
            source_abi_command = encode_abi_command(
                build_mode = BuildMode("ABI"),
                target_type = source_abi_target_type,
                output_paths = source_abi_output_paths,
                classpath_jars_tag = source_abi_classpath_jars_tag,
                source_only_abi_compiling_deps = [],
                track_class_usage = track_class_usage,
            )
            define_action(
                "source_abi_",
                source_abi_identifier,
                source_abi_command,
                source_abi_qualified_name,
                source_abi_output_paths,
                source_abi_classpath_jars_tag,
                source_abi_dir,
                source_abi_target_type,
            )
            source_abi = source_abi_output_paths.jar

            if abi_generation_mode == AbiGenerationMode("source"):
                classpath_abi = source_abi
                classpath_abi_dir = source_abi_dir

        if abi_generation_mode == AbiGenerationMode("source_only") or not is_building_android_binary:
            source_only_abi_identifier = declare_prefixed_name("source_only_abi", actions_identifier)
            source_only_abi_target_type = TargetType("source_only_abi")
            source_only_abi_qualified_name = get_qualified_name(label, source_only_abi_target_type)
            source_only_abi_output_paths = define_output_paths(actions, source_only_abi_identifier, label)
            source_only_abi_classpath_jars_tag = actions.artifact_tag()
            source_only_abi_dir = declare_prefixed_output(actions, source_only_abi_identifier, "dir", dir = True)
            source_only_abi_compiling_deps = _get_source_only_abi_compiling_deps(compiling_deps_tset, source_only_abi_deps)
            source_only_abi_command = encode_abi_command(
                build_mode = BuildMode("ABI"),
                target_type = source_only_abi_target_type,
                output_paths = source_only_abi_output_paths,
                classpath_jars_tag = source_only_abi_classpath_jars_tag,
                source_only_abi_compiling_deps = source_only_abi_compiling_deps,
                track_class_usage = track_class_usage,
            )
            define_action(
                "source_only_abi_",
                source_only_abi_identifier,
                source_only_abi_command,
                source_only_abi_qualified_name,
                source_only_abi_output_paths,
                source_only_abi_classpath_jars_tag,
                source_only_abi_dir,
                source_only_abi_target_type,
                source_only_abi_compiling_deps = source_only_abi_compiling_deps,
            )
            source_only_abi = source_only_abi_output_paths.jar

            if abi_generation_mode == AbiGenerationMode("source_only"):
                classpath_abi = source_only_abi
                classpath_abi_dir = source_only_abi_dir

        if abi_generation_mode == AbiGenerationMode("none"):
            classpath_abi = final_jar

    if classpath_abi == None or not is_building_android_binary:
        class_abi = class_abi_jar or create_abi(actions, class_abi_generator, final_jar)
        if classpath_abi == None:
            classpath_abi = class_abi
            if class_abi_output_dir:
                classpath_abi_dir = class_abi_output_dir

    return class_abi, source_abi, source_only_abi, classpath_abi, classpath_abi_dir

def postprocess_jar(
        actions: AnalysisActions,
        zip_scrubber: RunInfo,
        jar_postprocessor: RunInfo,
        jar_postprocessor_runner: RunInfo,
        original_jar: Artifact,
        actions_identifier: [str, None]) -> Artifact:
    jar_path = original_jar.short_path
    postprocessed_output = actions.declare_output("postprocessed_{}".format(jar_path))

    postprocess_jar_cmd = cmd_args(
        jar_postprocessor_runner,
        "--postprocessor_cmd",
        cmd_args([
            jar_postprocessor,
            original_jar,
            postprocessed_output.as_output(),
        ], delimiter = " "),
        "--zip_scrubber",
        cmd_args(zip_scrubber, delimiter = " "),
        "--output",
        postprocessed_output.as_output(),
    )

    identifier = actions_identifier if actions_identifier else ""
    actions.run(postprocess_jar_cmd, category = "postprocessed{}".format(identifier))

    return postprocessed_output
