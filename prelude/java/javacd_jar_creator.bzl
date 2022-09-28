load(
    "@prelude//java:java_providers.bzl",
    "JavaClasspathEntry",
    "JavaCompilingDepsTSet",
    "JavaLibraryInfo",
    "create_abi",
    "derive_compiling_deps",
    "make_compile_outputs",
)
load("@prelude//java:java_resources.bzl", "get_resources_map")
load("@prelude//java:java_toolchain.bzl", "AbiGenerationMode")

def create_jar_artifact_javacd(
        actions: "actions",
        actions_prefix: str.type,
        abi_generation_mode: [AbiGenerationMode.type, None],
        java_toolchain: "JavaToolchainInfo",
        label,
        output: ["artifact", None],
        javac_tool: ["", None],
        srcs: ["artifact"],
        remove_classes: [str.type],
        resources: ["artifact"],
        resources_root: [str.type, None],
        manifest_file: ["artifact", None],
        ap_params: ["AnnotationProcessorParams"],
        plugin_params: ["PluginParams", None],
        source_level: int.type,
        target_level: int.type,
        deps: ["dependency"],
        required_for_source_only_abi: bool.type,
        source_only_abi_deps: ["dependency"],
        extra_arguments: ["string"],
        additional_classpath_entries: ["artifact"],
        additional_compiled_srcs: ["artifact", None],
        bootclasspath_entries: ["artifact"]) -> "JavaCompileOutputs":
    if javac_tool != None:
        # TODO(cjhopman): We can probably handle this better. I think we should be able to just use the non-javacd path.
        fail("cannot set explicit javac on library when using javacd")

    resources_map = get_resources_map(java_toolchain, label.package, resources, resources_root)

    # TODO(cjhopman): Handle manifest file.
    _ = manifest_file

    if target_level == 7:
        bootclasspath_entries = bootclasspath_entries + java_toolchain.bootclasspath_7
    if target_level == 8:
        bootclasspath_entries = bootclasspath_entries + java_toolchain.bootclasspath_8

    def declare_prefixed_output(prefix, output):
        return actions.declare_output("{}{}".format(prefix, output))

    # The library and the toolchain can both set a specific abi generation
    # mode. The toolchain's setting is effectively the "highest" form of abi
    # that the toolchain supports and then the same for the target and we will choose
    # the "highest" that both support.
    def resolve_abi_generation_mode(abi_generation_mode, java_toolchain):
        if abi_generation_mode == None:
            return java_toolchain.abi_generation_mode
        for mode in [AbiGenerationMode("class"), AbiGenerationMode("source"), AbiGenerationMode("source_only")]:
            if mode in (java_toolchain.abi_generation_mode, abi_generation_mode):
                return mode
        fail("resolving abi generation mode failed. had `{}` and `{}`".format(java_toolchain.abi_generation_mode, abi_generation_mode))

    abi_generation_mode = resolve_abi_generation_mode(abi_generation_mode, java_toolchain)
    if not srcs:
        abi_generation_mode = AbiGenerationMode("class")

    if abi_generation_mode == AbiGenerationMode("source_only"):
        def plugins_support_source_only_abi():
            for ap in ap_params:
                if ap.affects_abi and not ap.supports_source_only_abi:
                    return False
            return True

        if not plugins_support_source_only_abi():
            abi_generation_mode = AbiGenerationMode("source")

    # We need to construct a complex protobuf message. We do it by constructing
    # a bunch of nested structs and then use write_json to get a json-encoded
    # protobuf message.
    #
    # The definition is in xplat/build_infra/buck_client/src/com/facebook/buck/cd/resources/proto/javacd.proto
    # and is, sadly, poorly documented.
    #
    # As we are generally trying to match buck1 for now, you can get buck1 to dump the protos for a build
    # by running `export JAVACD_DUMP_PROTOS=1; buck build foo -c javacd.pass_env_variables_to_javacd=true`

    # Our protobuf format mostly encodes paths in RelPath/AbsPath structs with a single "path" field.
    # Note that we don't actually use abspath and instead enable JAVACD_ABSOLUTE_PATHS_ARE_RELATIVE_TO_CWD
    def encode_path(path):
        return struct(path = path)

    # now prefix is just used for categories and prefixes of path segments, so we want it either non-empty or w/ a trailing underscore
    if actions_prefix:
        actions_prefix += "_"

    TargetType = enum("library", "source_abi", "source_only_abi")

    def encode_abi_generation_mode(mode: AbiGenerationMode.type) -> str.type:
        return {
            AbiGenerationMode("class"): "CLASS",
            AbiGenerationMode("source"): "SOURCE",
            AbiGenerationMode("source_only"): "SOURCE_ONLY",
        }[mode]

    def encode_target_type(target_type: TargetType.type) -> str.type:
        if target_type == TargetType("library"):
            return "LIBRARY"
        if target_type == TargetType("source_abi"):
            return "SOURCE_ABI"
        if target_type == TargetType("source_only_abi"):
            return "SOURCE_ONLY_ABI"
        fail()

    OutputPaths = record(
        jar_parent = "artifact",
        jar = "artifact",
        classes = "artifact",
        annotations = "artifact",
        scratch = "artifact",
    )

    # Converted to str so that we get the right result when written as json.
    base_qualified_name = "{}:{}".format(label.path, label.name)

    def get_qualified_name(target_type: TargetType.type) -> str.type:
        # These should match the names for subtargets in java_library.bzl
        return {
            TargetType("library"): base_qualified_name,
            TargetType("source_abi"): base_qualified_name + "[source-abi]",
            TargetType("source_only_abi"): base_qualified_name + "[source-only-abi]",
        }[target_type]

    def define_output_paths(prefix: str.type) -> OutputPaths.type:
        # currently, javacd requires that at least some outputs are in the root
        # output dir. so we put all of them there. If javacd is updated we
        # could consolidate some of these into one subdir.
        jar_parent = declare_prefixed_output(prefix, "jar")
        return OutputPaths(
            jar_parent = jar_parent,
            jar = jar_parent.project("lib.jar"),
            classes = declare_prefixed_output(prefix, "__classes__"),
            annotations = declare_prefixed_output(prefix, "__gen__"),
            scratch = declare_prefixed_output(prefix, "scratch"),
        )

    def encode_output_paths(paths: OutputPaths.type, target_type: TargetType.type):
        paths = struct(
            classesDir = encode_path(paths.classes.as_output()),
            outputJarDirPath = encode_path(paths.jar_parent.as_output()),
            annotationPath = encode_path(paths.annotations.as_output()),
            pathToSourcesList = encode_path(cmd_args([paths.scratch.as_output(), "/", "__srcs__"], delimiter = "")),
            workingDirectory = encode_path(paths.scratch.as_output()),
            outputJarPath = encode_path(paths.jar.as_output()),
        )

        return struct(
            libraryPaths = paths if target_type == TargetType("library") else None,
            sourceAbiPaths = paths if target_type == TargetType("source_abi") else None,
            sourceOnlyAbiPaths = paths if target_type == TargetType("source_only_abi") else None,
            libraryTargetFullyQualifiedName = base_qualified_name,
        )

    def encode_jar_params(output_paths: OutputPaths.type) -> struct.type:
        return struct(
            jarPath = encode_path(output_paths.jar.as_output()),
            removeEntryPredicate = struct(
                patterns = remove_classes,
            ),
            entriesToJar = [
                encode_path(output_paths.classes.as_output()),
            ],
            # TODO(cjhopman): Get correct duplicatesLogLevel
            duplicatesLogLevel = "INFO",
        )

    def encode_base_jar_command(target_type: TargetType.type, output_paths: OutputPaths.type):
        # We want the library target to have the real generation mode, but the source one's just use their own.
        # The generation mode will be used elsewhere to setup the target's classpath entry to use the correct abi.
        command_abi_generation_mode = abi_generation_mode
        if target_type == TargetType("source_abi"):
            command_abi_generation_mode = AbiGenerationMode("source")
        if target_type == TargetType("source_only_abi"):
            command_abi_generation_mode = AbiGenerationMode("source_only")

        # TODO(cjhopman): Get correct output root (and figure out whether its even actually necessary).
        # TODO(cjhopman): Get correct ignore paths.
        filesystem_params = struct(
            # For buck2, everything is relative to the project root.
            rootPath = encode_path(""),
            configuredBuckOut = encode_path("buck-out/v2"),
            globIgnorePaths = [],
        )

        library_jar_params = encode_jar_params(output_paths)

        qualified_name = get_qualified_name(target_type)

        compiling_deps_tset = derive_compiling_deps(actions, None, deps)
        if additional_classpath_entries:
            children = [compiling_deps_tset] if compiling_deps_tset else []
            for entry in additional_classpath_entries:
                children.append(actions.tset(JavaCompilingDepsTSet, value = JavaClasspathEntry(
                    full_library = entry,
                    abi = entry,
                    required_for_source_only_abi = True,
                )))
            compiling_deps_tset = actions.tset(JavaCompilingDepsTSet, children = children)

        compiling_classpath = []
        if compiling_deps_tset:
            if target_type == TargetType("source_only_abi"):
                source_only_abi_deps_filter = {}
                for d in source_only_abi_deps:
                    info = d.get(JavaLibraryInfo)
                    if not info:
                        fail("source_only_abi_deps must produce a JavaLibraryInfo but {} does not, please remove it".format(d))
                    if info.library_output:
                        source_only_abi_deps_filter[info.library_output] = True

                def filter_compiling_deps(dep):
                    return dep in source_only_abi_deps_filter or dep.required_for_source_only_abi

                compiling_classpath = [encode_path(compiling_dep.abi) for compiling_dep in list(compiling_deps_tset.traverse()) if filter_compiling_deps(compiling_dep)]
            else:
                compiling_classpath = compiling_deps_tset.project_as_json("javacd_json")

        # buck1 oddly only inspects annotation processors, not plugins for
        # abi/source-only abi related things, even though the plugin rules
        # support the flags. we apply it to both.
        encoded_ap_params = None
        if ap_params:
            encoded_ap_params = struct(
                parameters = [],
                pluginProperties = [],
            )
            for ap in ap_params:
                # We should also filter out non-abi-affecting APs for source-abi, but buck1 doesn't and so we have lots that depend on not filtering them there.
                if target_type == TargetType("source_only_abi") and not ap.affects_abi:
                    continue

                parameters = encoded_ap_params.parameters
                parameters += ap.params
                encoded_ap_params.pluginProperties.append(
                    struct(
                        canReuseClassLoader = False,
                        doesNotAffectAbi = not ap.affects_abi,
                        supportsAbiGenerationFromSource = ap.supports_source_only_abi,
                        processorNames = ap.processors,
                        classpath = ap.deps.project_as_json("javacd_json") if ap.deps else [],
                        pathParams = {},
                    ),
                )

        # TODO(cjhopman): We should change plugins to not be merged together just like APs.
        encoded_plugin_params = None
        if plugin_params:
            encoded_plugin_params = struct(
                parameters = [],
                pluginProperties = [struct(
                    canReuseClassLoader = False,
                    doesNotAffectAbi = False,
                    supportsAbiGenerationFromSource = False,
                    processorNames = plugin_params.processors,
                    classpath = plugin_params.deps.project_as_json("javacd_json") if plugin_params.deps else [],
                    pathParams = {},
                )],
            )

        return struct(
            outputPathsValue = encode_output_paths(output_paths, target_type),
            compileTimeClasspathPaths = compiling_classpath,
            javaSrcs = [encode_path(s) for s in srcs],
            # TODO(cjhopman): populate jar infos. I think these are only used for unused dependencies (and appear to be broken in buck1 w/javacd anyway).
            fullJarInfos = [],
            abiJarInfos = [],
            # We use "class" abi compatibility to match buck1 (other compatibility modes are used for abi verification.
            abiCompatibilityMode = encode_abi_generation_mode(AbiGenerationMode("class")),
            abiGenerationMode = encode_abi_generation_mode(command_abi_generation_mode),
            trackClassUsage = True,
            filesystemParams = filesystem_params,
            buildTargetValue = struct(
                fullyQualifiedName = qualified_name,
                type = encode_target_type(target_type),
            ),
            # TODO(cjhopman): Populate this or remove it.
            cellToPathMappings = {},
            resourcesMap = [
                {
                    "key": encode_path(v),
                    "value": encode_path(cmd_args([output_paths.classes.as_output(), "/", k], delimiter = "")),
                }
                for (k, v) in resources_map.items()
            ],
            resolvedJavac = {"jsr199Javac": {}},
            resolvedJavacOptions = struct(
                bootclasspathList = [
                    encode_path(v)
                    for v in bootclasspath_entries
                ],
                languageLevelOptions = struct(
                    sourceLevel = source_level,
                    targetLevel = target_level,
                ),
                debug = True,
                javaAnnotationProcessorParams = encoded_ap_params,
                standardJavacPluginParams = encoded_plugin_params,
                extraArguments = extra_arguments,
            ),
            libraryJarParameters = library_jar_params,
        )

    def encode_library_command(output_paths: OutputPaths.type, path_to_class_hashes: "artifact") -> struct.type:
        target_type = TargetType("library")

        base_jar_command = encode_base_jar_command(target_type, output_paths)

        library_jar_base_command = struct(
            pathToClasses = encode_path(output_paths.jar.as_output()),
            rootOutput = encode_path(output_paths.jar_parent.as_output()),
            pathToClassHashes = encode_path(path_to_class_hashes.as_output()),
            annotationsPath = encode_path(output_paths.annotations.as_output()),
        )

        library_jar_command = struct(
            baseJarCommand = base_jar_command,
            libraryJarBaseCommand = library_jar_base_command,
        )

        return struct(
            baseCommandParams = struct(
                withDownwardApi = True,
                spoolMode = "DIRECT_TO_JAR",
            ),
            libraryJarCommand = library_jar_command,
        )

    def encode_abi_command(output_paths: OutputPaths.type, target_type: TargetType.type) -> struct.type:
        base_jar_command = encode_base_jar_command(target_type, output_paths)

        abi_params = encode_jar_params(output_paths)

        abi_command = struct(
            baseJarCommand = base_jar_command,
            abiJarParameters = abi_params,
        )

        return struct(
            baseCommandParams = struct(
                withDownwardApi = True,
                spoolMode = "DIRECT_TO_JAR",
            ),
            abiJarCommand = abi_command,
        )

    def define_javacd_action(actions_prefix: str.type, encoded_command: struct.type, qualified_name: str.type):
        proto = declare_prefixed_output(actions_prefix, "jar_command.proto.json")

        proto_with_inputs = actions.write_json(proto, encoded_command, with_inputs = True)

        cmd = cmd_args([
            java_toolchain.javac,
            "--action-id",
            qualified_name,
            "--command-file",
            proto_with_inputs,
        ])
        cmd.hidden(proto_with_inputs)

        # TODO(cjhopman): make sure this works both locally and remote.
        event_pipe_out = declare_prefixed_output(actions_prefix, "events.data")

        actions.run(
            cmd,
            env = {
                "BUCK_EVENT_PIPE": event_pipe_out.as_output(),
                "JAVACD_ABSOLUTE_PATHS_ARE_RELATIVE_TO_CWD": "1",
            },
            category = "{}javacd_jar".format(actions_prefix),
        )

    output_paths = define_output_paths(actions_prefix)
    path_to_class_hashes_out = declare_prefixed_output(actions_prefix, "classes.txt")
    command = encode_library_command(output_paths, path_to_class_hashes_out)
    define_javacd_action(actions_prefix, command, base_qualified_name)

    # If there's additional compiled srcs, we need to merge them in and if the
    # caller specified an output artifact we need to make sure the jar is in that
    # location.
    def prepare_final_jar():
        if not additional_compiled_srcs:
            if output:
                actions.copy_file(output.as_output(), output_paths.jar)
                return output
            return output_paths.jar

        merged_jar = output
        if not merged_jar:
            merged_jar = declare_prefixed_output(actions_prefix, "merged.jar")
        actions.run(
            [
                java_toolchain.merge_to_jar[RunInfo],
                "--jar_tool",
                java_toolchain.jar,
                "--output",
                merged_jar.as_output(),
                "--jar",
                output_paths.jar,
                "--extra-files",
                additional_compiled_srcs,
            ],
            category = "{}merge_additional_srcs".format(actions_prefix),
        )
        return merged_jar

    final_jar = prepare_final_jar()
    class_abi = None if java_toolchain.is_bootstrap_toolchain else create_abi(actions, java_toolchain.class_abi_generator, final_jar)

    source_abi = None
    source_only_abi = None

    classpath_abi = class_abi

    # If we are merging additional compiled_srcs, we can't produce source/source-only abis. Otherwise we
    # always generation the source/source-only abis and setup the classpath entry to use the appropriate
    # abi. This allows us to build/inspect/debug source/source-only abi for rules that don't have it enabled.
    if not additional_compiled_srcs:
        source_abi_prefix = "{}source_abi_".format(actions_prefix)
        source_abi_target_type = TargetType("source_abi")
        source_abi_qualified_name = get_qualified_name(source_abi_target_type)
        source_abi_output_paths = define_output_paths(source_abi_prefix)
        source_abi_command = encode_abi_command(source_abi_output_paths, source_abi_target_type)
        define_javacd_action(source_abi_prefix, source_abi_command, source_abi_qualified_name)

        source_only_abi_prefix = "{}source_only_abi_".format(actions_prefix)
        source_only_abi_target_type = TargetType("source_only_abi")
        source_only_abi_qualified_name = get_qualified_name(source_only_abi_target_type)
        source_only_abi_output_paths = define_output_paths(source_only_abi_prefix)
        source_only_abi_command = encode_abi_command(source_only_abi_output_paths, source_only_abi_target_type)
        define_javacd_action(source_only_abi_prefix, source_only_abi_command, source_only_abi_qualified_name)

        source_abi = source_abi_output_paths.jar
        source_only_abi = source_only_abi_output_paths.jar

        if abi_generation_mode == AbiGenerationMode("source"):
            classpath_abi = source_abi
        elif abi_generation_mode == AbiGenerationMode("source_only"):
            classpath_abi = source_only_abi

    result = make_compile_outputs(
        full_library = final_jar,
        class_abi = class_abi,
        source_abi = source_abi,
        source_only_abi = source_only_abi,
        classpath_abi = classpath_abi,
        required_for_source_only_abi = required_for_source_only_abi,
    )
    return result
