load("@fbcode//buck2/prelude:paths.bzl", "paths")
load(
    "@fbcode//buck2/prelude/java:java_providers.bzl",
    "derive_compiling_deps",
    "make_compile_outputs",
    "maybe_create_abi",
)
load("@fbcode//buck2/prelude/java:java_resources.bzl", "get_resources_map")

def create_jar_artifact_javacd(
        actions: "actions",
        actions_prefix: str.type,
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

    # We need to construct a complex protobuf message. We do it by constructing
    # a bunch of nested structs and then use write_json to get a json-encoded
    # protobuf message.
    #
    # The definition is in xplat/build_infra/buck_client/src/com/facebook/buck/cd/resources/proto/javacd.proto
    # and is, sadly, poorly documented.
    #
    # As we are generally trying to match buck1 for now, you can get buck1 to dump the protos for a build
    # by running `export JAVACD_DUMP_PROTOS=1; buck build foo -c javacd.path_env_variables_to_javacd=true`

    # Our protobuf format mostly encodes paths in RelPath/AbsPath structs with a single "path" field.
    # Note that we don't actually use abspath and instead enable JAVACD_ABSOLUTE_PATHS_ARE_RELATIVE_TO_CWD
    def encode_path(path):
        return struct(path = path)

    # now prefix is just used for categories and prefixes of path segments, so we want it either non-empty or w/ a trailing underscore
    if actions_prefix:
        actions_prefix += "_"

    OutputPaths = record(
        jar_parent = "artifact",
        jar = "artifact",
        classes = "artifact",
        annotations = "artifact",
        scratch = "artifact",
    )

    base_qualified_name = "{}:{}".format(label.path, label.name)  # Converted to str so that we get the right result when written as json.

    def define_output_paths(prefix: str.type) -> OutputPaths.type:
        # currently, javacd requires that at least some outputs are in the root
        # output dir. so we put all of them there. If javacd is updated we
        # could consolidate some of these into one subdir.
        return OutputPaths(
            jar_parent = declare_prefixed_output(prefix, "jar"),
            jar = declare_prefixed_output(prefix, paths.join("jar", "lib.jar")),
            classes = declare_prefixed_output(prefix, "__classes__"),
            annotations = declare_prefixed_output(prefix, "__gen__"),
            scratch = declare_prefixed_output(prefix, "scratch"),
        )

    def encode_output_paths(paths: OutputPaths.type):
        paths = struct(
            classesDir = encode_path(paths.classes.as_output()),
            outputJarDirPath = encode_path(paths.jar_parent.as_output()),
            annotationPath = encode_path(paths.annotations.as_output()),
            pathToSourcesList = encode_path(cmd_args([paths.scratch.as_output(), "/", "__srcs__"], delimiter = "")),
            workingDirectory = encode_path(paths.scratch.as_output()),
            outputJarPath = encode_path(paths.jar.as_output()),
        )

        return struct(
            libraryPaths = paths,
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

    def encode_base_jar_command(output_paths: OutputPaths.type):
        # TODO(cjhopman): Get correct output root (and figure out whether its even actually necessary).
        # TODO(cjhopman): Get correct ignore paths.
        filesystem_params = struct(
            # For buck2, everything is relative to the project root.
            rootPath = encode_path(""),
            configuredBuckOut = encode_path("buck-out/v2"),
            globIgnorePaths = [],
        )

        library_jar_params = encode_jar_params(output_paths)

        compiling_classpath = [] + additional_classpath_entries
        compiling_deps_tset = derive_compiling_deps(actions, None, deps)
        if compiling_deps_tset:
            compiling_classpath.extend(
                [compiling_dep.abi for compiling_dep in list(compiling_deps_tset.traverse())],
            )

        encoded_ap_params = None
        if ap_params:
            encoded_ap_params = struct(
                parameters = [],
                pluginProperties = [],
            )
            for ap in ap_params:
                parameters = encoded_ap_params.parameters
                parameters += ap.params
                encoded_ap_params.pluginProperties.append(
                    struct(
                        canReuseClassLoader = False,
                        processorNames = ap.processors,
                        classpath = [encode_path(v) for v in ap.deps],
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
                    processorNames = plugin_params.processors,
                    classpath = [encode_path(v) for v in plugin_params.deps],
                    pathParams = {},
                )],
            )

        return struct(
            outputPathsValue = encode_output_paths(output_paths),
            compileTimeClasspathPaths = [encode_path(p) for p in compiling_classpath],
            javaSrcs = [encode_path(s) for s in srcs],
            # TODO(cjhopman): populate jar infos. I think these are only used for unused dependencies (and appear to be broken in buck1 w/javacd anyway).
            fullJarInfos = [],
            abiJarInfos = [],
            abiCompatibilityMode = "CLASS",
            abiGenerationMode = "CLASS",
            trackClassUsage = True,
            filesystemParams = filesystem_params,
            buildTargetValue = struct(
                fullyQualifiedName = base_qualified_name,
                type = "LIBRARY",
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
        base_jar_command = encode_base_jar_command(output_paths)

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

    # write_json doesn't support struct yet, so we need to traverse the value
    # to convert any structs to dicts ourselves. In addition, we need to extract
    # the referenced artifacts so that we can add them to the consumer's .hidden().
    # TODO(cjhopman): Remove this when write_json supports struct
    def process_for_write_json(val, consume_inputs):
        def process(val, consumer):
            if type(val) == type([]):
                return [process(v, consumer) for v in val]
            if type(val) == type({}):
                return {k: process(v, consumer) for (k, v) in val.items()}
            if type(val) == type(struct()):
                return {
                    attr: process(getattr(val, attr), consumer)
                    for attr in dir(val)
                    if attr != "to_json"
                }
            if type(val) in ("artifact", "output_artifact", "cmd_args"):
                consume_inputs(val)
            return val

        return process(val, consume_inputs)

    def define_javacd_action(actions_prefix: str.type, encoded_command: struct.type, qualified_name: str.type):
        proto = declare_prefixed_output(actions_prefix, "jar_command.proto.json")

        hidden = []

        def add_hidden(v):
            hidden.append(v)

        command = process_for_write_json(encoded_command, add_hidden)

        actions.write_json(proto, command)

        cmd = cmd_args([
            java_toolchain.javac,
            "--action-id",
            qualified_name,
            "--command-file",
            proto,
        ])

        # TODO(cjhopman): make sure this works both locally and remote.
        event_pipe_out = declare_prefixed_output(actions_prefix, "events.data")

        cmd.hidden(
            hidden,
        )

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
                actions.copy(output_paths.jar, output.as_output())
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
    class_abi = maybe_create_abi(actions, java_toolchain, final_jar)
    classpath_abi = class_abi

    result = make_compile_outputs(
        full_library = final_jar,
        class_abi = class_abi,
        classpath_abi = classpath_abi,
    )
    return result
