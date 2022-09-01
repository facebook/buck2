load("@prelude//:paths.bzl", "paths")
load("@prelude//android:android_providers.bzl", "merge_android_packageable_info")
load(
    "@prelude//java:java_providers.bzl",
    "JavaLibraryInfo",
    "JavaProviders",
    "create_abi",
    "create_java_library_providers",
    "derive_compiling_deps",
    "make_compile_outputs",
    "to_list",
)
load("@prelude//java:java_resources.bzl", "get_resources_map")
load("@prelude//java:java_toolchain.bzl", "AbiGenerationMode", "JavaToolchainInfo")
load("@prelude//java:javacd_jar_creator.bzl", "create_jar_artifact_javacd")
load("@prelude//java/plugins:java_annotation_processor.bzl", "create_ap_params")
load("@prelude//java/plugins:java_plugin.bzl", "PluginParams", "create_plugin_params")
load("@prelude//java/utils:java_utils.bzl", "derive_javac", "get_path_separator")
load("@prelude//linking:shared_libraries.bzl", "SharedLibraryInfo")
load("@prelude//utils:utils.bzl", "expect")

_JAVA_FILE_EXTENSION = [".java"]
_SUPPORTED_ARCHIVE_SUFFIXES = [".src.zip", "-sources.jar"]

def _get_java_version_attributes(ctx: "context") -> (int.type, int.type):
    java_toolchain = ctx.attrs._java_toolchain[JavaToolchainInfo]
    java_version = ctx.attrs.java_version
    java_source = ctx.attrs.source
    java_target = ctx.attrs.target

    if java_version:
        if java_source or java_target:
            fail("No need to set 'source' and/or 'target' attributes when 'java_version' is present")
        java_version = _to_java_version(java_version)
        return (java_version, java_version)

    source = java_source or java_toolchain.source_level
    target = java_target or java_toolchain.target_level

    expect(bool(source) and bool(target), "Java source level and target level must be set!")

    source = _to_java_version(source)
    target = _to_java_version(target)

    expect(source <= target, "java library source level {} is higher than target {} ", source, target)

    return (source, target)

def _to_java_version(java_version: str.type) -> int.type:
    if java_version.startswith("1."):
        expect(len(java_version) == 3, "Supported java version number format is 1.X, where X is a single digit numnber, but it was set to {}", java_version)
        java_version_number = int(java_version[2:])
        expect(java_version_number < 9, "Supported java version number format is 1.X, where X is a single digit numnber that is less than 9, but it was set to {}", java_version)
        return java_version_number
    else:
        return int(java_version)

def _process_classpath(
        actions: "actions",
        classpath_args: "cmd_args",
        cmd: "cmd_args",
        args_file_name: "string",
        option_name: "string"):
    # write joined classpath string into args file
    classpath_args_file, classpath_macro_files = actions.write(
        args_file_name,
        classpath_args,
        allow_args = True,
    )

    # mark classpath artifacts as input
    cmd.hidden(classpath_args, classpath_macro_files)

    # add classpath args file to cmd
    cmd.add(option_name, classpath_args_file)

def classpath_args(args):
    return cmd_args(args, delimiter = get_path_separator())

def _process_plugins(
        actions: "actions",
        actions_prefix: str.type,
        ap_params: ["AnnotationProcessorParams"],
        plugin_params: ["PluginParams", None],
        javac_args: "cmd_args",
        cmd: "cmd_args"):
    processors_classpath = []

    # Process Annotation processors
    if ap_params:
        # For external javac, we can't preserve separate classpaths for separate processors. So we just concat everything.
        javac_args.add("-processor")
        joined_processors_string = ",".join([p for ap in ap_params for p in ap.processors])

        javac_args.add(joined_processors_string)

        for ap in ap_params:
            for param in ap.params:
                javac_args.add("-A{}".format(param))
            processors_classpath = processors_classpath + ap.deps

    else:
        javac_args.add("-proc:none")

    # Process Javac Plugins
    if plugin_params:
        plugin = plugin_params.processors[0]
        args = plugin_params.args.get(plugin, cmd_args())

        # Produces "-Xplugin:PluginName arg1 arg2 arg3", as a single argument
        plugin_and_args = cmd_args(plugin)
        plugin_and_args.add(args)
        plugin_arg = cmd_args(format = "-Xplugin:{}", quote = "shell")
        plugin_arg.add(cmd_args(plugin_and_args, delimiter = " "))

        javac_args.add(plugin_arg)
        processors_classpath = processors_classpath + plugin_params.deps

    if processors_classpath:
        processors_classpath = classpath_args(processors_classpath)
        _process_classpath(
            actions,
            processors_classpath,
            cmd,
            "{}plugin_cp_args".format(actions_prefix),
            "--javac_processors_classpath_file",
        )

def _build_classpath(actions: "actions", deps: ["dependency"], additional_classpath_entries: ["artifact"], classpath_args_projection: "string") -> ["cmd_args", None]:
    compiling_deps_tset = derive_compiling_deps(actions, None, deps)

    if additional_classpath_entries or compiling_deps_tset:
        args = cmd_args()
        if compiling_deps_tset:
            args.add(compiling_deps_tset.project_as_args(classpath_args_projection))
        args.add(additional_classpath_entries)
        return args

    return None

def _build_bootclasspath(bootclasspath_entries: ["artifact"], source_level: int.type, java_toolchain: "JavaToolchainInfo") -> ["artifact"]:
    bootclasspath_list = []
    if source_level in [7, 8]:
        if bootclasspath_entries:
            bootclasspath_list = bootclasspath_entries
        elif source_level == 7:
            bootclasspath_list = java_toolchain.bootclasspath_7
        elif source_level == 8:
            bootclasspath_list = java_toolchain.bootclasspath_8
    return bootclasspath_list

def _append_javac_params(
        actions: "actions",
        actions_prefix: str.type,
        java_toolchain: "JavaToolchainInfo",
        srcs: ["artifact"],
        remove_classes: [str.type],
        annotation_processor_params: ["AnnotationProcessorParams"],
        javac_plugin_params: ["PluginParams", None],
        source_level: int.type,
        target_level: int.type,
        deps: ["dependency"],
        extra_arguments: ["string"],
        additional_classpath_entries: ["artifact"],
        bootclasspath_entries: ["artifact"],
        cmd: "cmd_args"):
    javac_args = cmd_args(
        "-encoding",
        "utf-8",
        # Set the sourcepath to stop us reading source files out of jars by mistake.
        "-sourcepath",
        '""',
    )
    javac_args.add(*extra_arguments)

    # we want something that looks nice when prepended to another string, so add "_" to non-empty prefixes.
    if actions_prefix:
        actions_prefix += "_"

    compiling_classpath = _build_classpath(actions, deps, additional_classpath_entries, "args_for_compiling")
    if compiling_classpath:
        _process_classpath(
            actions,
            classpath_args(compiling_classpath),
            cmd,
            "{}classpath_args".format(actions_prefix),
            "--javac_classpath_file",
        )
    else:
        javac_args.add("-classpath ''")

    javac_args.add("-source")
    javac_args.add(str(source_level))
    javac_args.add("-target")
    javac_args.add(str(target_level))

    bootclasspath_list = _build_bootclasspath(bootclasspath_entries, source_level, java_toolchain)
    if bootclasspath_list:
        _process_classpath(
            actions,
            classpath_args(bootclasspath_list),
            cmd,
            "{}bootclasspath_args".format(actions_prefix),
            "--javac_bootclasspath_file",
        )

    _process_plugins(
        actions,
        actions_prefix,
        annotation_processor_params,
        javac_plugin_params,
        javac_args,
        cmd,
    )

    generated_sources_dir = actions.declare_output("{}generated_sources".format(actions_prefix))
    cmd.add("--generated_sources_dir", generated_sources_dir.as_output())

    zipped_sources, plain_sources = split_on_archives_and_plain_files(srcs, _JAVA_FILE_EXTENSION)

    javac_args.add(*plain_sources)
    args_file, macro_files = actions.write(
        "{}javac_args".format(actions_prefix),
        javac_args,
        allow_args = True,
    )
    cmd.hidden(javac_args)

    # mark plain srcs artifacts as input
    cmd.hidden(plain_sources, macro_files)

    cmd.add("--javac_args_file", args_file)

    if zipped_sources:
        cmd.add("--zipped_sources_file", actions.write("{}zipped_source_args".format(actions_prefix), zipped_sources))
        cmd.hidden(zipped_sources)

    if remove_classes:
        cmd.add("--remove_classes", actions.write("{}remove_classes_args".format(actions_prefix), remove_classes))

def split_on_archives_and_plain_files(
        srcs: ["artifact"],
        plain_file_extensions: [str.type]) -> (["artifact"], ["artifact"]):
    archives = []
    plain_sources = []

    for src in srcs:
        if src.extension in plain_file_extensions:
            plain_sources.append(src)
        elif _is_supported_archive(src):
            archives.append(src)
        else:
            fail("Provided java source is not supported: {}".format(src))

    return (archives, plain_sources)

def _is_supported_archive(src: "artifact") -> bool.type:
    basename = src.basename
    for supported_suffix in _SUPPORTED_ARCHIVE_SUFFIXES:
        if basename.endswith(supported_suffix):
            return True
    return False

def _copy_resources(
        actions: "actions",
        actions_prefix: str.type,
        java_toolchain: JavaToolchainInfo.type,
        package: str.type,
        resources: ["artifact"],
        resources_root: [str.type, None]) -> "artifact":
    resources_to_copy = get_resources_map(java_toolchain, package, resources, resources_root)
    resource_output = actions.declare_output("{}resources".format(actions_prefix))
    actions.symlinked_dir(resource_output, resources_to_copy)
    return resource_output

def _jar_creator(
        javac_tool: ["", None],
        java_toolchain: JavaToolchainInfo.type) -> "function":
    if javac_tool or java_toolchain.javac_protocol == "classic":
        return _create_jar_artifact
    elif java_toolchain.javac_protocol == "javacd":
        return create_jar_artifact_javacd
    else:
        fail("unrecognized javac protocol `{}`".format(java_toolchain.javac_protocol))

def compile_to_jar(
        ctx: "context",
        srcs: ["artifact"],
        *,
        abi_generation_mode: [AbiGenerationMode.type, None] = None,
        output: ["artifact", None] = None,
        actions_prefix: [str.type, None] = None,
        javac_tool: ["", None] = None,
        resources: [["artifact"], None] = None,
        resources_root: [str.type, None] = None,
        remove_classes: [[str.type], None] = None,
        manifest_file: ["artifact", None] = None,
        ap_params: [["AnnotationProcessorParams"], None] = None,
        plugin_params: ["PluginParams", None] = None,
        source_level: [int.type, None] = None,
        target_level: [int.type, None] = None,
        deps: [["dependency"], None] = None,
        required_for_source_only_abi: bool.type = False,
        source_only_abi_deps: [["dependency"], None] = None,
        extra_arguments: [["string"], None] = None,
        additional_classpath_entries: [["artifact"], None] = None,
        additional_compiled_srcs: ["artifact", None] = None,
        bootclasspath_entries: [["artifact"], None] = None) -> "JavaCompileOutputs":
    if not additional_classpath_entries:
        additional_classpath_entries = []
    if not bootclasspath_entries:
        bootclasspath_entries = []
    if not extra_arguments:
        extra_arguments = []
    if not resources:
        resources = []
    if not deps:
        deps = []
    if not remove_classes:
        remove_classes = []
    if not actions_prefix:
        actions_prefix = ""
    if not ap_params:
        ap_params = []
    if not source_only_abi_deps:
        source_only_abi_deps = []

    # TODO(cjhopman): Should verify that source_only_abi_deps are contained within the normal classpath.

    java_toolchain = ctx.attrs._java_toolchain[JavaToolchainInfo]
    if not source_level:
        source_level = _to_java_version(java_toolchain.source_level)
    if not target_level:
        target_level = _to_java_version(java_toolchain.target_level)

    return _jar_creator(javac_tool, java_toolchain)(
        ctx.actions,
        actions_prefix,
        abi_generation_mode,
        java_toolchain,
        ctx.label,
        output,
        javac_tool,
        srcs,
        remove_classes,
        resources,
        resources_root,
        manifest_file,
        ap_params,
        plugin_params,
        source_level,
        target_level,
        deps,
        required_for_source_only_abi,
        source_only_abi_deps,
        extra_arguments,
        additional_classpath_entries,
        additional_compiled_srcs,
        bootclasspath_entries,
    )

def _create_jar_artifact(
        actions: "actions",
        actions_prefix: str.type,
        _abi_generation_mode: [AbiGenerationMode.type, None],
        java_toolchain: JavaToolchainInfo.type,
        label: "label",
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
        _source_only_abi_deps: ["dependency"],
        extra_arguments: ["string"],
        additional_classpath_entries: ["artifact"],
        additional_compiled_srcs: ["artifact", None],
        bootclasspath_entries: ["artifact"]) -> "JavaCompileOutputs":
    """
    Creates jar artifact.

    Returns a single artifacts that represents jar output file
    """
    javac_tool = javac_tool or java_toolchain.javac
    unscrubbed_jar = actions.declare_output(paths.join(actions_prefix or "jar", "lib_unscrubbed.jar"))

    args = [
        java_toolchain.compile_and_package[RunInfo],
        "--jar_tool",
        java_toolchain.jar,
        "--output",
        unscrubbed_jar.as_output(),
    ]

    skip_javac = False if srcs or ap_params or plugin_params else True
    if skip_javac:
        args.append("--skip_javac_run")
    else:
        args += ["--javac_tool", javac_tool]

    if resources:
        resource_dir = _copy_resources(actions, actions_prefix, java_toolchain, label.package, resources, resources_root)
        args += ["--resources_dir", resource_dir]

    if manifest_file:
        args += ["--manifest", manifest_file]

    if additional_compiled_srcs:
        args += ["--additional_compiled_srcs", additional_compiled_srcs]

    compile_and_package_cmd = cmd_args(args)
    if not skip_javac:
        _append_javac_params(actions, actions_prefix, java_toolchain, srcs, remove_classes, ap_params, plugin_params, source_level, target_level, deps, extra_arguments, additional_classpath_entries, bootclasspath_entries, compile_and_package_cmd)

    actions.run(compile_and_package_cmd, category = "javac_and_jar", identifier = actions_prefix)

    jar_out = output or actions.declare_output(paths.join(actions_prefix or "jar", "lib.jar"))
    actions.run(
        cmd_args([java_toolchain.zip_scrubber, unscrubbed_jar, jar_out.as_output()]),
        category = "scrub_jar",
        identifier = actions_prefix,
    )

    abi = None if java_toolchain.is_bootstrap_toolchain else create_abi(actions, java_toolchain.class_abi_generator, jar_out)

    return make_compile_outputs(
        full_library = jar_out,
        class_abi = abi,
        required_for_source_only_abi = required_for_source_only_abi,
    )

def _check_dep_types(deps: ["dependency"]):
    for dep in deps:
        if not dep[JavaLibraryInfo] and not dep[SharedLibraryInfo]:
            fail("Received dependency {} is not supported. `java_library`, `prebuilt_jar` and native libraries are supported.".format(dep))

def _check_provided_deps(provided_deps: ["dependency"], attr_name: str.type):
    for provided_dep in provided_deps:
        expect(
            provided_dep[JavaLibraryInfo] != None or provided_dep[SharedLibraryInfo] == None,
            "Java code does not need native libs in order to compile, so not valid as {}: {}".format(attr_name, provided_dep),
        )

def _check_exported_deps(exported_deps: ["dependency"], attr_name: str.type):
    for exported_dep in exported_deps:
        expect(
            exported_dep[JavaLibraryInfo] != None,
            "Exported deps are meant to be forwarded onto the classpath for dependents, so only " +
            "make sense for a target that emits Java bytecode, {} in {} does not.".format(exported_dep, attr_name),
        )

# TODO(T108258238) remove need for this
def _skip_java_library_dep_checks(ctx: "context") -> bool.type:
    return "skip_buck2_java_library_dep_checks" in ctx.attrs.labels

def java_library_impl(ctx: "context") -> ["provider"]:
    """
     java_library() rule implementation

    Args:
        ctx: rule analysis context
    Returns:
        list of created providers
    """
    if not _skip_java_library_dep_checks(ctx):
        _check_dep_types(ctx.attrs.deps)
        _check_dep_types(ctx.attrs.provided_deps)
        _check_dep_types(ctx.attrs.exported_deps)
        _check_dep_types(ctx.attrs.exported_provided_deps)
        _check_dep_types(ctx.attrs.runtime_deps)

    java_providers = build_java_library(ctx, ctx.attrs.srcs)

    return to_list(java_providers) + [
        # TODO(T107163344) this shouldn't be in java_library itself, use overlays to remove it.
        merge_android_packageable_info(
            ctx.label,
            ctx.actions,
            ctx.attrs.deps + ctx.attrs.exported_deps + ctx.attrs.runtime_deps,
        ),
    ]

def build_java_library(
        ctx: "context",
        srcs: ["artifact"],
        run_annotation_processors = True,
        additional_classpath_entries: ["artifact"] = [],
        bootclasspath_entries: ["artifact"] = [],
        additional_compiled_srcs: ["artifact", None] = None) -> JavaProviders.type:
    _check_provided_deps(ctx.attrs.provided_deps, "provided_deps")
    _check_provided_deps(ctx.attrs.exported_provided_deps, "exported_provided_deps")
    _check_exported_deps(ctx.attrs.exported_deps, "exported_deps")
    _check_exported_deps(ctx.attrs.exported_provided_deps, "exported_provided_deps")

    deps_query = getattr(ctx.attrs, "deps_query", []) or []
    provided_deps_query = getattr(ctx.attrs, "provided_deps_query", []) or []
    first_order_deps = (
        ctx.attrs.deps +
        deps_query +
        ctx.attrs.exported_deps +
        ctx.attrs.provided_deps +
        provided_deps_query +
        ctx.attrs.exported_provided_deps
    )

    resources = ctx.attrs.resources
    ap_params = create_ap_params(
        ctx,
        ctx.attrs.plugins,
        ctx.attrs.annotation_processors,
        ctx.attrs.annotation_processor_params,
        ctx.attrs.annotation_processor_deps,
    ) if run_annotation_processors else None
    plugin_params = create_plugin_params(ctx.attrs.plugins) if run_annotation_processors else None
    manifest_file = ctx.attrs.manifest_file
    source_level, target_level = _get_java_version_attributes(ctx)
    javac_tool = derive_javac(ctx.attrs.javac) if ctx.attrs.javac else None

    outputs = None
    common_compile_kwargs = None
    sub_targets = {}
    if srcs or additional_compiled_srcs or resources or ap_params or plugin_params or manifest_file:
        abi_generation_mode = {
            None: None,
            "class": AbiGenerationMode("class"),
            "migrating_to_source_only": AbiGenerationMode("source"),
            "source": AbiGenerationMode("source"),
            "source_only": AbiGenerationMode("source_only"),
        }[ctx.attrs.abi_generation_mode]

        common_compile_kwargs = {
            "abi_generation_mode": abi_generation_mode,
            "additional_classpath_entries": additional_classpath_entries,
            "additional_compiled_srcs": additional_compiled_srcs,
            "ap_params": ap_params,
            "bootclasspath_entries": bootclasspath_entries,
            "deps": first_order_deps,
            "extra_arguments": ctx.attrs.extra_arguments,
            "manifest_file": manifest_file,
            "remove_classes": ctx.attrs.remove_classes,
            "required_for_source_only_abi": ctx.attrs.required_for_source_only_abi,
            "resources": resources,
            "resources_root": ctx.attrs.resources_root,
            "source_level": source_level,
            "source_only_abi_deps": ctx.attrs.source_only_abi_deps,
            "srcs": srcs,
            "target_level": target_level,
        }

        outputs = compile_to_jar(
            ctx,
            javac_tool = javac_tool,
            plugin_params = plugin_params,
            **common_compile_kwargs
        )

    java_toolchain = ctx.attrs._java_toolchain[JavaToolchainInfo]
    if common_compile_kwargs and srcs and not java_toolchain.is_bootstrap_toolchain:
        ast_dumper = java_toolchain.ast_dumper

        # Replace whatever compiler plugins are present with the AST dumper instead
        ast_output = ctx.actions.declare_output("ast_json")
        dump_ast_args = cmd_args(ast_output.as_output(), "--target-label", '"{}"'.format(ctx.label))
        for dep in _build_bootclasspath(bootclasspath_entries, source_level, java_toolchain):
            dump_ast_args.add("--dependency", '"{}"'.format(dep.owner), dep)
        classpath_args = _build_classpath(ctx.actions, first_order_deps, additional_classpath_entries, "args_for_ast_dumper")
        if classpath_args:
            dump_ast_args.add(classpath_args)
        ast_dumping_plugin_params = create_plugin_params([ast_dumper])
        ast_dumper_args_file = ctx.actions.write("dump_ast_args", dump_ast_args)
        ast_dumping_plugin_params = PluginParams(
            processors = ast_dumping_plugin_params.processors,
            deps = ast_dumping_plugin_params.deps,
            args = {
                "DumpAstPlugin": cmd_args(ast_dumper_args_file).hidden(dump_ast_args),
            },
        )

        # We don't actually care about the jar output this time; we just want the AST from the
        # plugin
        compile_to_jar(
            ctx,
            actions_prefix = "ast",
            plugin_params = ast_dumping_plugin_params,
            javac_tool = java_toolchain.fallback_javac,
            **common_compile_kwargs
        )

        sub_targets["ast"] = [DefaultInfo(default_outputs = [ast_output])]

    java_library_info, java_packaging_info, shared_library_info, cxx_resource_info, template_placeholder_info, intellij_info = create_java_library_providers(
        ctx,
        library_output = outputs.classpath_entry if outputs else None,
        declared_deps = ctx.attrs.deps + deps_query,
        exported_deps = ctx.attrs.exported_deps,
        provided_deps = ctx.attrs.provided_deps + provided_deps_query,
        exported_provided_deps = ctx.attrs.exported_provided_deps,
        runtime_deps = ctx.attrs.runtime_deps,
        needs_desugar = source_level > 7 or target_level > 7,
    )

    default_info = DefaultInfo()
    if outputs:
        abis = [
            ("class-abi", outputs.class_abi),
            ("source-abi", outputs.source_abi),
            ("source-only-abi", outputs.source_only_abi),
        ]
        for (name, artifact) in abis:
            if artifact != None:
                sub_targets[name] = [DefaultInfo(default_outputs = [artifact])]
        default_info = DefaultInfo(
            default_outputs = [outputs.full_library],
            sub_targets = sub_targets,
        )

    return JavaProviders(
        java_library_info = java_library_info,
        java_library_intellij_info = intellij_info,
        java_packaging_info = java_packaging_info,
        shared_library_info = shared_library_info,
        cxx_resource_info = cxx_resource_info,
        template_placeholder_info = template_placeholder_info,
        default_info = default_info,
    )
