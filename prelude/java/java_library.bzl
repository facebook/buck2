load("@fbcode//buck2/prelude:paths.bzl", "paths")
load("@fbcode//buck2/prelude/android:android_providers.bzl", "merge_android_packageable_info")
load(
    "@fbcode//buck2/prelude/java:java_providers.bzl",
    "JavaLibraryInfo",
    "create_java_library_providers",
    "derive_compiling_deps",
    "make_compile_outputs",
    "maybe_create_abi",
)
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JavaToolchainInfo")
load("@fbcode//buck2/prelude/java/plugins:java_annotation_processor.bzl", "create_ap_params")
load("@fbcode//buck2/prelude/java/plugins:java_plugin.bzl", "create_plugin_params")
load("@fbcode//buck2/prelude/java/utils:java_utils.bzl", "derive_javac", "get_path_separator")
load("@fbcode//buck2/prelude/linking:shared_libraries.bzl", "SharedLibraryInfo")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

_JAVA_FILE_EXTENSION = [".java"]
_SUPPORTED_ARCHIVE_SUFFIXES = [".src.zip", "-sources.jar"]

def _get_java_version_attributes(ctx: "context") -> (int.type, int.type):
    java_toolchain = ctx.attr._java_toolchain[JavaToolchainInfo]
    java_version = ctx.attr.java_version
    java_source = ctx.attr.source
    java_target = ctx.attr.target

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
        expect(len(java_version) == 3, "Supported java vesion number format is 1.X, where X is a single digit numnber, but it was set to {}", java_version)
        java_version_number = int(java_version[2:])
        expect(java_version_number < 9, "Supported java vesion number format is 1.X, where X is a single digit numnber that is less than 9, but it was set to {}", java_version)
        return java_version_number
    else:
        return int(java_version)

def _process_classpath(
        actions: "actions",
        classpath_artifacts: ["artifact"],
        cmd: "cmd_args",
        args_file_name: "string",
        option_name: "string") -> "bool":
    classpath_artifacts = dedupe(classpath_artifacts)
    if not classpath_artifacts:
        return False

    classpath_cmd = cmd_args(
        classpath_artifacts,
        delimiter = get_path_separator(),
    )

    # write joined classpath string into args file
    classpath_args_file, classpath_macro_files = actions.write(
        args_file_name,
        classpath_cmd,
        allow_args = True,
    )

    # mark classpath artifacts as input
    cmd.hidden(classpath_artifacts, classpath_macro_files)

    # add classpath args file to cmd
    cmd.add(option_name, classpath_args_file)

    return True

def _process_plugins(
        actions: "actions",
        actions_prefix: str.type,
        ap_params: ["AnnotationProcessorParams"],
        plugin_params: ["PluginParams", None],
        javac_args: ["string"],
        cmd_args: "cmd_args"):
    processors_classpath = []

    # Process Annotation processors
    if ap_params:
        # For external javac, we can't preserve separate classpaths for separate processors. So we just concat everything.
        javac_args.append("-processor")
        joined_processors_string = ",".join([p for ap in ap_params for p in ap.processors])

        javac_args.append(joined_processors_string)

        for ap in ap_params:
            for param in ap.params:
                javac_args.append("-A{}".format(param))
            processors_classpath = processors_classpath + ap.deps

    else:
        javac_args.append("-proc:none")

    # Process Javac Plugins
    if plugin_params:
        javac_args.append("-Xplugin:{}".format(plugin_params.processors[0]))
        processors_classpath = processors_classpath + plugin_params.deps

    _process_classpath(
        actions,
        processors_classpath,
        cmd_args,
        "{}plugin_cp_args".format(actions_prefix),
        "--javac_processors_classpath_file",
    )

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
        cmd_args: "cmd_args"):
    javac_args = [
        "-encoding",
        "utf-8",
        # Set the sourcepath to stop us reading source files out of jars by mistake.
        "-sourcepath",
        '""',
    ] + extra_arguments

    # we want something that looks nice when prepended to another string, so add "_" to non-empty prefixes.
    if actions_prefix:
        actions_prefix += "_"

    compiling_classpath = [] + additional_classpath_entries
    compiling_deps_tset = derive_compiling_deps(actions, None, deps)
    if compiling_deps_tset:
        compiling_classpath.extend(
            [compiling_dep.abi for compiling_dep in list(compiling_deps_tset.traverse())],
        )

    if not _process_classpath(
        actions,
        compiling_classpath,
        cmd_args,
        "{}classpath_args".format(actions_prefix),
        "--javac_classpath_file",
    ):
        javac_args.append("-classpath ''")

    javac_args.append("-source")
    javac_args.append(str(source_level))
    javac_args.append("-target")
    javac_args.append(str(target_level))

    bootclasspath_list = []
    if source_level in [7, 8]:
        if bootclasspath_entries:
            bootclasspath_list = bootclasspath_entries
        elif source_level == 7:
            bootclasspath_list = java_toolchain.bootclasspath_7
        elif source_level == 8:
            bootclasspath_list = java_toolchain.bootclasspath_8

    if bootclasspath_list:
        _process_classpath(
            actions,
            bootclasspath_list,
            cmd_args,
            "{}bootclasspath_args".format(actions_prefix),
            "--javac_bootclasspath_file",
        )

    _process_plugins(
        actions,
        actions_prefix,
        annotation_processor_params,
        javac_plugin_params,
        javac_args,
        cmd_args,
    )

    generated_sources_dir = actions.declare_output("{}generated_sources".format(actions_prefix))
    cmd_args.add("--generated_sources_dir", generated_sources_dir.as_output())

    zipped_sources, plain_sources = split_on_archives_and_plain_files(srcs, _JAVA_FILE_EXTENSION)

    args_file, macro_files = actions.write(
        "{}javac_args".format(actions_prefix),
        javac_args + plain_sources,
        allow_args = True,
    )

    # mark plain srcs artifacts as input
    cmd_args.hidden(plain_sources, macro_files)

    cmd_args.add("--javac_args_file", args_file)

    if zipped_sources:
        cmd_args.add("--zipped_sources_file", actions.write("{}zipped_source_args".format(actions_prefix), zipped_sources))
        cmd_args.hidden(zipped_sources)

    if remove_classes:
        cmd_args.add("--remove_classes", actions.write("{}remove_classes_args".format(actions_prefix), remove_classes))

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

def _get_resources_map(
        java_toolchain: JavaToolchainInfo.type,
        package: str.type,
        resources: ["artifact"],
        resources_root: [str.type, None]) -> {str.type: "artifact"}:
    # As in v1, root the resource root via the current package.
    if resources_root != None:
        resources_root = paths.normalize(paths.join(package, resources_root))

    java_package_finder = _get_java_package_finder(java_toolchain)

    resources_to_copy = {}
    for resource in resources:
        # Create the full resource path.
        full_resource = paths.join(
            resource.owner.package if resource.owner else package,
            resource.short_path,
        )

        # As in v1 (https://fburl.com/code/j2vwny56, https://fburl.com/code/9era0xpz),
        # if this resource starts with the resource root, relativize and insert it as
        # is.
        if resources_root != None and paths.starts_with(full_resource, resources_root):
            resource_name = paths.relativize(
                full_resource,
                resources_root,
            )
        else:
            resource_name = java_package_finder(full_resource)
        resources_to_copy[resource_name] = resource
    return resources_to_copy

def _copy_resources(
        actions: "actions",
        java_toolchain: JavaToolchainInfo.type,
        package: str.type,
        resources: ["artifact"],
        resources_root: [str.type, None]) -> "artifact":
    resources_to_copy = _get_resources_map(java_toolchain, package, resources, resources_root)
    resource_output = actions.declare_output("resources")
    actions.symlinked_dir(resource_output, resources_to_copy)
    return resource_output

def _get_java_package_finder(java_toolchain: JavaToolchainInfo.type) -> "function":
    src_root_prefixes = java_toolchain.src_root_prefixes
    src_root_elements = java_toolchain.src_root_elements

    def finder(path):
        for prefix in src_root_prefixes:
            if path.startswith(prefix):
                return paths.relativize(
                    path,
                    prefix,
                )
        parts = path.split("/")
        for i in range(len(parts) - 1, -1, -1):
            part = parts[i]
            if part in src_root_elements:
                return "/".join(parts[i + 1:])

        return path

    return finder

def compile_to_jar(
        ctx: "context",
        srcs: ["artifact"],
        *,
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

    java_toolchain = ctx.attr._java_toolchain[JavaToolchainInfo]
    if not source_level:
        source_level = _to_java_version(java_toolchain.source_level)
    if not target_level:
        target_level = _to_java_version(java_toolchain.target_level)

    return _create_jar_artifact(
        ctx.actions,
        actions_prefix,
        java_toolchain,
        ctx.label.package,
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
        extra_arguments,
        additional_classpath_entries,
        additional_compiled_srcs,
        bootclasspath_entries,
    )

def _create_jar_artifact(
        actions: "actions",
        actions_prefix: str.type,
        java_toolchain: JavaToolchainInfo.type,
        package: str.type,
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
    """
    Creates jar artifact.

    Returns a single artifacts that represents jar output file
    """
    javac_tool = javac_tool or java_toolchain.javac
    jar_out = output or actions.declare_output(paths.join(actions_prefix or "jar", "lib.jar"))

    args = [
        java_toolchain.compile_and_package[RunInfo],
        "--jar_tool",
        java_toolchain.jar,
        "--output",
        jar_out.as_output(),
    ]

    skip_javac = False if srcs or ap_params or plugin_params else True
    if skip_javac:
        args.append("--skip_javac_run")
    else:
        args += ["--javac_tool", javac_tool]

    if resources:
        resource_dir = _copy_resources(actions, java_toolchain, package, resources, resources_root)
        args += ["--resources_dir", resource_dir]

    if manifest_file:
        args += ["--manifest", manifest_file]

    if additional_compiled_srcs:
        args += ["--additional_compiled_srcs", additional_compiled_srcs]

    compile_and_package_cmd = cmd_args(args)
    if not skip_javac:
        _append_javac_params(actions, actions_prefix, java_toolchain, srcs, remove_classes, ap_params, plugin_params, source_level, target_level, deps, extra_arguments, additional_classpath_entries, bootclasspath_entries, compile_and_package_cmd)

    actions.run(compile_and_package_cmd, category = "javac_and_jar", identifier = actions_prefix)
    abi = maybe_create_abi(actions, java_toolchain, jar_out)
    return make_compile_outputs(
        full_library = jar_out,
        class_abi = abi,
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

# TODO(T108258238) remove need for this
def _skip_java_library_dep_checks(ctx: "context") -> bool.type:
    return "skip_buck2_java_library_dep_checks" in ctx.attr.labels

def java_library_impl(ctx: "context") -> ["provider"]:
    """
     java_library() rule implementation

    Args:
        ctx: rule analysis context
    Returns:
        list of created providers
    """
    if not _skip_java_library_dep_checks(ctx):
        _check_dep_types(ctx.attr.deps)
        _check_dep_types(ctx.attr.provided_deps)
        _check_dep_types(ctx.attr.exported_deps)
        _check_dep_types(ctx.attr.exported_provided_deps)
        _check_dep_types(ctx.attr.runtime_deps)

    java_library_info, java_packaging_info, shared_library_info, cxx_resource_info, template_placeholder_info, default_info = build_java_library(ctx, ctx.attr.srcs)

    return [
        java_library_info,
        java_packaging_info,
        shared_library_info,
        cxx_resource_info,
        template_placeholder_info,
        default_info,
        # TODO(T107163344) this shouldn't be in java_library itself, use overlays to remove it.
        merge_android_packageable_info(
            ctx.actions,
            ctx.attr.deps + ctx.attr.exported_deps + ctx.attr.runtime_deps,
        ),
    ]

def build_java_library(
        ctx: "context",
        srcs: ["artifact"],
        run_annotation_processors = True,
        additional_classpath_entries: ["artifact"] = [],
        bootclasspath_entries: ["artifact"] = [],
        additional_compiled_srcs: ["artifact", None] = None) -> (
    JavaLibraryInfo.type,
    "JavaPackagingInfo",
    "SharedLibraryInfo",
    "CxxResourceInfo",
    TemplatePlaceholderInfo.type,
    DefaultInfo.type,
):
    _check_provided_deps(ctx.attr.provided_deps, "provided_deps")
    _check_provided_deps(ctx.attr.exported_provided_deps, "exported_provided_deps")

    deps_query = getattr(ctx.attr, "deps_query", []) or []
    provided_deps_query = getattr(ctx.attr, "provided_deps_query", []) or []
    first_order_deps = (
        ctx.attr.deps +
        deps_query +
        ctx.attr.exported_deps +
        ctx.attr.provided_deps +
        provided_deps_query +
        ctx.attr.exported_provided_deps
    )

    resources = ctx.attr.resources
    ap_params = create_ap_params(
        ctx,
        ctx.attr.plugins,
        ctx.attr.annotation_processors,
        ctx.attr.annotation_processor_params,
        ctx.attr.annotation_processor_deps,
    ) if run_annotation_processors else None
    plugin_params = create_plugin_params(ctx.attr.plugins) if run_annotation_processors else None
    manifest_file = ctx.attr.manifest_file
    source_level, target_level = _get_java_version_attributes(ctx)
    javac_tool = derive_javac(ctx.attr.javac) if ctx.attr.javac else None

    outputs = None
    if srcs or additional_compiled_srcs or resources or ap_params or plugin_params or manifest_file:
        outputs = compile_to_jar(
            ctx,
            javac_tool = javac_tool,
            srcs = srcs,
            deps = first_order_deps,
            remove_classes = ctx.attr.remove_classes,
            resources = resources,
            resources_root = ctx.attr.resources_root,
            manifest_file = manifest_file,
            ap_params = ap_params,
            plugin_params = plugin_params,
            source_level = source_level,
            target_level = target_level,
            extra_arguments = ctx.attr.extra_arguments,
            additional_classpath_entries = additional_classpath_entries,
            additional_compiled_srcs = additional_compiled_srcs,
            bootclasspath_entries = bootclasspath_entries,
        )

    java_library_info, java_packaging_info, shared_library_info, cxx_resource_info, template_placeholder_info = create_java_library_providers(
        ctx,
        library_output = outputs.classpath_entry if outputs else None,
        declared_deps = ctx.attr.deps + deps_query,
        exported_deps = ctx.attr.exported_deps,
        provided_deps = ctx.attr.provided_deps + provided_deps_query,
        exported_provided_deps = ctx.attr.exported_provided_deps,
        runtime_deps = ctx.attr.runtime_deps,
        needs_desugar = source_level > 7 or target_level > 7,
    )

    default_info = DefaultInfo()
    if outputs:
        abis = [
            ("class-abi", outputs.class_abi),
        ]
        default_info = DefaultInfo(
            default_outputs = [outputs.full_library],
            sub_targets = {
                name: [DefaultInfo(default_outputs = [artifact])]
                for (name, artifact) in abis
                if artifact != None
            },
        )

    return (
        java_library_info,
        java_packaging_info,
        shared_library_info,
        cxx_resource_info,
        template_placeholder_info,
        default_info,
    )
