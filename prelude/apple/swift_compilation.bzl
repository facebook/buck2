load(
    "@fbcode//buck2/prelude/cxx:compile.bzl",
    "CxxSrcWithFlags",  # @unused Used as a type
)
load("@fbcode//buck2/prelude/cxx:headers.bzl", "CHeader")
load(
    "@fbcode//buck2/prelude/cxx:preprocessor.bzl",
    "CPreprocessor",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
)
load(":apple_modular_utility.bzl", "MODULE_CACHE_PATH")
load(":apple_sdk_modules_utility.bzl", "get_sdk_deps_tset")
load(":apple_toolchain_types.bzl", "AppleToolchainInfo")
load(":apple_utility.bzl", "get_module_name", "get_versioned_target_triple")
load(":modulemap.bzl", "preprocessor_info_for_modulemap")
load(":swift_module_map.bzl", "write_swift_module_map_with_swift_deps")
load(":swift_pcm_compilation.bzl", "compile_swift_pcm")

def _add_swiftmodule_search_path(args: "cmd_args", swift_info: "SwiftDependencyInfo"):
    if swift_info.swiftmodule_path != None:
        # Value will contain a path to the artifact,
        # while we need only the folder which contains the artifact.
        args.add(["-I", cmd_args(swift_info.swiftmodule_path).parent()])

def _hidden_projection(args: "cmd_args", swift_info: "SwiftDependencyInfo"):
    # Value will contain a path to the artifact,
    # while we need only the folder which contains the artifact.
    args.add([swift_info.swiftmodule_path] if swift_info.swiftmodule_path != None else [])

SwiftmodulePathsTSet = transitive_set(args_projections = {
    "hidden": _hidden_projection,
    "module_search_path": _add_swiftmodule_search_path,
})

SwiftDependencyInfo = provider(fields = [
    "name",  # str.type
    "swiftmodule_path",  # ["cmd_args", None] A path to compiled swiftmodule artifact.
    "swiftmodule_deps_paths",  # [SwiftDependencyInfo]
])

SwiftCompilationOutput = record(
    # The object files output from compilation.
    object_files = field(["artifact"]),
    # The swiftmodule file output from compilation.
    swiftmodule = field("artifact"),
    # The dependency info provider that provides the swiftmodule
    # search paths required for compilation.
    providers = field([["SwiftPCMCompilationInfo", "SwiftDependencyInfo"]]),
    # Preprocessor info required for ObjC compilation of this library.
    pre = field(CPreprocessor.type),
    # Exported preprocessor info required for ObjC compilation of rdeps.
    exported_pre = field(CPreprocessor.type),
)

def compile_swift(
        ctx: "context",
        srcs: [CxxSrcWithFlags.type],
        exported_headers: [CHeader.type],
        objc_modulemap_pp_info: ["CPreprocessor", None]) -> ["SwiftCompilationOutput", None]:
    if not srcs:
        return None

    module_name = get_module_name(ctx)
    output_header = ctx.actions.declare_output(module_name + "-Swift.h")
    output_object = ctx.actions.declare_output(module_name + ".o")
    output_swiftmodule = ctx.actions.declare_output(module_name + ".swiftmodule")
    shared_flags = _get_shared_flags(ctx, module_name, srcs, exported_headers, objc_modulemap_pp_info)

    # We use separate actions for swiftmodule and object file output. This
    # improves build parallelism at the cost of duplicated work, but by disabling
    # type checking in function bodies the swiftmodule compilation can be done much
    # faster than object file output.
    swiftmodule_cmd = cmd_args(shared_flags)
    swiftmodule_cmd.add([
        "-Xfrontend",
        "-experimental-skip-non-inlinable-function-bodies",
        "-emit-module",
        "-emit-module-path",
        output_swiftmodule.as_output(),
        "-emit-objc-header",
        "-emit-objc-header-path",
        output_header.as_output(),
    ])
    ctx.actions.run(swiftmodule_cmd, category = "swiftmodule_compile")

    object_cmd = cmd_args(shared_flags)
    object_cmd.add([
        "-emit-object",
        "-o",
        output_object.as_output(),
    ])
    ctx.actions.run(object_cmd, category = "swift_compile")

    # Swift libraries extend the ObjC modulemaps to include the -Swift.h header
    modulemap_pp_info = preprocessor_info_for_modulemap(ctx, "swift-extended", exported_headers, output_header)
    exported_swift_header = CHeader(
        artifact = output_header,
        name = output_header.basename,
        namespace = module_name,
        named = False,
    )
    exported_pp_info = CPreprocessor(
        headers = [exported_swift_header],
        modular_args = modulemap_pp_info.modular_args,
        args = modulemap_pp_info.args,
    )

    # We also need to include the unprefixed -Swift.h header in this libraries preprocessor info
    swift_header = CHeader(
        artifact = output_header,
        name = output_header.basename,
        namespace = "",
        named = False,
    )
    pre = CPreprocessor(headers = [swift_header])

    # Pass up the swiftmodule paths for this module and its exported_deps
    return SwiftCompilationOutput(
        object_files = [output_object],
        swiftmodule = output_swiftmodule,
        providers = get_swift_dependency_infos(ctx, None, output_swiftmodule),
        pre = pre,
        exported_pre = exported_pp_info,
    )

def _get_shared_flags(
        ctx: "context",
        module_name: str.type,
        srcs: [CxxSrcWithFlags.type],
        objc_headers: [CHeader.type],
        objc_modulemap_pp_info: ["CPreprocessor", None]) -> "cmd_args":
    toolchain = ctx.attr._apple_toolchain[AppleToolchainInfo].swift_toolchain_info
    cmd = cmd_args(toolchain.compiler)
    cmd.add([
        # This allows us to use a relative path for the compiler resource directory.
        "-working-directory",
        ".",
        "-sdk",
        toolchain.sdk_path,
        "-target",
        get_versioned_target_triple(ctx),
        "-wmo",
        "-module-name",
        module_name,
        "-parse-as-library",
        # Disable Clang module breadcrumbs in the DWARF info. These will not be
        # debug prefix mapped and are not shareable across machines.
        "-Xfrontend",
        "-no-clang-module-breadcrumbs",
        "-module-cache-path",
        MODULE_CACHE_PATH,
    ])

    if toolchain.resource_dir:
        cmd.add([
            "-resource-dir",
            toolchain.resource_dir,
        ])

    if ctx.attr.swift_version:
        cmd.add(["-swift-version", ctx.attr.swift_version])

    if ctx.attr.enable_cxx_interop:
        cmd.add(["-Xfrontend", "-enable-cxx-interop"])

    if ctx.attr.serialize_debugging_options:
        if objc_headers:
            # TODO(T99100029): We cannot use VFS overlays with Buck2, so we have to disable
            # serializing debugging options for mixed libraries to debug successfully
            warning("Mixed libraries cannot serialize debugging options, disabling for module `{}` in rule `{}`".format(module_name, ctx.label))
        else:
            cmd.add(["-Xfrontend", "-serialize-debugging-options"])

    # Add flags required to import ObjC module dependencies
    _add_clang_deps_flags(ctx, cmd)
    _add_swift_deps_flags(ctx, cmd)

    # Add flags for importing the ObjC part of this library
    _add_mixed_library_flags_to_cmd(cmd, objc_headers, objc_modulemap_pp_info)

    # We can't have per source flags for Swift
    cmd.add([s.file for s in srcs])

    # Add toolchain and target flags last to allow for overriding defaults
    cmd.add(toolchain.compiler_flags)
    cmd.add(ctx.attr.swift_compiler_flags)

    return cmd

def _add_swift_deps_flags(ctx: "context", cmd: "cmd_args"):
    # If Explicit Modules are enabled, a few things must be provided to a compilation job:
    # 1. Direct and transitive SDK deps from `sdk_modules` attribute.
    # 2. Direct and transitive user-defined deps.
    # 3. Transitive SDK deps of user-defined deps.
    # (This is the case, when a user-defined dep exports a type from SDK module,
    # thus such SDK module should be implicitly visible to consumers of that custom dep)
    if ctx.attr.uses_explicit_modules:
        cmd.add(["-Xfrontend", "-disable-implicit-swift-modules"])

        toolchain = ctx.attr._apple_toolchain[AppleToolchainInfo].swift_toolchain_info
        module_name = get_module_name(ctx)
        sdk_deps_tset = get_sdk_deps_tset(ctx, module_name, toolchain)
        swift_deps_tset = ctx.actions.tset(
            SwiftmodulePathsTSet,
            children = _get_swift_paths_tsets(ctx, ctx.attr.deps + ctx.attr.exported_deps),
        )
        swift_deps_list = list(swift_deps_tset.traverse())

        swift_module_map_artifact = write_swift_module_map_with_swift_deps(
            ctx,
            module_name,
            list(sdk_deps_tset.traverse()),
            swift_deps_list,
        )
        cmd.add([
            "-Xfrontend",
            "-explicit-swift-module-map-file",
            "-Xfrontend",
            swift_module_map_artifact,
        ])

        # Add Clang sdk modules which do not go to swift modulemap
        cmd.add(sdk_deps_tset.project_as_args("clang_deps"))

        # Swift compilation should depend on transitive Swift modules from swift-module-map.
        cmd.hidden(sdk_deps_tset.project_as_args("hidden"))
        cmd.hidden(swift_deps_tset.project_as_args("hidden"))
    else:
        depset = ctx.actions.tset(SwiftmodulePathsTSet, children = _get_swift_paths_tsets(ctx, ctx.attr.deps + ctx.attr.exported_deps))
        cmd.add(depset.project_as_args("module_search_path"))

def _add_clang_deps_flags(ctx: "context", cmd: "cmd_args") -> None:
    inherited_preprocessor_infos = cxx_inherited_preprocessor_infos(ctx.attr.deps + ctx.attr.exported_deps)
    preprocessors = cxx_merge_cpreprocessors(ctx, [], inherited_preprocessor_infos)
    cmd.add(cmd_args(preprocessors.set.project_as_args("args"), prepend = "-Xcc"))
    cmd.add(cmd_args(preprocessors.set.project_as_args("modular_args"), prepend = "-Xcc"))
    cmd.add(cmd_args(preprocessors.set.project_as_args("include_dirs"), prepend = "-Xcc"))

def _add_mixed_library_flags_to_cmd(
        cmd: "cmd_args",
        objc_headers: [CHeader.type],
        objc_modulemap_pp_info: ["CPreprocessor", None]) -> None:
    if not objc_headers:
        return

    # TODO(T99100029): We cannot use VFS overlays to mask this import from
    # the debugger as they require absolute paths. Instead we will enforce
    # that mixed libraries do not have serialized debugging info and rely on
    # rdeps to serialize the correct paths.
    for arg in objc_modulemap_pp_info.args:
        cmd.add("-Xcc")
        cmd.add(arg)

    for arg in objc_modulemap_pp_info.modular_args:
        cmd.add("-Xcc")
        cmd.add(arg)

    cmd.add("-import-underlying-module")

def _get_swift_paths_tsets(ctx: "context", deps: ["dependency"]) -> ["SwiftmodulePathsTSet"]:
    return [
        ctx.actions.tset(
            SwiftmodulePathsTSet,
            value = d[SwiftDependencyInfo],
            children = d[SwiftDependencyInfo].swiftmodule_deps_paths,
        )
        for d in deps
        if d[SwiftDependencyInfo] != None
    ]

def get_swift_dependency_infos(
        ctx: "context",
        exported_pre: ["CPreprocessor", None],
        output_module: ["artifact", None]) -> [["SwiftPCMCompilationInfo", "SwiftDependencyInfo"]]:
    deps = ctx.attr.exported_deps
    if ctx.attr.reexport_all_header_dependencies:
        deps += ctx.attr.deps

    providers = [SwiftDependencyInfo(
        name = get_module_name(ctx),
        swiftmodule_path = output_module,
        swiftmodule_deps_paths = _get_swift_paths_tsets(ctx, deps),
    )]

    # If exported PP exists, we need to precompile a modulemap,
    # in order to enable consuming by Swift.
    if exported_pre and exported_pre.modulemap_path:
        providers.append(compile_swift_pcm(
            ctx,
            exported_pre,
        ))

    return providers
