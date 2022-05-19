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
load(":apple_target_sdk_version.bzl", "get_min_deployment_version_for_node")
load(":apple_toolchain_types.bzl", "AppleToolchainInfo")
load(":apple_utility.bzl", "get_module_name")
load(":modulemap.bzl", "preprocessor_info_for_modulemap")

def _add_swiftmodule_search_path(args: "cmd_args", value: "cmd_args"):
    args.add(["-I", value])

SwiftmoduleDirsTSet = transitive_set(args_projections = {"search_path": _add_swiftmodule_search_path})

SwiftDependencyInfo = provider(fields = [
    # A tset of paths to the containing folders of swiftmodule output.
    "swiftmodule_dirs",
])

SwiftCompilationOutput = record(
    # The object files output from compilation.
    object_files = field(["artifact"]),
    # The swiftmodule file output from compilation.
    swiftmodule = field("artifact"),
    # The dependency info provider that provides the swiftmodule
    # search paths required for compilation.
    provider = field(SwiftDependencyInfo.type),
    # Preprocessor info required for ObjC compilation of this library.
    pre = field(CPreprocessor.type),
    # Exported preprocessor info required for ObjC compilation of rdeps.
    exported_pre = field(CPreprocessor.type),
)

_VERSION_PLACEHOLDER = "(VERSION)"

# TODO(T115177501): Make target triples part of the toolchains
# Map from SDK name -> target triple _without_ leading architecture
_TARGET_TRIPLE_MAP = {
    "iphoneos": "apple-ios{}".format(_VERSION_PLACEHOLDER),
    "iphonesimulator": "apple-ios{}-simulator".format(_VERSION_PLACEHOLDER),
    "macosx": "apple-macosx{}".format(_VERSION_PLACEHOLDER),
    "watchos": "apple-watchos{}".format(_VERSION_PLACEHOLDER),
    "watchsimulator": "apple-watchos{}-simulator".format(_VERSION_PLACEHOLDER),
}

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

    # Pass up the swiftmodule search paths for this module and its exported_deps
    output_dir = cmd_args(output_swiftmodule).parent()
    return SwiftCompilationOutput(
        object_files = [output_object],
        swiftmodule = output_swiftmodule,
        provider = get_swift_dependency_info(ctx, output_dir),
        pre = pre,
        exported_pre = exported_pp_info,
    )

def _get_versioned_target_triple(ctx: "context") -> str.type:
    apple_toolchain_info = ctx.attr._apple_toolchain[AppleToolchainInfo]
    swift_toolchain_info = apple_toolchain_info.swift_toolchain_info

    architecture = swift_toolchain_info.architecture
    if architecture == None:
        fail("Need to set `architecture` field of swift_toolchain(), target: {}".format(ctx.label))

    target_sdk_version = get_min_deployment_version_for_node(ctx) or ""

    sdk_name = apple_toolchain_info.sdk_name
    target_triple_with_version_placeholder = _TARGET_TRIPLE_MAP.get(sdk_name)
    if target_triple_with_version_placeholder == None:
        fail("Could not find target triple for sdk = {}".format(sdk_name))

    versioned_target_triple = target_triple_with_version_placeholder.replace(_VERSION_PLACEHOLDER, target_sdk_version)
    return "{}-{}".format(architecture, versioned_target_triple)

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
        _get_versioned_target_triple(ctx),
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
    _add_clang_include_flags_to_cmd(ctx, cmd)

    # Add flags for importing the ObjC part of this library
    _add_mixed_library_flags_to_cmd(cmd, objc_headers, objc_modulemap_pp_info)

    # Add swiftmodule folder search paths required to import Swift module dependencies
    depset = ctx.actions.tset(SwiftmoduleDirsTSet, children = _get_swift_dir_tsets(ctx.attr.deps + ctx.attr.exported_deps))
    cmd.add(depset.project_as_args("search_path"))

    # We can't have per source flags for Swift
    cmd.add([s.file for s in srcs])

    # Add toolchain and target flags last to allow for overriding defaults
    cmd.add(toolchain.compiler_flags)
    cmd.add(ctx.attr.swift_compiler_flags)

    return cmd

def _add_clang_include_flags_to_cmd(ctx: "context", cmd: "cmd_args") -> None:
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

def _get_swift_dir_tsets(deps: ["dependency"]) -> ["SwiftmoduleDirsTSet"]:
    return [d[SwiftDependencyInfo].swiftmodule_dirs for d in deps if d[SwiftDependencyInfo] != None]

def get_swift_dependency_info(ctx: "context", output_dir: ["cmd_args", None]) -> "SwiftDependencyInfo":
    deps = ctx.attr.exported_deps
    if ctx.attr.reexport_all_header_dependencies:
        deps += ctx.attr.deps

    if output_dir:
        swiftmodule_dirs = ctx.actions.tset(SwiftmoduleDirsTSet, value = output_dir, children = _get_swift_dir_tsets(deps))
    else:
        swiftmodule_dirs = ctx.actions.tset(SwiftmoduleDirsTSet, children = _get_swift_dir_tsets(deps))

    return SwiftDependencyInfo(swiftmodule_dirs = swiftmodule_dirs)
