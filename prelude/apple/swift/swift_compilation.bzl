# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactInfoTag",
    "ArtifactTSet",  # @unused Used as a type
    "make_artifact_tset",
    "project_artifacts",
)
load("@prelude//apple:apple_error_handler.bzl", "apple_build_error_handler")
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//apple:apple_utility.bzl", "get_disable_pch_validation_flags", "get_module_name")
load("@prelude//apple:modulemap.bzl", "create_modulemap")
load("@prelude//apple/swift:swift_helpers.bzl", "compile_with_argsfile", "uses_explicit_modules")
load("@prelude//apple/swift:swift_types.bzl", "SWIFTMODULE_EXTENSION", "SWIFT_EXTENSION", "SwiftMacroPlugin", "SwiftVersion", "get_implicit_framework_search_path_providers")
load("@prelude//cxx:argsfiles.bzl", "CompileArgsfile", "CompileArgsfiles")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_platform_info", "get_cxx_toolchain_info")
load("@prelude//cxx:cxx_library_utility.bzl", "cxx_use_shlib_intfs_mode")
load(
    "@prelude//cxx:cxx_sources.bzl",
    "CxxSrcWithFlags",  # @unused Used as a type
)
load("@prelude//cxx:cxx_toolchain_types.bzl", "ShlibInterfacesMode")
load("@prelude//cxx:headers.bzl", "CHeader")
load(
    "@prelude//cxx:link_groups.bzl",
    "get_link_group",
)
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
    "CPreprocessorArgs",
    "CPreprocessorInfo",  # @unused Used as a type
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
)
load("@prelude//cxx:target_sdk_version.bzl", "get_target_triple")
load(
    "@prelude//linking:link_info.bzl",
    "LinkInfo",  # @unused Used as a type
    "SwiftmoduleLinkable",  # @unused Used as a type
)
load("@prelude//utils:arglike.bzl", "ArgLike")
load(":apple_sdk_modules_utility.bzl", "get_compiled_sdk_clang_deps_tset", "get_compiled_sdk_swift_deps_tset", "get_uncompiled_sdk_deps", "is_sdk_modules_provided")
load(
    ":swift_debug_info_utils.bzl",
    "extract_and_merge_clang_debug_infos",
    "extract_and_merge_swift_debug_infos",
)
load(
    ":swift_incremental_support.bzl",
    "IncrementalCompilationInput",
    "get_incremental_file_hashing_enabled",
    "get_incremental_object_compilation_flags",
    "should_build_swift_incrementally",
)
load(":swift_module_map.bzl", "write_swift_module_map_with_deps")
load(":swift_output_file_map.bzl", "add_dependencies_output")
load(":swift_pcm_compilation.bzl", "compile_underlying_pcm", "get_compiled_pcm_deps_tset", "get_swift_pcm_anon_targets")
load(
    ":swift_pcm_compilation_types.bzl",
    "SwiftPCMUncompiledInfo",
)
load(":swift_sdk_flags.bzl", "get_sdk_flags")
load(":swift_sdk_pcm_compilation.bzl", "get_swift_sdk_pcm_anon_targets")
load(":swift_swiftinterface_compilation.bzl", "get_swift_interface_anon_targets")
load(":swift_toolchain.bzl", "get_swift_toolchain_info")
load(
    ":swift_toolchain_types.bzl",
    "SwiftCompiledModuleInfo",
    "SwiftCompiledModuleTset",
    "SwiftObjectFormat",
    "SwiftToolchainInfo",
)

SwiftDependencyInfo = provider(fields = {
    "debug_info_tset": provider_field(ArtifactTSet),
    # Includes modules through exported_deps, used for compilation
    "exported_swiftmodules": provider_field(SwiftCompiledModuleTset),
    # Macro deps cannot be mixed with apple_library deps
    "is_macro": provider_field(bool),
})

SwiftCompilationDatabase = record(
    db = field(Artifact),
    other_outputs = field(ArgLike),
)

SwiftObjectOutput = record(
    object_files = field(list[Artifact]),
    argsfiles = field(CompileArgsfiles),
    output_map_artifact = field(Artifact | None),
    swiftdeps = field(list[Artifact]),
)

SwiftLibraryForDistributionOutput = record(
    swiftinterface = field(Artifact),
    private_swiftinterface = field(Artifact),
    swiftdoc = field(Artifact),
)

SwiftCompilationOutput = record(
    # The object file output from compilation.
    object_files = field(list[Artifact]),
    object_format = field(SwiftObjectFormat),
    # The swiftmodule file output from compilation.
    swiftmodule = field(Artifact),
    # The dependency info provider that contains the swiftmodule
    # search paths required for compilation and linking.
    dependency_info = field(SwiftDependencyInfo),
    # Preprocessor info required for ObjC compilation of this library.
    pre = field(CPreprocessor),
    # Exported preprocessor info required for ObjC compilation of rdeps.
    exported_pre = field(CPreprocessor),
    # Exported -Swift.h header
    exported_swift_header = field(Artifact),
    # Argsfiles used to compile object files.
    argsfiles = field(CompileArgsfiles),
    # A tset of (SDK/first-party) swiftmodule artifacts required to be linked into binary.
    # Different outputs of Swift compilation actions might contain duplicates of SDK swiftmodule artifacts.
    swift_debug_info = field(ArtifactTSet),
    # A tset of PCM artifacts used to compile a Swift module.
    clang_debug_info = field(ArtifactTSet),
    # Info required for `[swift-compilation-database]` subtarget.
    compilation_database = field(SwiftCompilationDatabase),
    # An artifact that represent the Swift module map for this target.
    output_map_artifact = field(Artifact | None),
    # An optional artifact of the exported symbols emitted for this module.
    exported_symbols = field(Artifact | None),
    # An optional artifact with files that support consuming the generated library with later versions of the swift compiler.
    swift_library_for_distribution_output = field(SwiftLibraryForDistributionOutput | None),
    # A list of artifacts that stores the index data
    index_store = field(Artifact),
    # A list of artifacts of the swiftdeps files produced during incremental compilation.
    swiftdeps = field(list[Artifact]),
    compiled_underlying_pcm_artifact = field(Artifact | None),
)

SwiftDebugInfo = record(
    static = list[ArtifactTSet],
    shared = list[ArtifactTSet],
)

_IS_USER_BUILD = True # @oss-enable
# @oss-disable: # To determine whether we're running on CI or not, we expect user.sandcastle_alias to be set.
# @oss-disable[end= ]: _IS_USER_BUILD = (read_root_config("user", "sandcastle_alias", None) == None)

_REQUIRED_SDK_MODULES = ["Swift", "SwiftOnoneSupport", "Darwin", "_Concurrency", "_StringProcessing"]

_REQUIRED_SDK_CXX_MODULES = _REQUIRED_SDK_MODULES + ["std"]

def _get_target_flags(ctx) -> list[str]:
    if get_cxx_platform_info(ctx).name.startswith("linux"):
        # Linux triples are unversioned
        return ["-target", "{}-unknown-linux-gnu".format(get_swift_toolchain_info(ctx).architecture)]
    else:
        return ["-target", get_target_triple(ctx)]

def get_swift_framework_anonymous_targets(ctx: AnalysisContext, get_providers: typing.Callable) -> Promise:
    # Get SDK deps from direct dependencies,
    # all transitive deps will be compiled recursively.
    direct_uncompiled_sdk_deps = get_uncompiled_sdk_deps(
        ctx.attrs.sdk_modules,
        _REQUIRED_SDK_MODULES,
        get_swift_toolchain_info(ctx),
    )

    # Recursively compiling headers of direct and transitive deps as PCM modules,
    # prebuilt_apple_framework rule doesn't support custom compiler_flags, so we pass only target triple.
    pcm_targets = get_swift_pcm_anon_targets(
        ctx,
        ctx.attrs.deps,
        _get_target_flags(ctx),
        False,  # C++ Interop is disabled for now.
    )

    # Recursively compiling SDK's Clang dependencies,
    # prebuilt_apple_framework rule doesn't support custom compiler_flags, so we pass only target triple.
    sdk_pcm_targets = get_swift_sdk_pcm_anon_targets(
        ctx,
        False,
        direct_uncompiled_sdk_deps,
        _get_target_flags(ctx),
    )

    # Recursively compile SDK and prebuilt_apple_framework's Swift dependencies.
    swift_interface_anon_targets = get_swift_interface_anon_targets(
        ctx,
        direct_uncompiled_sdk_deps,
    )
    return ctx.actions.anon_targets(pcm_targets + sdk_pcm_targets + swift_interface_anon_targets).promise.map(get_providers)

def get_swift_anonymous_targets(ctx: AnalysisContext, get_apple_library_providers: typing.Callable) -> Promise:
    swift_cxx_flags = get_swift_cxx_flags(ctx)

    # Get SDK deps from direct dependencies,
    # all transitive deps will be compiled recursively.
    direct_uncompiled_sdk_deps = get_uncompiled_sdk_deps(
        ctx.attrs.sdk_modules,
        _REQUIRED_SDK_CXX_MODULES if ctx.attrs.enable_cxx_interop else _REQUIRED_SDK_MODULES,
        get_swift_toolchain_info(ctx),
    )

    # Recursively compiling headers of direct and transitive deps as PCM modules,
    # passing apple_library's cxx flags through that must be used for all downward PCM compilations.
    pcm_targets = get_swift_pcm_anon_targets(
        ctx,
        ctx.attrs.deps + getattr(ctx.attrs, "exported_deps", []),
        swift_cxx_flags,
        ctx.attrs.enable_cxx_interop,
    )

    # Recursively compiling SDK's Clang dependencies,
    # passing apple_library's cxx flags through that must be used for all downward PCM compilations.
    sdk_pcm_targets = get_swift_sdk_pcm_anon_targets(
        ctx,
        ctx.attrs.enable_cxx_interop,
        direct_uncompiled_sdk_deps,
        swift_cxx_flags,
    )

    # Recursively compiling SDK's Swift dependencies,
    # passing apple_library's cxx flags through that must be used for all downward PCM compilations.
    swift_interface_anon_targets = get_swift_interface_anon_targets(
        ctx,
        direct_uncompiled_sdk_deps,
    )
    return ctx.actions.anon_targets(pcm_targets + sdk_pcm_targets + swift_interface_anon_targets).promise.map(get_apple_library_providers)

def get_swift_cxx_flags(ctx: AnalysisContext) -> list[str]:
    """Iterates through `swift_compiler_flags` and returns a list of flags that might affect Clang compilation"""
    gather, next = ([], False)

    # Each target needs to propagate the compilers target triple.
    # This can vary depending on the deployment target of each library.
    gather += _get_target_flags(ctx)

    for f in ctx.attrs.swift_compiler_flags:
        if next:
            gather.append("-Xcc")
            gather.append(str(f).replace('\"', ""))
            next = False
        elif str(f) == "\"-Xcc\"":
            next = True
        elif str(f) == "\"-warnings-as-errors\"":
            gather.append("-warnings-as-errors")
        elif str(f) == "\"-no-warnings-as-errors\"":
            gather.append("-no-warnings-as-errors")

    if ctx.attrs.enable_cxx_interop:
        gather += ["-cxx-interoperability-mode=default"]

    if ctx.attrs.swift_version != None:
        gather += ["-swift-version", ctx.attrs.swift_version]

    return gather

def _get_compiled_underlying_pcm(
        ctx: AnalysisContext,
        module_name: str,
        module_pp_info: CPreprocessor | None,
        deps_providers: list,
        swift_cxx_flags: list[str],
        framework_search_paths: cmd_args) -> SwiftCompiledModuleInfo | None:
    underlying_swift_pcm_uncompiled_info = get_swift_pcm_uncompile_info(
        ctx,
        None,
        module_pp_info,
    )
    if not underlying_swift_pcm_uncompiled_info:
        return None

    return compile_underlying_pcm(
        ctx,
        module_name,
        underlying_swift_pcm_uncompiled_info,
        deps_providers,
        swift_cxx_flags,
        framework_search_paths,
    )

def _should_compile_with_swift_interface(ctx):
    if not get_swift_toolchain_info(ctx).library_interface_uses_swiftinterface:
        return False

    if ctx.attrs._swift_enable_testing:
        return False

    return ctx.attrs.swift_interface_compilation_enabled and uses_explicit_modules(ctx)

def compile_swift(
        ctx: AnalysisContext,
        srcs: list[CxxSrcWithFlags],
        parse_as_library: bool,
        deps_providers: list,
        module_name: str,
        private_module_name: str,
        exported_headers: list[CHeader],
        exported_objc_modulemap_pp_info: [CPreprocessor, None],
        private_objc_modulemap_pp_info: [CPreprocessor, None],
        framework_search_paths_flags: cmd_args,
        extra_search_paths_flags: list[ArgLike] = [],
        compile_category = "swift_compile",
        compile_swiftmodule_category = "swiftmodule_compile",
        is_macro = False) -> ([SwiftCompilationOutput, None], DefaultInfo):
    # If this target imports XCTest we need to pass the search path to its swiftmodule.
    framework_search_paths = cmd_args()
    framework_search_paths.add(_get_xctest_swiftmodule_search_path(ctx))

    # Pass the framework search paths to the driver and clang importer. This is required
    # for pcm compilation, which does not pass through driver search paths.
    framework_search_paths.add(framework_search_paths_flags)
    framework_search_paths.add(cmd_args(framework_search_paths_flags, prepend = "-Xcc"))

    # If a target exports ObjC headers and Swift explicit modules are enabled,
    # we need to precompile a PCM of the underlying module and supply it to the Swift compilation.
    swift_cxx_flags = get_swift_cxx_flags(ctx)
    if uses_explicit_modules(ctx):
        exported_compiled_underlying_pcm = _get_compiled_underlying_pcm(
            ctx,
            module_name,
            exported_objc_modulemap_pp_info,
            deps_providers,
            swift_cxx_flags,
            framework_search_paths,
        ) if exported_objc_modulemap_pp_info else None
        private_compiled_underlying_pcm = _get_compiled_underlying_pcm(
            ctx,
            private_module_name,
            private_objc_modulemap_pp_info,
            deps_providers,
            swift_cxx_flags,
            framework_search_paths,
        ) if private_objc_modulemap_pp_info else None
    else:
        exported_compiled_underlying_pcm = None
        private_compiled_underlying_pcm = None

    # We always track inputs for dependency file tracking, but optionally set
    # the tag on the action depending on the use_depsfiles config.
    inputs_tag = ctx.actions.artifact_tag()

    shared_flags = _get_shared_flags(
        ctx = ctx,
        deps_providers = deps_providers,
        parse_as_library = parse_as_library,
        private_module = private_compiled_underlying_pcm,
        public_module = exported_compiled_underlying_pcm,
        module_name = module_name,
        private_modulemap_pp_info = private_objc_modulemap_pp_info,
        public_modulemap_pp_info = exported_objc_modulemap_pp_info,
        extra_search_paths_flags = extra_search_paths_flags,
        inputs_tag = inputs_tag,
        is_macro = is_macro,
    )
    shared_flags.add(framework_search_paths)
    swift_interface_info = _create_swift_interface(ctx, shared_flags, module_name)

    if not srcs:
        return (None, swift_interface_info)

    toolchain = get_swift_toolchain_info(ctx)
    output_header = ctx.actions.declare_output(module_name + "-Swift.h")
    output_swiftmodule = ctx.actions.declare_output(module_name + SWIFTMODULE_EXTENSION)

    swift_framework_output = None
    swiftinterface_output = None
    if _should_compile_with_evolution(ctx):
        swift_framework_output = SwiftLibraryForDistributionOutput(
            swiftinterface = ctx.actions.declare_output(module_name + ".swiftinterface"),
            private_swiftinterface = ctx.actions.declare_output(module_name + ".private.swiftinterface"),
            swiftdoc = ctx.actions.declare_output(module_name + ".swiftdoc"),  #this is generated automatically once we pass -emit-module-info, so must have this name
        )
    elif _should_compile_with_swift_interface(ctx):
        swiftinterface_output = ctx.actions.declare_output(get_module_name(ctx) + ".swiftinterface")

    output_symbols = None
    if cxx_use_shlib_intfs_mode(ctx, ShlibInterfacesMode("stub_from_headers")):
        output_symbols = ctx.actions.declare_output("__tbd__/" + module_name + ".swift_symbols.txt")

    # When compiling with WMO (ie, not incrementally), we compile the
    # swiftmodule separately. In incremental mode, we generate the swiftmodule
    # as part of the compile action to make use of incrementality.
    if not should_build_swift_incrementally(ctx):
        _compile_swiftmodule(
            ctx,
            toolchain,
            shared_flags,
            srcs,
            swiftinterface_output,
            output_swiftmodule,
            output_header,
            output_symbols,
            swift_framework_output,
            inputs_tag,
            compile_swiftmodule_category,
        )

    object_output = _compile_object(
        ctx = ctx,
        toolchain = toolchain,
        shared_flags = shared_flags,
        srcs = srcs,
        output_swiftmodule = output_swiftmodule,
        output_header = output_header,
        inputs_tag = inputs_tag,
        category = compile_category,
    )

    index_store = _compile_index_store(ctx, toolchain, shared_flags, srcs)

    # Swift libraries extend the ObjC modulemaps to include the -Swift.h header
    modulemap_pp_info, extended_modulemap = create_modulemap(
        ctx,
        name = "swift-extended",
        module_name = module_name,
        headers = exported_headers,
        swift_header = output_header,
        mark_headers_private = False,
        additional_args = None,
    )
    exported_swift_header = CHeader(
        artifact = output_header,
        name = output_header.basename,
        namespace = module_name,
        named = False,
    )
    exported_pp_info = CPreprocessor(
        headers = [exported_swift_header],
        modular_args = modulemap_pp_info.modular_args,
        args = CPreprocessorArgs(args = modulemap_pp_info.args.args),
        modulemap_path = modulemap_pp_info.modulemap_path,
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
    return (SwiftCompilationOutput(
        output_map_artifact = object_output.output_map_artifact,
        object_files = object_output.object_files,
        object_format = toolchain.object_format,
        swiftmodule = output_swiftmodule,
        compiled_underlying_pcm_artifact = exported_compiled_underlying_pcm.output_artifact if exported_compiled_underlying_pcm else None,
        dependency_info = get_swift_dependency_info(ctx, output_swiftmodule, swiftinterface_output, deps_providers, is_macro),
        pre = pre,
        exported_pre = exported_pp_info,
        exported_swift_header = exported_swift_header.artifact,
        argsfiles = object_output.argsfiles,
        swift_debug_info = extract_and_merge_swift_debug_infos(ctx, deps_providers, [output_swiftmodule]),
        clang_debug_info = extract_and_merge_clang_debug_infos(
            ctx,
            deps_providers,
            filter(
                None,
                [
                    exported_compiled_underlying_pcm.output_artifact if exported_compiled_underlying_pcm else None,
                    private_compiled_underlying_pcm.output_artifact if private_compiled_underlying_pcm else None,
                    extended_modulemap,
                ],
            ) +
            (exported_compiled_underlying_pcm.clang_modulemap_artifacts if exported_compiled_underlying_pcm else []) +
            (private_compiled_underlying_pcm.clang_modulemap_artifacts if private_compiled_underlying_pcm else []),
        ),
        compilation_database = _create_compilation_database(ctx, srcs, object_output.argsfiles.relative[SWIFT_EXTENSION]),
        exported_symbols = output_symbols,
        swift_library_for_distribution_output = swift_framework_output,
        index_store = index_store,
        swiftdeps = object_output.swiftdeps,
    ), swift_interface_info)

# We use separate actions for swiftmodule and object file output. This
# improves build parallelism at the cost of duplicated work, but by disabling
# type checking in function bodies the swiftmodule compilation can be done much
# faster than object file output.
def _compile_swiftmodule(
        ctx: AnalysisContext,
        toolchain: SwiftToolchainInfo,
        shared_flags: cmd_args,
        srcs: list[CxxSrcWithFlags],
        output_swiftinterface: Artifact | None,
        output_swiftmodule: Artifact,
        output_header: Artifact,
        output_symbols: Artifact | None,
        swift_framework_output: SwiftLibraryForDistributionOutput | None,
        inputs_tag: ArtifactTag,
        category: str) -> CompileArgsfiles:
    if output_swiftinterface:
        # We compile the interface in two passes:
        #  1. generate the ObjC header and swiftinterface file
        #  2. generate the swiftmodule file from the intermediate swiftinterface file
        #
        # This is more work overall, but as the swiftinterface file only contains
        # the public API we should have improved cache hit reducing the amount of
        # subsequent swiftmodule compile actions.
        swiftinterface_argsfile = cmd_args(shared_flags)
        swiftinterface_argsfile.add([
            # Required as emitting a swiftinterface without library evolution
            # produces a warning.
            "-no-warnings-as-errors",
            # Workaround to avoid producing swiftdoc and other auxiliary outputs.
            "-typecheck",
            "-wmo",
        ])
        swiftinterface_cmd = cmd_args([
            "-emit-objc-header",
            "-emit-objc-header-path",
            output_header.as_output(),
            "-emit-module-interface",
            "-emit-module-interface-path",
            output_swiftinterface.as_output(),
            "-Xwrapper",
            "-remove-module-prefixes",
        ])
        _compile_with_argsfile(
            ctx = ctx,
            category = "emit_swiftinterface",
            shared_flags = swiftinterface_argsfile,
            srcs = srcs,
            additional_flags = swiftinterface_cmd,
            toolchain = toolchain,
        )
        argfile_cmd = cmd_args(shared_flags)
        argfile_cmd.add([
            "-disable-cmo",
            "-wmo",
        ])
        cmd = cmd_args([
            "-c",
            "-Xfrontend",
            "-compile-module-from-interface",
            output_swiftinterface,
            # The new driver will fail with "error: no input files"
            # so use the legacy driver until we add functionality for this.
            "-disallow-use-new-driver",
            "-o",
            output_swiftmodule.as_output(),
        ])

        # We don't need the Swift srcs to compile a swiftmodule
        # from a generated swiftinterface file.
        srcs = []
    else:
        argfile_cmd = cmd_args(shared_flags)
        argfile_cmd.add([
            "-disable-cmo",
            "-wmo",
        ])

        if ctx.attrs.swift_module_skip_function_bodies:
            argfile_cmd.add([
                "-Xfrontend",
                "-experimental-skip-non-inlinable-function-bodies-without-types",
            ])

        cmd = cmd_args([
            "-emit-objc-header",
            "-emit-objc-header-path",
            output_header.as_output(),
            "-emit-module",
            "-emit-module-path",
            output_swiftmodule.as_output(),
        ])

        if swift_framework_output:
            argfile_cmd.add([
                "-enable-library-evolution",
            ])
            cmd.add([
                "-emit-module-interface",
                "-emit-module-interface-path",
                swift_framework_output.swiftinterface.as_output(),
                "-emit-private-module-interface-path",
                swift_framework_output.private_swiftinterface.as_output(),
                # Module verification fails here as the clang modules
                # are not loaded correctly.
                "-no-verify-emitted-module-interface",
            ])

            # There is no driver flag to specify the swiftdoc output path
            # TODO: use an output file map for this.
            cmd.add(cmd_args(hidden = swift_framework_output.swiftdoc.as_output()))

    output_tbd = None
    if output_symbols != None:
        # Two step process, first we need to emit the TBD
        output_tbd = ctx.actions.declare_output("__tbd__/" + ctx.attrs.name + "-Swift.tbd")
        cmd.add([
            "-emit-tbd",
            "-emit-tbd-path",
            output_tbd.as_output(),
        ])

    dep_files = {}
    output_file_map = {}
    if toolchain.use_depsfiles and not output_swiftinterface:
        add_dependencies_output(ctx, output_file_map, cmd, "swiftmodule", inputs_tag)
        dep_files["swiftmodule"] = inputs_tag

    ret, _ = _compile_with_argsfile(
        ctx = ctx,
        category = category,
        extension = SWIFTMODULE_EXTENSION,
        shared_flags = argfile_cmd,
        srcs = srcs,
        additional_flags = cmd,
        toolchain = toolchain,
        num_threads = 1,
        dep_files = dep_files,
        output_file_map = output_file_map,
    )

    if output_tbd != None:
        # Now we have run the TBD action we need to extract the symbols
        extract_cmd = cmd_args([
            get_cxx_toolchain_info(ctx).linker_info.mk_shlib_intf[RunInfo],
            "extract",
            "-o",
            output_symbols.as_output(),
            "--tbd",
            output_tbd,
        ])
        ctx.actions.run(extract_cmd, category = "extract_tbd_symbols", error_handler = apple_build_error_handler)

    return ret

def _compile_object(
        ctx: AnalysisContext,
        toolchain: SwiftToolchainInfo,
        shared_flags: cmd_args,
        srcs: list[CxxSrcWithFlags],
        output_swiftmodule: Artifact,
        output_header: Artifact,
        inputs_tag: ArtifactTag,
        category: str) -> SwiftObjectOutput:
    dep_files = {}
    output_file_map = {}
    emit_depsfiles = toolchain.use_depsfiles
    skip_incremental_outputs = False

    if should_build_swift_incrementally(ctx):
        incremental_compilation_output = get_incremental_object_compilation_flags(ctx, srcs, output_swiftmodule, output_header)
        cmd = incremental_compilation_output.incremental_flags_cmd

        # With -skip-incremental-output the output_file_map is an output, so
        # we cannot support depsfiles.
        emit_depsfiles = emit_depsfiles and not incremental_compilation_output.skip_incremental_outputs
        num_threads = incremental_compilation_output.num_threads
        objects = incremental_compilation_output.artifacts
        output_file_map = incremental_compilation_output.output_file_map
        skip_incremental_outputs = incremental_compilation_output.skip_incremental_outputs
        swiftdeps = incremental_compilation_output.swiftdeps
        depfiles = incremental_compilation_output.depfiles
    else:
        num_threads = 1
        swiftdeps = []
        depfiles = []
        output_object = ctx.actions.declare_output(get_module_name(ctx) + ".o")
        objects = [output_object]
        object_format = toolchain.object_format.value
        embed_bitcode = False
        if toolchain.object_format == SwiftObjectFormat("object-embed-bitcode"):
            object_format = "object"
            embed_bitcode = True

        cmd = cmd_args([
            "-emit-{}".format(object_format),
            "-o",
            output_object.as_output(),
            "-wmo",
        ])

        if embed_bitcode:
            cmd.add("--embed-bitcode")

    if emit_depsfiles:
        add_dependencies_output(ctx, output_file_map, cmd, "object", inputs_tag)
        dep_files["object"] = inputs_tag

    if _should_compile_with_evolution(ctx):
        cmd.add(["-enable-library-evolution"])

    argsfiles, output_map_artifact = _compile_with_argsfile(
        ctx = ctx,
        category = category,
        extension = SWIFT_EXTENSION,
        shared_flags = shared_flags,
        srcs = srcs,
        additional_flags = cmd,
        toolchain = toolchain,
        num_threads = num_threads,
        dep_files = dep_files,
        output_file_map = output_file_map,
        skip_incremental_outputs = skip_incremental_outputs,
        objects = objects,
        incremental_artifacts = IncrementalCompilationInput(depfiles = depfiles, swiftdeps = swiftdeps),
    )

    return SwiftObjectOutput(
        object_files = objects,
        argsfiles = argsfiles,
        output_map_artifact = output_map_artifact,
        swiftdeps = swiftdeps,
    )

def _compile_index_store(
        ctx: AnalysisContext,
        toolchain: SwiftToolchainInfo,
        shared_flags: cmd_args,
        srcs: list[CxxSrcWithFlags]) -> Artifact:
    module_name = get_module_name(ctx)

    # We need an output file map with index-unit-output-path entries to be able
    # to index all of the srcs in a single pass.
    output_file_map = {}
    object_outputs = []
    for src in srcs:
        # We have to declare outputs for all the srcs as the Swift driver will
        # not allow us to share the same output path for multiple inputs. We
        # don't care about the output, but there is no way to get an output
        # path without declaring one.
        obj_out = ctx.actions.declare_output("__indexstore__/" + src.file.basename + ".o")
        object_outputs.append(obj_out.as_output())
        output_file_map[src.file] = {
            # Not declared as output, we have the path already so this can be
            # thrown away.
            "diagnostics": cmd_args(obj_out, format = "{}.dia", delimiter = ""),
            # The output here is only used for the identifier of the index unit file
            "index-unit-output-path": src.file,
            "object": cmd_args(obj_out, delimiter = ""),
        }

    index_store_output = ctx.actions.declare_output("__indexstore__/swift_{}".format(module_name), dir = True)
    additional_flags = cmd_args(
        "-index-ignore-system-modules",
        "-index-store-path",
        index_store_output.as_output(),
        "-c",
        "-disable-batch-mode",
        "-Xwrapper",
        "-ignore-errors",
        hidden = object_outputs,
    )

    _compile_with_argsfile(
        ctx = ctx,
        category = "swift_index_compile",
        shared_flags = shared_flags,
        srcs = srcs,
        additional_flags = additional_flags,
        toolchain = toolchain,
        cacheable = True,
        output_file_map = output_file_map,
    )

    return index_store_output

def _compile_with_argsfile(
        ctx: AnalysisContext,
        category: str,
        shared_flags: cmd_args,
        srcs: list[CxxSrcWithFlags],
        additional_flags: cmd_args,
        toolchain: SwiftToolchainInfo,
        extension: str | None = None,
        num_threads: int = 1,
        dep_files: dict[str, ArtifactTag] = {},
        output_file_map: dict = {},
        cacheable = True,
        skip_incremental_outputs = False,
        objects = [],
        incremental_artifacts: IncrementalCompilationInput | None = None) -> (CompileArgsfiles | None, Artifact | None):
    build_swift_incrementally = should_build_swift_incrementally(ctx)
    explicit_modules_enabled = uses_explicit_modules(ctx)

    # The main compilation actions add a category suffix when explicit modules
    # are enabled for historical reasons. This could be removed.
    if extension and explicit_modules_enabled:
        category += "_with_explicit_mods"

    # If we prefer to execute locally (e.g., for perf reasons), ensure we
    # upload to the cache so that CI builds populate caches used by developer
    # machines.
    allow_cache_upload = True
    local_only = False

    # Swift compilation on RE without explicit modules is impractically
    # expensive because there's no shared module cache across different
    # libraries.
    prefer_local = not explicit_modules_enabled

    if (not cacheable) or (build_swift_incrementally and not toolchain.supports_relative_resource_dir):
        # When Swift code is built incrementally, the swift-driver embeds
        # absolute paths into the artifacts without relative resource dir
        # support. In this case we can only build locally.
        allow_cache_upload = False
        local_only = True
        prefer_local = False
    elif build_swift_incrementally and not get_incremental_file_hashing_enabled(ctx) and _IS_USER_BUILD:
        # Swift incremental compilation requires the swiftdep files which are
        # only present when compiling locally. Prefer local unless otherwise
        # overridden.
        prefer_local = True

    argsfile, output_file_map = compile_with_argsfile(
        ctx = ctx,
        category = category,
        shared_flags = shared_flags,
        srcs = srcs,
        additional_flags = additional_flags,
        toolchain = toolchain,
        num_threads = num_threads,
        dep_files = dep_files,
        output_file_map = output_file_map,
        allow_cache_upload = allow_cache_upload,
        local_only = local_only,
        prefer_local = prefer_local,
        # We need to preserve the action outputs for incremental compilation.
        no_outputs_cleanup = build_swift_incrementally,
        # Skip incremental outputs requires an empty output file map, so is not
        # compatible with serialized diagnostics.
        supports_serialized_errors = not skip_incremental_outputs,
        skip_incremental_outputs = skip_incremental_outputs,
        objects = objects,
        incremental_artifacts = incremental_artifacts,
    )

    if extension:
        # Swift correctly handles relative paths and we can utilize the relative argsfile for Xcode.
        return CompileArgsfiles(relative = {extension: argsfile}, xcode = {extension: argsfile}), output_file_map
    else:
        return None, output_file_map

def _get_serialize_debugging_options(ctx: AnalysisContext, uses_explicit_modules: bool):
    if ctx.attrs.serialize_debugging_options == False:
        return False

    if uses_explicit_modules:
        # If the toolchain supports explicit modules we can
        # enable, regardless if this is a mixed library or not
        return get_swift_toolchain_info(ctx).supports_explicit_module_debug_serialization

    return True

def _get_shared_flags(
        ctx: AnalysisContext,
        deps_providers: list,
        parse_as_library: bool,
        private_module: SwiftCompiledModuleInfo | None,
        public_module: SwiftCompiledModuleInfo | None,
        module_name: str,
        private_modulemap_pp_info: CPreprocessor | None,
        public_modulemap_pp_info: CPreprocessor | None,
        extra_search_paths_flags: list[ArgLike],
        inputs_tag: ArtifactTag,
        is_macro: bool) -> cmd_args:
    toolchain = get_swift_toolchain_info(ctx)
    cmd = cmd_args()

    if not toolchain.supports_relative_resource_dir:
        # Setting this to empty will get the driver to make all paths absolute when
        # passed to the frontend. We later debug prefix these to ensure relative paths
        # in the debug info.
        cmd.add(["-working-directory="])

    cmd.add(get_sdk_flags(ctx))
    cmd.add(_get_target_flags(ctx))
    cmd.add([
        # Always use color, consistent with clang.
        "-color-diagnostics",
        # Unset the working directory in the debug information.
        "-file-compilation-dir",
        ".",
        "-module-name",
        module_name,
        "-Xfrontend",
        "-enable-cross-import-overlays",
        "-Xfrontend",
        "-disable-cxx-interop-requirement-at-import",
        "-Xfrontend",
        "-emit-clang-header-nonmodular-includes",
        # RE actions are run in a sandbox, which will fail to run macros as
        # you cannot nest sandbox actions.
        # https://github.com/swiftlang/swift/pull/70079
        "-disable-sandbox",
    ])

    if parse_as_library:
        cmd.add([
            "-parse-as-library",
        ])

    if ctx.attrs.swift_package_name != None:
        cmd.add([
            "-package-name",
            ctx.attrs.swift_package_name,
        ])

    explicit_modules_enabled = uses_explicit_modules(ctx)
    if explicit_modules_enabled:
        cmd.add([
            "-Xcc",
            "-Xclang",
            "-Xcc",
            # We set -fmodule-file-home-is-cwd as this is used to correctly
            # set the working directory of modules when generating debug info.
            "-fmodule-file-home-is-cwd",
            "-Xcc",
            "-Xclang",
            "-Xcc",
            # This is the default for compilation, but not in sourcekitd.
            # Set it explicitly here so that indexing will not fail with
            # invalid module format errors.
            "-fmodule-format=obj",
        ])
        cmd.add(get_disable_pch_validation_flags())

    if toolchain.resource_dir:
        cmd.add([
            "-resource-dir",
            toolchain.resource_dir,
        ])

    if ctx.attrs.swift_version:
        cmd.add(["-swift-version", ctx.attrs.swift_version])
        swift_version = ctx.attrs.swift_version
    else:
        # Swift compiler defaults to 5 and therefore so do we
        # use the version 5 for upcoming features passed to tools
        # like the ide-tool for swift-interface generation below
        # include/swift/Basic/LangOptions.h?lines=175-176
        swift_version = SwiftVersion[0]  # "5"

    if ctx.attrs.enable_cxx_interop:
        cmd.add(["-cxx-interoperability-mode=default"])

    if _get_serialize_debugging_options(ctx, explicit_modules_enabled):
        cmd.add([
            "-Xfrontend",
            "-serialize-debugging-options",
            "-Xfrontend",
            "-prefix-serialized-debugging-options",
        ])
    else:
        cmd.add([
            "-Xfrontend",
            "-no-serialize-debugging-options",
        ])

    upcoming_features = toolchain.swift_upcoming_features[swift_version]
    experimental_features = toolchain.swift_experimental_features[swift_version]

    for feature in upcoming_features:
        cmd.add([
            "-Xfrontend",
            "-enable-upcoming-feature",
            "-Xfrontend",
            feature,
        ])

    for feature in experimental_features:
        cmd.add([
            "-Xfrontend",
            "-enable-experimental-feature",
            "-Xfrontend",
            feature,
        ])

    if ctx.attrs._swift_enable_testing:
        cmd.add("-enable-testing")

    if getattr(ctx.attrs, "application_extension", False):
        cmd.add("-application-extension")

    # Only apple_library has swift_macro_deps
    swift_macros = getattr(ctx.attrs, "swift_macro_deps", [])
    if swift_macros:
        for m in ctx.plugins[SwiftMacroPlugin]:
            macro_artifact = m[DefaultInfo].default_outputs[0]
            cmd.add([
                "-load-plugin-executable",
                cmd_args(inputs_tag.tag_artifacts(macro_artifact), format = "{}#" + macro_artifact.basename),
            ])

    pcm_deps_tset = get_compiled_pcm_deps_tset(ctx, deps_providers)

    # If Swift Explicit Modules are enabled, a few things must be provided to a compilation job:
    # 1. Direct and transitive SDK deps from `sdk_modules` attribute.
    # 2. Direct and transitive user-defined deps.
    # 3. Transitive SDK deps of user-defined deps.
    # (This is the case, when a user-defined dep exports a type from SDK module,
    # thus such SDK module should be implicitly visible to consumers of that custom dep)
    if uses_explicit_modules(ctx):
        sdk_clang_deps_tset = get_compiled_sdk_clang_deps_tset(ctx, deps_providers)
        sdk_swift_deps_tset = get_compiled_sdk_swift_deps_tset(ctx, deps_providers)
        _add_swift_module_map_args(
            ctx = ctx,
            sdk_swiftmodule_deps_tset = sdk_swift_deps_tset,
            pcm_deps_tset = pcm_deps_tset,
            sdk_deps_tset = sdk_clang_deps_tset,
            inputs_tag = inputs_tag,
            cmd = cmd,
            is_macro = is_macro,
        )

    _add_clang_deps_flags(ctx, pcm_deps_tset, cmd, inputs_tag)
    _add_swift_deps_flags(ctx, cmd, is_macro)

    # Add flags for importing the ObjC part of this library
    _add_mixed_library_flags_to_cmd(
        ctx,
        cmd,
        private_module,
        public_module,
        private_modulemap_pp_info,
        public_modulemap_pp_info,
    )

    # Add toolchain and target flags last to allow for overriding defaults
    cmd.add(toolchain.compiler_flags)
    cmd.add(ctx.attrs.swift_compiler_flags)
    cmd.add(extra_search_paths_flags)

    return cmd

def _add_swift_module_map_args(
        ctx: AnalysisContext,
        sdk_swiftmodule_deps_tset: SwiftCompiledModuleTset,
        pcm_deps_tset: SwiftCompiledModuleTset,
        sdk_deps_tset: SwiftCompiledModuleTset,
        inputs_tag: ArtifactTag,
        cmd: cmd_args,
        is_macro: bool):
    module_name = get_module_name(ctx)
    sdk_swiftmodule_deps_tset = [sdk_swiftmodule_deps_tset] if sdk_swiftmodule_deps_tset else []
    all_deps_tset = ctx.actions.tset(
        SwiftCompiledModuleTset,
        children = _get_swift_paths_tsets(is_macro, ctx.attrs.deps + getattr(ctx.attrs, "exported_deps", [])) + [pcm_deps_tset, sdk_deps_tset] + sdk_swiftmodule_deps_tset,
    )
    swift_module_map_artifact = write_swift_module_map_with_deps(
        ctx,
        module_name,
        all_deps_tset,
    )
    cmd.add([
        "-Xfrontend",
        "-explicit-swift-module-map-file",
        "-Xfrontend",
        inputs_tag.tag_artifacts(swift_module_map_artifact),
    ])

def _add_swift_deps_flags(
        ctx: AnalysisContext,
        cmd: cmd_args,
        is_macro: bool):
    if uses_explicit_modules(ctx):
        cmd.add([
            "-Xcc",
            "-fno-implicit-modules",
            "-Xcc",
            "-fno-implicit-module-maps",
            "-Xfrontend",
            "-disable-implicit-swift-modules",
        ])
    else:
        depset = ctx.actions.tset(SwiftCompiledModuleTset, children = _get_swift_paths_tsets(is_macro, ctx.attrs.deps + getattr(ctx.attrs, "exported_deps", [])))
        cmd.add(depset.project_as_args("module_search_path"))

        implicit_search_path_tset = get_implicit_framework_search_path_providers(
            ctx,
            None,
            ctx.attrs.deps + getattr(ctx.attrs, "exported_deps", []),
        )
        cmd.add(implicit_search_path_tset.project_as_args("swift_framework_implicit_search_paths_args"))

def _add_clang_deps_flags(
        ctx: AnalysisContext,
        pcm_deps_tset: SwiftCompiledModuleTset,
        cmd: cmd_args,
        inputs_tag: ArtifactTag) -> None:
    if uses_explicit_modules(ctx):
        clang_flags = pcm_deps_tset.project_as_args("clang_importer_flags")
        cmd.add(inputs_tag.tag_artifacts(clang_flags))
    else:
        inherited_preprocessor_infos = cxx_inherited_preprocessor_infos(ctx.attrs.deps + getattr(ctx.attrs, "exported_deps", []))
        preprocessors = cxx_merge_cpreprocessors(ctx, [], inherited_preprocessor_infos)
        cmd.add(cmd_args(preprocessors.set.project_as_args("args"), prepend = "-Xcc"))
        cmd.add(cmd_args(preprocessors.set.project_as_args("modular_args"), prepend = "-Xcc"))
        cmd.add(cmd_args(preprocessors.set.project_as_args("include_dirs"), prepend = "-Xcc"))

def _add_mixed_library_flags_to_cmd(
        ctx: AnalysisContext,
        cmd: cmd_args,
        private_module: SwiftCompiledModuleInfo | None,
        underlying_module: SwiftCompiledModuleInfo | None,
        private_modulemap_pp_info: CPreprocessor | None,
        public_modulemap_pp_info: CPreprocessor | None) -> None:
    if uses_explicit_modules(ctx):
        if private_module:
            cmd.add(private_module.clang_importer_args)
            cmd.add(private_module.clang_module_file_args)

        if underlying_module:
            cmd.add(underlying_module.clang_importer_args)
            cmd.add(underlying_module.clang_module_file_args)
            cmd.add("-import-underlying-module")
        return

    for objc_modulemap_pp_info in filter(None, [private_modulemap_pp_info, public_modulemap_pp_info]):
        # TODO(T99100029): We cannot use VFS overlays to mask this import from
        # the debugger as they require absolute paths. Instead we will enforce
        # that mixed libraries do not have serialized debugging info and rely on
        # rdeps to serialize the correct paths.
        for arg in objc_modulemap_pp_info.args.args:
            cmd.add("-Xcc")
            cmd.add(arg)

        for arg in objc_modulemap_pp_info.modular_args:
            cmd.add("-Xcc")
            cmd.add(arg)

    if public_modulemap_pp_info:
        cmd.add("-import-underlying-module")

def _get_swift_dependency_info(is_macro: bool, deps: list[Dependency]) -> list[SwiftDependencyInfo]:
    ret = []
    for d in deps:
        if SwiftDependencyInfo in d:
            if d[SwiftDependencyInfo].is_macro != is_macro:
                fail("Cannot have a {} dep in a {} target".format(
                    "macro" if d[SwiftDependencyInfo].is_macro else "non-macro",
                    "macro" if is_macro else "non-macro",
                ))
            ret.append(d[SwiftDependencyInfo])

    return ret

def _get_swift_paths_tsets(is_macro: bool, deps: list[Dependency]) -> list[SwiftCompiledModuleTset]:
    return [d.exported_swiftmodules for d in _get_swift_dependency_info(is_macro, deps)]

def get_external_debug_info_tsets(is_macro: bool, deps: list[Dependency]) -> list[ArtifactTSet]:
    return [d.debug_info_tset for d in _get_swift_dependency_info(is_macro, deps)]

def get_swift_pcm_uncompile_info(
        ctx: AnalysisContext,
        propagated_exported_preprocessor_info: [CPreprocessorInfo, None],
        exported_pre: [CPreprocessor, None]) -> [SwiftPCMUncompiledInfo, None]:
    swift_toolchain = get_swift_toolchain_info(ctx)

    if is_sdk_modules_provided(swift_toolchain):
        propagated_pp_args_cmd = cmd_args(propagated_exported_preprocessor_info.set.project_as_args("args"), prepend = "-Xcc") if propagated_exported_preprocessor_info else None
        return SwiftPCMUncompiledInfo(
            name = get_module_name(ctx),
            is_transient = not ctx.attrs.modular or not exported_pre,
            exported_preprocessor = exported_pre,
            exported_deps = _exported_deps(ctx),
            propagated_preprocessor_args_cmd = propagated_pp_args_cmd,
            uncompiled_sdk_modules = ctx.attrs.sdk_modules,
            modulemap_artifacts = exported_pre.modulemap_artifacts if exported_pre else [],
        )
    return None

def create_swift_dependency_info(
        ctx: AnalysisContext,
        deps,
        deps_providers: list,
        compiled_info: [SwiftCompiledModuleInfo, None],
        debug_info_tset: ArtifactTSet,
        is_macro: bool):
    # We pass through the SDK swiftmodules here to match Buck 1 behaviour. This is
    # pretty loose, but it matches Buck 1 behavior so cannot be improved until
    # migration is complete.
    transitive_swiftmodule_deps = _get_swift_paths_tsets(is_macro, deps) + [get_compiled_sdk_swift_deps_tset(ctx, deps_providers)]

    if compiled_info:
        exported_swiftmodules = ctx.actions.tset(SwiftCompiledModuleTset, value = compiled_info, children = transitive_swiftmodule_deps)
    else:
        exported_swiftmodules = ctx.actions.tset(SwiftCompiledModuleTset, children = transitive_swiftmodule_deps)

    return SwiftDependencyInfo(
        debug_info_tset = debug_info_tset,
        exported_swiftmodules = exported_swiftmodules,
        is_macro = is_macro,
    )

def get_swift_dependency_info(
        ctx: AnalysisContext,
        output_module: Artifact | None,
        output_interface: Artifact | None,
        deps_providers: list,
        is_macro: bool) -> SwiftDependencyInfo:
    exported_deps = _exported_deps(ctx)

    if output_module:
        compiled_info = SwiftCompiledModuleInfo(
            is_framework = False,
            is_sdk_module = False,
            is_swiftmodule = True,
            module_name = get_module_name(ctx),
            output_artifact = output_module,
            interface_artifact = output_interface,
        )
    else:
        compiled_info = None

    debug_info_tset = make_artifact_tset(
        actions = ctx.actions,
        artifacts = filter(None, [output_module, output_interface]),
        children = get_external_debug_info_tsets(is_macro, ctx.attrs.deps + getattr(ctx.attrs, "exported_deps", [])),
        label = ctx.label,
        tags = [ArtifactInfoTag("swiftmodule")],
    )

    return create_swift_dependency_info(
        ctx,
        exported_deps,
        deps_providers,
        compiled_info,
        debug_info_tset,
        is_macro,
    )

def get_swiftmodule_linkable(swift_compile_output: [SwiftCompilationOutput, None]) -> [SwiftmoduleLinkable, None]:
    return SwiftmoduleLinkable(swiftmodules = swift_compile_output.swift_debug_info) if swift_compile_output else None

def extract_swiftmodule_linkables(link_infos: [list[LinkInfo], None]) -> list[SwiftmoduleLinkable]:
    linkables = []
    for info in link_infos:
        for linkable in info.linkables:
            if isinstance(linkable, SwiftmoduleLinkable):
                linkables.append(linkable)

    return linkables

def get_swiftmodule_linker_flags(ctx: AnalysisContext, swiftmodule_linkable: [SwiftmoduleLinkable, None]) -> cmd_args:
    if swiftmodule_linkable:
        tset = swiftmodule_linkable.swiftmodules
        artifacts = project_artifacts(
            actions = ctx.actions,
            tsets = [tset],
        )
        return cmd_args([cmd_args(swiftmodule, format = "-Wl,-add_ast_path,{}") for swiftmodule in artifacts])
    return cmd_args()

def _get_xctest_swiftmodule_search_path(ctx: AnalysisContext) -> cmd_args:
    # With explicit modules we don't need to search at all.
    if uses_explicit_modules(ctx):
        return cmd_args()

    for fw in ctx.attrs.frameworks:
        if fw.endswith("XCTest.framework"):
            toolchain = ctx.attrs._apple_toolchain[AppleToolchainInfo]
            return cmd_args(toolchain.platform_path, format = "-I{}/Developer/usr/lib")

    return cmd_args()

def get_swift_debug_infos(
        ctx: AnalysisContext,
        swift_dependency_info: [SwiftDependencyInfo, None],
        swift_output: [SwiftCompilationOutput, None]) -> SwiftDebugInfo:
    # When determining the debug info for shared libraries, if the shared library is a link group, we rely on the link group links to
    # obtain the debug info for linked libraries and only need to provide any swift debug info for this library itself. Otherwise
    # if linking standard shared, we need to obtain the transitive debug info.
    if get_link_group(ctx):
        swift_shared_debug_info = []
    else:
        swift_shared_debug_info = _get_swift_shared_debug_info(swift_dependency_info) if swift_dependency_info else []

    clang_debug_info = [swift_output.clang_debug_info] if swift_output else []
    swift_debug_info = [swift_output.swift_debug_info] if swift_output else []

    return SwiftDebugInfo(
        static = clang_debug_info + swift_debug_info,
        shared = swift_shared_debug_info + clang_debug_info + swift_debug_info,
    )

def _get_swift_shared_debug_info(swift_dependency_info: SwiftDependencyInfo) -> list[ArtifactTSet]:
    return [swift_dependency_info.debug_info_tset] if swift_dependency_info.debug_info_tset else []

def _get_project_root_file(ctx) -> Artifact:
    content = cmd_args(ctx.label.project_root)
    return ctx.actions.write("project_root_file", content, absolute = True)

def _create_compilation_database(
        ctx: AnalysisContext,
        srcs: list[CxxSrcWithFlags],
        argfile: CompileArgsfile) -> SwiftCompilationDatabase:
    module_name = get_module_name(ctx)

    swift_toolchain = get_swift_toolchain_info(ctx)
    mk_comp_db = swift_toolchain.mk_swift_comp_db

    identifier = module_name + ".swift_comp_db.json"
    cdb_artifact = ctx.actions.declare_output(identifier)
    cmd = cmd_args(mk_comp_db)
    cmd.add(cmd_args(cdb_artifact.as_output(), format = "--output={}"))
    cmd.add(cmd_args(_get_project_root_file(ctx), format = "--project-root-file={}"))
    cmd.add(["--files"] + [s.file for s in srcs])

    cmd.add("--")
    cmd.add(argfile.cmd_form)
    cmd.add([s.file for s in srcs])
    ctx.actions.run(
        cmd,
        category = "swift_compilation_database",
        identifier = identifier,
        error_handler = apple_build_error_handler,
    )

    return SwiftCompilationDatabase(db = cdb_artifact, other_outputs = argfile.cmd_form)

def _create_swift_interface(ctx: AnalysisContext, shared_flags: cmd_args, module_name: str) -> DefaultInfo:
    swift_toolchain = get_swift_toolchain_info(ctx)
    swift_ide_test_tool = swift_toolchain.swift_ide_test_tool
    if not swift_ide_test_tool:
        return DefaultInfo()
    mk_swift_interface = swift_toolchain.mk_swift_interface

    identifier = module_name + ".swift_interface"

    argsfile, _ = ctx.actions.write(
        identifier + "_argsfile",
        shared_flags,
        allow_args = True,
    )
    interface_artifact = ctx.actions.declare_output(identifier)

    mk_swift_args = cmd_args(
        mk_swift_interface,
        "--swift-ide-test-tool",
        swift_ide_test_tool,
        "--module",
        module_name,
        "--out",
        interface_artifact.as_output(),
        "--",
        cmd_args(cmd_args(argsfile, format = "@{}", delimiter = ""), hidden = [shared_flags]),
    )

    ctx.actions.run(
        mk_swift_args,
        category = "mk_swift_interface",
        identifier = identifier,
        error_handler = apple_build_error_handler,
    )

    return DefaultInfo(
        default_output = interface_artifact,
        other_outputs = [
            argsfile,
        ],
    )

def _exported_deps(ctx) -> list[Dependency]:
    if ctx.attrs.reexport_all_header_dependencies:
        return getattr(ctx.attrs, "exported_deps", []) + ctx.attrs.deps
    else:
        return getattr(ctx.attrs, "exported_deps", [])

def _should_compile_with_evolution(ctx) -> bool:
    if ctx.attrs.enable_library_evolution != None:
        return ctx.attrs.enable_library_evolution
    return ctx.attrs._enable_library_evolution
