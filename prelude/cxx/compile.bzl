# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load(
    "@prelude//cxx:compile_types.bzl",
    "AsmExtensions",
    "CxxCompileCommand",
    "CxxCompileCommandOutput",
    "CxxCompileFlavor",
    "CxxCompileOutput",
    "CxxExtension",
    "CxxSrcCompileCommand",
    "CxxSrcPrecompileCommand",
    "DepFileType",
    "HeaderExtension",
    "HeadersDepFiles",
)
load("@prelude//cxx:cuda.bzl", "CudaCompileInfo", "cuda_compile")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//cxx:cxx_utility.bzl", "cxx_attrs_get_allow_cache_upload")
load(
    "@prelude//ide_integrations/xcode:argsfiles.bzl",
    "XCODE_ARG_SUBSTITUTIONS",
)
load("@prelude//linking:lto.bzl", "LtoMode")
load(
    "@prelude//utils:utils.bzl",
    "flatten",
)
load(":argsfiles.bzl", "CompileArgsfile", "CompileArgsfiles")
load(":attr_selection.bzl", "cxx_by_language_ext")
load(
    ":compiler.bzl",
    "get_flags_for_colorful_output",
    "get_flags_for_reproducible_build",
    "get_headers_dep_files_flags_factory",
    "get_output_flags",
    "get_pic_flags",
)
load(":cxx_context.bzl", "get_cxx_toolchain_info")
load(":cxx_sources.bzl", "CxxSrcWithFlags")
load(":cxx_toolchain_types.bzl", "CxxObjectFormat", "DepTrackingMode")
load(":cxx_types.bzl", "CxxRuleConstructorParams")
load(":debug.bzl", "SplitDebugMode")
load(
    ":headers.bzl",
    "CHeader",
    "CPrecompiledHeaderInfo",
    "add_headers_dep_files",
)
load(":platform.bzl", "cxx_by_platform")
load(
    ":preprocessor.bzl",
    "CPreprocessor",  # @unused Used as a type
    "CPreprocessorInfo",  # @unused Used as a type
    "HeaderUnit",  # @unused Used as a type
    "cxx_merge_cpreprocessors",
    "get_flags_for_compiler_type",
)

def get_source_extension_for_header(header_extension: str, default: CxxExtension) -> CxxExtension:
    """
    Which source file extension to use to get compiler flags for the header.
    """
    if header_extension in (".hpp", ".hh", ".h++", ".hxx"):
        return CxxExtension(".cpp")
    elif header_extension == ".cuh":
        return CxxExtension(".cu")
    elif header_extension not in HeaderExtension.values():
        return CxxExtension(header_extension)  # a file in `headers` has a source extension
    else:
        return default

def get_source_extension(src: CxxSrcWithFlags, default_for_headers: CxxExtension) -> CxxExtension:
    """
    Which source files extension to use for a source or a header file. We want
    headers to appear as though they are source files.
    """
    if src.is_header:
        return get_source_extension_for_header(src.file.extension, default_for_headers)
    else:
        return CxxExtension(src.file.extension)

def collect_extensions(srcs: list[CxxSrcWithFlags]) -> set[CxxExtension]:
    """
    Collect extensions of source files while doing light normalization.
    """

    duplicates = {
        ".c++": ".cpp",
        ".cc": ".cpp",
        ".cxx": ".cpp",
    }

    extensions = set([CxxExtension(duplicates.get(src.file.extension, src.file.extension)) for src in srcs])
    return extensions

def default_source_extension_for_plain_header(rule_type: str) -> CxxExtension:
    """
    Returns default source file extension to use to get get compiler flags for plain .h headers.
    """

    # Default to (Objective-)C++ instead of plain (Objective-)C as it is more likely to be compatible with both.
    return CxxExtension(".mm") if rule_type.startswith("apple_") else CxxExtension(".cpp")

def detect_source_extension_for_plain_headers(exts: set[CxxExtension], rule_type: str) -> CxxExtension:
    """
    For a given list source files determine which source file extension
    to use to get compiler flags for plain .h headers.
    """

    # Assembly doesn't need any special handling as included files tend to have .asm extension themselves.
    # And the presence of assembly in the target doesn't tell us anything about the language of .h files.
    for asm_ext in AsmExtensions:
        exts.discard(asm_ext)

    if len(exts) == 0:
        return default_source_extension_for_plain_header(rule_type)

    if len(exts) == 1:
        return exts.pop()
    if CxxExtension(".hip") in exts:
        return CxxExtension(".hip")
    if CxxExtension(".cu") in exts:
        return CxxExtension(".cu")
    if CxxExtension(".mm") in exts:
        return CxxExtension(".mm")
    if CxxExtension(".cpp") in exts and CxxExtension(".m") in exts:
        return CxxExtension(".mm")
    if CxxExtension(".cpp") in exts:
        return CxxExtension(".cpp")
    if CxxExtension(".m") in exts:
        return CxxExtension(".m")
    return CxxExtension(".c")

def collect_source_extensions(
        srcs: list[CxxSrcWithFlags],
        default_for_headers: CxxExtension) -> set[CxxExtension]:
    """
    Return unique source extensions from a list of source and header files where
    header extensions are mapped to corresponding source extensions.
    """
    source_extensions = set([get_source_extension(src, default_for_headers) for src in srcs])
    return source_extensions

def get_header_language_mode(source_extension: CxxExtension) -> str | None:
    """
    Returns the header mode to use for plain .h headers based on the
    source file extension used to obtain the compiler flags for them.
    """

    # Note: CUDA doesn't have its own header language mode, but the headers have distinct .cuh extension.
    modes = {
        CxxExtension(".cpp"): "c++-header",
        CxxExtension(".m"): "objective-c-header",
        CxxExtension(".mm"): "objective-c++-header",
    }
    return modes.get(source_extension)

def create_compile_cmds(
        ctx: AnalysisContext,
        impl_params: CxxRuleConstructorParams,
        own_preprocessors: list[CPreprocessor],
        inherited_preprocessor_infos: list[CPreprocessorInfo],
        add_coverage_instrumentation_compiler_flags: bool) -> CxxCompileCommandOutput:
    """
    Forms the CxxSrcCompileCommand to use for each source file based on it's extension
    and optional source file flags. Returns CxxCompileCommandOutput containing an array
    of the generated compile commands and argsfile output.
    """

    srcs_extensions = collect_extensions(impl_params.srcs)
    extension_for_plain_headers = detect_source_extension_for_plain_headers(srcs_extensions, impl_params.rule_type)

    srcs_with_flags = []  # type: [CxxSrcWithFlags]

    for src in impl_params.srcs:
        srcs_with_flags.append(src)

    # Some targets have .cpp files in their `headers` lists, see D46195628
    # todo: should this be prohibited or expanded to allow all source extensions?
    artifact_extensions = HeaderExtension.values() + [".cpp"]
    all_headers = flatten([x.headers for x in own_preprocessors])
    for header in all_headers:
        if header.artifact.extension in artifact_extensions:
            srcs_with_flags.append(CxxSrcWithFlags(file = header.artifact, is_header = True))

    all_raw_headers = flatten([x.raw_headers for x in own_preprocessors])
    for header in all_raw_headers:
        if header.extension in HeaderExtension.values():
            srcs_with_flags.append(CxxSrcWithFlags(file = header, is_header = True))

    if len(srcs_with_flags) == 0:
        return CxxCompileCommandOutput()

    # TODO(T110378129): Buck v1 validates *all* headers used by a compilation
    # at compile time, but that doing that here/eagerly might be expensive (but
    # we should figure out something).
    _validate_target_headers(ctx, own_preprocessors)

    # Combine all preprocessor info and prepare it for compilations.
    pre = cxx_merge_cpreprocessors(
        ctx,
        filter(None, own_preprocessors + impl_params.extra_preprocessors),
        inherited_preprocessor_infos,
    )

    headers_tag = ctx.actions.artifact_tag()

    src_compile_cmds = []
    hdr_compile_cmds = []
    cxx_compile_cmd_by_ext = {}  # type: dict[CxxExtension, CxxCompileCommand]
    argsfile_by_ext = {}  # type: dict[str, CompileArgsfile]
    xcode_argsfile_by_ext = {}  # type: dict[str, CompileArgsfile]

    src_extensions = collect_source_extensions(srcs_with_flags, extension_for_plain_headers)

    # Deduplicate shared arguments to save memory. If we compile multiple files
    # of the same extension they will have some of the same flags. Save on
    # allocations by caching and reusing these objects.
    for ext in src_extensions:
        cmd = _generate_base_compile_command(ctx, impl_params, pre, headers_tag, ext)
        cxx_compile_cmd_by_ext[ext] = cmd
        argsfile_by_ext[ext.value] = cmd.argsfile
        xcode_argsfile_by_ext[ext.value] = cmd.xcode_argsfile

    for src in srcs_with_flags:
        src_args = []
        src_args.extend(src.flags)

        ext = get_source_extension(src, extension_for_plain_headers)
        cxx_compile_cmd = cxx_compile_cmd_by_ext[ext]

        if add_coverage_instrumentation_compiler_flags and cxx_compile_cmd.compiler_type != "gcc":
            src_args.extend(ctx.attrs.coverage_instrumentation_compiler_flags)

        if src.is_header:
            if cxx_compile_cmd.compiler_type in ["clang", "clang_windows", "gcc"]:
                language_mode = get_header_language_mode(ext)
                src_args.extend(["-x", language_mode] if language_mode else [])
            elif cxx_compile_cmd.compiler_type in ["clang_cl", "windows", "windows_ml64"] and ext == CxxExtension(".cpp"):
                src_args.append("/TP")

        if cxx_compile_cmd.compiler_type != "nasm":
            src_args.append("-c")
        src_args.append(src.file)

        src_compile_command = CxxSrcCompileCommand(
            src = src.file,
            cxx_compile_cmd = cxx_compile_cmd,
            args = src_args,
            index = src.index,
            is_header = src.is_header,
            index_store_factory = impl_params.index_store_factory,
            error_handler = impl_params.error_handler,
        )

        if src.is_header:
            hdr_compile_cmds.append(src_compile_command)
        else:
            src_compile_cmds.append(src_compile_command)

    argsfile_by_ext.update(impl_params.additional.argsfiles.relative)
    xcode_argsfile_by_ext.update(impl_params.additional.argsfiles.xcode)

    return CxxCompileCommandOutput(
        src_compile_cmds = src_compile_cmds,
        base_compile_cmds = cxx_compile_cmd_by_ext,
        argsfiles = CompileArgsfiles(
            relative = argsfile_by_ext,
            xcode = xcode_argsfile_by_ext,
        ),
        comp_db_compile_cmds = src_compile_cmds + hdr_compile_cmds,
    )

def _compile_index_store(ctx: AnalysisContext, src_compile_cmd: CxxSrcCompileCommand, toolchain: CxxToolchainInfo, compile_cmd: cmd_args) -> Artifact | None:
    if src_compile_cmd.index_store_factory:
        return src_compile_cmd.index_store_factory(ctx, src_compile_cmd, toolchain, compile_cmd)
    return None

COMMON_PREPROCESSOR_OUTPUT_ARGS = cmd_args("-E", "-dD")

def _compile_single_cxx(
        ctx: AnalysisContext,
        toolchain: CxxToolchainInfo,
        default_object_format: CxxObjectFormat,
        bitcode_args: list,
        optimization_flags: list,
        src_compile_cmd: CxxSrcCompileCommand,
        pic: bool,
        provide_syntax_only: bool,
        use_header_units: bool) -> CxxCompileOutput:
    """
    Construct a final compile command for a single CXX source based on
    `src_compile_command` and other compilation options.
    """

    short_path = src_compile_cmd.src.short_path
    if src_compile_cmd.index != None:
        # Add a unique postfix if we have duplicate source files with different flags
        short_path = short_path + "_" + str(src_compile_cmd.index)

    filename_base = short_path + (".pic" if pic else "")
    identifier = short_path + (" (pic)" if pic else "")

    if optimization_flags:
        identifier += " (optimized) "

    filename_base = filename_base + (".optimized" if optimization_flags else "")
    folder_name = "__objects__"
    object = ctx.actions.declare_output(
        folder_name,
        "{}.{}".format(filename_base, toolchain.linker_info.object_file_extension),
    )

    compiler_type = src_compile_cmd.cxx_compile_cmd.compiler_type

    # For distributed NVCC compilation we will bind the object in the
    # cuda_compile function.
    output_args = None if src_compile_cmd.src.extension == ".cu" else get_output_flags(compiler_type, object)
    cmd = _get_base_compile_cmd(
        bitcode_args = bitcode_args,
        src_compile_cmd = src_compile_cmd,
        pic = pic,
        use_header_units = use_header_units,
        output_args = output_args,
    )
    cmd.add(optimization_flags)

    action_dep_files = {}

    headers_dep_files = src_compile_cmd.cxx_compile_cmd.headers_dep_files

    # Distributed NVCC compilation doesn't support dep files because we'll
    # dryrun cmd and the dep files won't be materialized.
    # TODO (T219249723): investigate if dep files are needed for dist nvcc.
    if headers_dep_files and src_compile_cmd.src.extension != ".cu":
        cmd = add_headers_dep_files(
            ctx,
            cmd,
            headers_dep_files,
            src_compile_cmd.src,
            filename_base,
            action_dep_files,
        )

    clang_remarks = None
    if toolchain.clang_remarks and compiler_type == "clang":
        cmd.add(["-fsave-optimization-record", "-fdiagnostics-show-hotness", "-foptimization-record-passes=" + toolchain.clang_remarks])
        clang_remarks = ctx.actions.declare_output(
            paths.join("__objects__", "{}.opt.yaml".format(filename_base)),
        )
        cmd.add(cmd_args(hidden = clang_remarks.as_output()))

    clang_trace = None
    if toolchain.clang_trace and compiler_type == "clang":
        cmd.add(["-ftime-trace"])
        clang_trace = ctx.actions.declare_output(
            paths.join("__objects__", "{}.json".format(filename_base)),
        )
        cmd.add(cmd_args(hidden = clang_trace.as_output()))

    gcno_file = None
    if toolchain.gcno_files and src_compile_cmd.src.extension not in (".S", ".sx"):
        cmd.add(["--coverage"])
        gcno_file = ctx.actions.declare_output(
            paths.join("__objects__", "{}.gcno".format(filename_base)),
        )
        cmd.add(cmd_args(hidden = gcno_file.as_output()))

    external_debug_info = None
    extension_supports_external_debug_info = src_compile_cmd.src.extension not in (".hip")
    use_external_debug_info = getattr(ctx.attrs, "separate_debug_info", False) and toolchain.split_debug_mode == SplitDebugMode("split") and compiler_type == "clang" and extension_supports_external_debug_info
    if use_external_debug_info:
        external_debug_info = ctx.actions.declare_output(
            folder_name,
            "{}.{}".format(filename_base, "dwo"),
        )
        cmd.add(cmd_args(external_debug_info.as_output(), format = "--fbcc-create-external-debug-info={}"))

    outputs_for_error_handler = []
    serialized_diags_to_json = toolchain.binary_utilities_info.custom_tools.get("serialized-diags-to-json", None)
    if serialized_diags_to_json and src_compile_cmd.error_handler and compiler_type == "clang" and src_compile_cmd.src.extension != ".cu":
        # We need to wrap the entire compile to provide serialized diagnostics
        # output and on error convert it to JSON.
        json_error_output = ctx.actions.declare_output("__diagnostics__/{}.json".format(filename_base)).as_output()
        outputs_for_error_handler.append(json_error_output)
        cmd = cmd_args(
            toolchain.internal_tools.serialized_diagnostics_to_json_wrapper,
            serialized_diags_to_json,
            json_error_output,
            cmd,
        )

    dist_nvcc_dag = None
    dist_nvcc_env = None
    if src_compile_cmd.src.extension == ".cu":
        cuda_compile_output = cuda_compile(
            ctx,
            cmd,
            object,
            src_compile_cmd,
            CudaCompileInfo(filename = filename_base, identifier = identifier, output_prefix = folder_name),
            action_dep_files,
            allow_dep_file_cache_upload = False,
            error_handler = src_compile_cmd.error_handler,
        )
        if cuda_compile_output:
            dist_nvcc_dag, dist_nvcc_env = cuda_compile_output
    else:
        ctx.actions.run(
            cmd,
            category = src_compile_cmd.cxx_compile_cmd.category,
            identifier = identifier,
            dep_files = action_dep_files,
            allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
            allow_dep_file_cache_upload = False,
            error_handler = src_compile_cmd.error_handler,
            outputs_for_error_handler = outputs_for_error_handler,
        )

    # If we're building with split debugging, where the debug info is in the
    # original object, then add the object as external debug info
    # FIXME: ThinLTO generates debug info in a separate dwo dir, but we still
    # need to track object files if the object file is not compiled to bitcode.
    # We should track whether ThinLTO is used on a per-object basis rather than
    # globally on a toolchain level.
    object_has_external_debug_info = (
        toolchain.split_debug_mode == SplitDebugMode("single")
    )

    # .S extension is native assembly code (machine level, processor specific)
    # and clang will happily compile them to .o files, but the object are always
    # native even if we ask for bitcode.  If we don't mark the output format,
    # other tools would try and parse the .o file as LLVM-IR and fail.
    if src_compile_cmd.src.extension in [".S", ".s"]:
        object_format = CxxObjectFormat("native")
    else:
        object_format = default_object_format

    compile_index_store_cmd = _get_base_compile_cmd(
        bitcode_args = bitcode_args,
        src_compile_cmd = src_compile_cmd,
        pic = pic,
    )

    index_store = None

    if pic:
        index_store = _compile_index_store(ctx, src_compile_cmd, toolchain, compile_index_store_cmd)

    # Generate asm for compiler which accept `-S` (TODO: support others)
    if compiler_type in ["clang", "gcc"]:
        # Generate assembler or llvm bitcode output file
        assembly_extension = "s"
        if compiler_type == "clang" and object_format == CxxObjectFormat("bitcode"):
            assembly_extension = "ll"
        assembly = ctx.actions.declare_output(
            "__assembly__",
            "{}.{}".format(filename_base, assembly_extension),
        )
        assembly_cmd = _get_base_compile_cmd(
            bitcode_args = bitcode_args,
            src_compile_cmd = src_compile_cmd,
            pic = pic,
            output_args = ["-S"] + get_output_flags(compiler_type, assembly),
        )
        ctx.actions.run(
            assembly_cmd,
            category = src_compile_cmd.cxx_compile_cmd.category,
            identifier = identifier + " (assembly)",
            allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
            allow_dep_file_cache_upload = False,
            error_handler = src_compile_cmd.error_handler,
        )
    else:
        assembly = None

    if compiler_type == "clang" and provide_syntax_only:
        diagnostics = ctx.actions.declare_output(
            "__diagnostics__",
            "{}.diag.txt".format(short_path),
        )
        syntax_only_cmd = _get_base_compile_cmd(
            bitcode_args = bitcode_args,
            src_compile_cmd = src_compile_cmd,
            pic = pic,
            output_args = ["-fsyntax-only"],
        )
        ctx.actions.run(
            [
                toolchain.internal_tools.stderr_to_file,
                cmd_args(diagnostics.as_output(), format = "--out={}"),
                syntax_only_cmd,
            ],
            category = "check",
            identifier = short_path,
            allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
            allow_dep_file_cache_upload = False,
            error_handler = src_compile_cmd.error_handler,
        )
    else:
        diagnostics = None

    # Generate pre-processed sources
    preproc = ctx.actions.declare_output(
        "__preprocessed__",
        "{}.{}".format(filename_base, "i"),
    )
    preproc_cmd = _get_base_compile_cmd(bitcode_args, src_compile_cmd, pic, [COMMON_PREPROCESSOR_OUTPUT_ARGS, get_output_flags(compiler_type, preproc)])
    ctx.actions.run(
        preproc_cmd,
        category = src_compile_cmd.cxx_compile_cmd.category,
        identifier = identifier + " (preprocessor)",
        allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
        allow_dep_file_cache_upload = False,
        error_handler = src_compile_cmd.error_handler,
    )

    return CxxCompileOutput(
        object = object,
        object_format = object_format,
        object_has_external_debug_info = object_has_external_debug_info,
        external_debug_info = external_debug_info,
        clang_remarks = clang_remarks,
        clang_trace = clang_trace,
        gcno_file = gcno_file,
        index_store = index_store,
        assembly = assembly,
        diagnostics = diagnostics,
        preproc = preproc,
        nvcc_dag = dist_nvcc_dag,
        nvcc_env = dist_nvcc_env,
    )

def _get_base_compile_cmd(
        bitcode_args: cmd_args | list,
        src_compile_cmd: CxxSrcCompileCommand,
        pic: bool,
        output_args: list | None = None,
        use_header_units: bool = False) -> cmd_args:
    """
    Construct a shared compile command for a single CXX source based on
    `src_compile_command` and other compilation options.
    """
    cmd = cmd_args(src_compile_cmd.cxx_compile_cmd.base_compile_cmd)
    if output_args:
        cmd.add(output_args)

    compiler_type = src_compile_cmd.cxx_compile_cmd.compiler_type

    if pic:
        cmd.add(get_pic_flags(compiler_type))

    if use_header_units and src_compile_cmd.cxx_compile_cmd.header_units_argsfile:
        cmd.add(src_compile_cmd.cxx_compile_cmd.header_units_argsfile.cmd_form)

    cmd.add(src_compile_cmd.cxx_compile_cmd.argsfile.cmd_form)
    cmd.add(src_compile_cmd.args)

    cmd.add(bitcode_args)

    return cmd

def compile_cxx(
        ctx: AnalysisContext,
        src_compile_cmds: list[CxxSrcCompileCommand],
        flavor: CxxCompileFlavor,
        provide_syntax_only: bool,
        use_header_units: bool = False) -> list[CxxCompileOutput]:
    """
    For a given list of src_compile_cmds, generate output artifacts.
    """
    toolchain = get_cxx_toolchain_info(ctx)
    linker_info = toolchain.linker_info

    # Resolve the output format, which is a tristate of native (default being mach-o/elf/pe)
    # bitcode (being LLVM-IR, which is also produced if any link time optimization flags are
    # enabled) or the third hybrid state where the bitcode is embedded into a section of the
    # native code, allowing the file to be used as either (but at twice the size)
    default_object_format = toolchain.object_format or CxxObjectFormat("native")
    bitcode_args = []
    if linker_info.lto_mode == LtoMode("none"):
        if toolchain.object_format == CxxObjectFormat("bitcode"):
            bitcode_args.append("-emit-llvm")
            default_object_format = CxxObjectFormat("bitcode")
        elif toolchain.object_format == CxxObjectFormat("embedded-bitcode"):
            bitcode_args.append("-fembed-bitcode")
            default_object_format = CxxObjectFormat("embedded-bitcode")
    else:
        # LTO always produces bitcode object in any mode (thin, full, etc)
        default_object_format = CxxObjectFormat("bitcode")

    objects = []
    optimization_flags = toolchain.optimization_compiler_flags_EXPERIMENTAL if flavor == CxxCompileFlavor("pic_optimized") else []
    for src_compile_cmd in src_compile_cmds:
        cxx_compile_output = _compile_single_cxx(
            ctx = ctx,
            toolchain = toolchain,
            default_object_format = default_object_format,
            bitcode_args = bitcode_args,
            optimization_flags = optimization_flags,
            src_compile_cmd = src_compile_cmd,
            pic = flavor != CxxCompileFlavor("default"),
            provide_syntax_only = provide_syntax_only,
            use_header_units = use_header_units,
        )
        objects.append(cxx_compile_output)

    return objects

def _compiler_supports_header_units(compiler_info: typing.Any):
    return ("clang" in compiler_info.compiler_type and
            compiler_info.supports_two_phase_compilation)

def _get_module_name(ctx: AnalysisContext, group_name: str) -> str:
    return paths.normalize(paths.join(
        "__header_units__",
        ctx.label.package,
        "{}{}.h".format(ctx.label.name, group_name),
    ))

def _get_import_filename(ctx: AnalysisContext, group_name: str) -> str:
    return paths.normalize(paths.join(
        ctx.label.package,
        "__import__{}{}.h".format(ctx.label.name, group_name),
    ))

def _is_standalone_header(header: CHeader) -> bool:
    if header.artifact.extension not in HeaderExtension.values():
        return False
    if header.name.endswith("-inl.h") or header.name.endswith("-inl.hpp"):
        return False
    if header.name.endswith(".tcc"):
        return False
    if header.name.endswith("-pre.h") or header.name.endswith("-pre.hpp"):
        return False
    if header.name.endswith("-post.h") or header.name.endswith("-post.hpp"):
        return False
    return True

def _convert_raw_header(
        ctx: AnalysisContext,
        raw_header: Artifact,
        include_dirs: list[CellPath]) -> CHeader:
    package_prefix = str(ctx.label.path)
    ns = paths.dirname(raw_header.short_path)
    for d in include_dirs:
        abs_dir = str(d)
        if paths.starts_with(abs_dir, package_prefix):
            prefix = paths.relativize(abs_dir, package_prefix)
            if paths.starts_with(ns, prefix):
                ns = paths.relativize(ns, prefix)
                break
    return CHeader(
        artifact = raw_header,
        name = raw_header.basename,
        namespace = ns,
        named = False,
    )

def _create_precompile_cmd(
        ctx: AnalysisContext,
        compiler_info: typing.Any,
        preprocessors: list[CPreprocessor],
        header_group: str | None,
        group_name: str,
        extra_preprocessors: list[CPreprocessor],
        cmd: CxxCompileCommand) -> CxxSrcPrecompileCommand:
    include_dirs = flatten([x.include_dirs for x in preprocessors])
    converted_headers = [
        _convert_raw_header(ctx, raw_header, include_dirs)
        for raw_header in flatten([x.raw_headers for x in preprocessors])
    ]
    header_paths = [
        paths.normalize(paths.join(header.namespace, header.name))
        for header in flatten([x.headers for x in preprocessors]) + converted_headers
        if (_is_standalone_header(header) if header_group == None else regex_match(header_group, header.name))
    ]

    module_name = _get_module_name(ctx, group_name)
    import_name = _get_import_filename(ctx, group_name)
    input_header = ctx.actions.write(
        module_name,
        "",
        uses_experimental_content_based_path_hashing = True,
    )

    import_stub = ctx.actions.write(
        import_name,
        """
#ifdef FACEBOOK_CPP_HEADER_UNIT
export
#endif
import \"{}\";
""".format(module_name),
        uses_experimental_content_based_path_hashing = True,
    )

    symlinked_files = {}
    for path in header_paths:
        symlinked_files[path] = import_stub

    modulemap_content = """
module "{}" {{
  header "{}"
  export *
}}
""".format(module_name, module_name)
    modulemap_file = ctx.actions.write(
        "module.modulemap" + group_name,
        modulemap_content,
        uses_experimental_content_based_path_hashing = True,
    )

    src_dir = ctx.actions.symlinked_dir(
        "header-unit" + group_name,
        symlinked_files | {
            module_name: input_header,
            import_name: import_stub,
            "module.modulemap": modulemap_file,
        },
        uses_experimental_content_based_path_hashing = True,
    )

    args = []
    args.extend([
        "-DFACEBOOK_CPP_HEADER_UNIT=1",
        # TODO(nml): Fix warning bugs.
        "-Wno-uninitialized",
        "-Wno-conversion",
        "-Wno-zero-as-null-pointer-constant",
        "-Wno-c++98-compat-extra-semi",
    ])

    extra_argsfile = None
    if extra_preprocessors:
        extra_argsfile = _mk_header_units_argsfile(
            ctx = ctx,
            compiler_info = compiler_info,
            preprocessor = cxx_merge_cpreprocessors(ctx, extra_preprocessors, []),
            ext = CxxExtension(".cpp"),
            uses_experimental_content_based_path_hashing = True,
            filename_prefix = "export{}_".format(group_name),
        )

    include_args = cmd_args(header_paths, format = "-include{}", quote = "shell")

    headers_argsfile, _ = ctx.actions.write(
        "{}.header_unit_headers".format(group_name),
        include_args,
        allow_args = True,
        uses_experimental_content_based_path_hashing = True,
    )

    args.extend([cmd_args(headers_argsfile, format = "@{}")])
    args.extend(["-xc++-user-header", "-fmodule-header"])
    args.extend(["-fmodule-name={}".format(module_name)])
    args.extend(["-Xclang", "-fmodule-file-home-is-cwd"])
    args.extend(["-Xclang", cmd_args(input_header, format = "-fmodules-embed-file={}")])
    args.extend(["--precompile", input_header])

    return CxxSrcPrecompileCommand(
        src = src_dir,
        cxx_compile_cmd = cmd,
        args = args,
        extra_argsfile = extra_argsfile,
    )

def _precompile_single_cxx(
        ctx: AnalysisContext,
        toolchain: CxxToolchainInfo,
        impl_params: CxxRuleConstructorParams,
        group_name: str,
        src_compile_cmd: CxxSrcPrecompileCommand) -> HeaderUnit:
    identifier = src_compile_cmd.src.short_path

    filename = "{}.pcm".format(identifier)
    module = ctx.actions.declare_output(
        "__pcm_files__",
        filename,
        uses_experimental_content_based_path_hashing = True,
    )

    cmd = cmd_args(src_compile_cmd.cxx_compile_cmd.base_compile_cmd)
    if src_compile_cmd.cxx_compile_cmd.header_units_argsfile:
        cmd.add(src_compile_cmd.cxx_compile_cmd.header_units_argsfile.cmd_form)
    if src_compile_cmd.extra_argsfile:
        cmd.add(src_compile_cmd.extra_argsfile.cmd_form)
    cmd.add(src_compile_cmd.cxx_compile_cmd.argsfile.cmd_form)
    cmd.add(src_compile_cmd.args)
    cmd.add(["-o", module.as_output()])

    clang_trace = None
    if toolchain.clang_trace and toolchain.cxx_compiler_info.compiler_type == "clang":
        cmd.add(["-ftime-trace"])
        clang_trace = ctx.actions.declare_output(
            paths.join("__pcm_files__", "{}.json".format(identifier)),
            uses_experimental_content_based_path_hashing = True,
        )
        cmd.add(cmd_args(hidden = clang_trace.as_output()))

    # TODO(nml): We don't meaningfully support dep files. See T225373444.
    ctx.actions.run(
        cmd,
        category = "cxx_modules_precompile",
        identifier = identifier,
        allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
        low_pass_filter = False,
    )

    return HeaderUnit(
        name = _get_module_name(ctx, group_name),
        module = module,
        include_dir = src_compile_cmd.src,
        import_include = _get_import_filename(ctx, group_name) if impl_params.export_header_unit == "preload" else None,
        clang_trace = clang_trace,
    )

def precompile_cxx(
        ctx: AnalysisContext,
        impl_params: CxxRuleConstructorParams,
        preprocessors: list[CPreprocessor],
        header_preprocessor_info: CPreprocessorInfo) -> list[CPreprocessor]:
    """
    Produces header units for the target and returns a list of preprocessors enabling
    them; depending on those preprocessors will allow the corresponding module to load.
    """
    toolchain = get_cxx_toolchain_info(ctx)
    compiler_info = toolchain.cxx_compiler_info
    if not _compiler_supports_header_units(compiler_info):
        return []

    def mk_base_cmd():
        base_compile_cmd = _get_compile_base(toolchain, compiler_info)
        ext = CxxExtension(".cpp")
        headers_tag = ctx.actions.artifact_tag()  # Currently ignored
        argsfile = _mk_argsfiles(
            ctx,
            impl_params,
            compiler_info,
            header_preprocessor_info,
            ext,
            headers_tag,
            is_xcode_argsfile = False,
            is_precompile = True,
            uses_experimental_content_based_path_hashing = True,
            filename_prefix = "pre_",
        )
        header_units_argsfile = _mk_header_units_argsfile(
            ctx,
            compiler_info,
            header_preprocessor_info,
            ext,
            uses_experimental_content_based_path_hashing = True,
            filename_prefix = "pre_",
        )
        return CxxCompileCommand(
            base_compile_cmd = base_compile_cmd,
            argsfile = argsfile,
            xcode_argsfile = argsfile,  # Unused
            header_units_argsfile = header_units_argsfile,
            headers_dep_files = None,
            compiler_type = compiler_info.compiler_type,
            category = "cxx_modules_precompile",
            allow_cache_upload = cxx_attrs_get_allow_cache_upload(ctx.attrs, default = compiler_info.allow_cache_upload),
        )

    cmd = mk_base_cmd()

    header_unit_preprocessors = []
    if len(impl_params.export_header_unit_filter) <= 1:
        group = None
        if impl_params.export_header_unit_filter:
            group = impl_params.export_header_unit_filter[0]
        precompile_cmd = _create_precompile_cmd(
            ctx = ctx,
            compiler_info = toolchain.cxx_compiler_info,
            preprocessors = preprocessors,
            header_group = group,
            group_name = "",
            extra_preprocessors = [],
            cmd = cmd,
        )
        header_unit = _precompile_single_cxx(ctx, toolchain, impl_params, "", precompile_cmd)
        header_unit_preprocessors.append(CPreprocessor(header_units = [header_unit]))
    else:
        # Chain preprocessors in order.
        i = 0
        for header_group in impl_params.export_header_unit_filter:
            name = ".{}".format(i)
            precompile_cmd = _create_precompile_cmd(
                ctx = ctx,
                compiler_info = toolchain.cxx_compiler_info,
                preprocessors = preprocessors,
                header_group = header_group,
                group_name = name,
                extra_preprocessors = header_unit_preprocessors,
                cmd = cmd,
            )
            header_unit = _precompile_single_cxx(ctx, toolchain, impl_params, name, precompile_cmd)
            header_unit_preprocessors.append(CPreprocessor(header_units = [header_unit]))
            i += 1

    return header_unit_preprocessors

def cxx_objects_sub_targets(outs: list[CxxCompileOutput]) -> dict[str, list[Provider]]:
    objects_sub_targets = {}
    for obj in outs:
        sub_targets = {}
        if obj.clang_trace:
            sub_targets["clang-trace"] = [DefaultInfo(obj.clang_trace)]
        if obj.clang_remarks:
            sub_targets["clang-remarks"] = [DefaultInfo(obj.clang_remarks)]
        if obj.assembly:
            sub_targets["assembly"] = [DefaultInfo(obj.assembly)]
        if obj.preproc:
            sub_targets["preprocessed"] = [DefaultInfo(obj.preproc)]
        if obj.nvcc_dag:
            sub_targets["nvcc-dag"] = [DefaultInfo(obj.nvcc_dag)]
        if obj.nvcc_env:
            sub_targets["nvcc-env"] = [DefaultInfo(obj.nvcc_env)]
        objects_sub_targets[obj.object.short_path] = [DefaultInfo(
            obj.object,
            sub_targets = sub_targets,
        )]
    return objects_sub_targets

def _validate_target_headers(ctx: AnalysisContext, preprocessor: list[CPreprocessor]):
    path_to_artifact = {}
    all_headers = flatten([x.headers for x in preprocessor])
    for header in all_headers:
        header_path = paths.join(header.namespace, header.name)
        artifact = path_to_artifact.get(header_path)
        if artifact != None:
            if artifact != header.artifact:
                fail("Conflicting headers {} and {} map to {} in target {}".format(artifact, header.artifact, header_path, ctx.label))
        else:
            path_to_artifact[header_path] = header.artifact

def _get_compiler_info(toolchain: CxxToolchainInfo, ext: CxxExtension) -> typing.Any:
    compiler_info = None
    if ext.value in (".cpp", ".cc", ".cl", ".cxx", ".c++", ".h", ".hpp", ".hh", ".h++", ".hxx", ".bc"):
        compiler_info = toolchain.cxx_compiler_info
    elif ext.value == ".c":
        compiler_info = toolchain.c_compiler_info
    elif ext.value == ".m":
        compiler_info = toolchain.objc_compiler_info
    elif ext.value == ".mm":
        compiler_info = toolchain.objcxx_compiler_info
    elif ext.value in (".s", ".sx", ".S"):
        compiler_info = toolchain.as_compiler_info
    elif ext.value == ".cu":
        compiler_info = toolchain.cuda_compiler_info
    elif ext.value == ".hip":
        compiler_info = toolchain.hip_compiler_info
    elif ext.value in (".asm", ".asmpp"):
        compiler_info = toolchain.asm_compiler_info
    else:
        # This should be unreachable as long as we handle all enum values
        fail("Unknown C++ extension: " + ext.value)

    if not compiler_info:
        fail("Could not find compiler for extension `{ext}`".format(ext = ext.value))

    return compiler_info

def _get_category(ext: CxxExtension) -> str:
    if ext.value in (".cpp", ".cc", ".cl", ".cxx", ".c++", ".h", ".hpp", ".hh", ".h++", ".hxx"):
        return "cxx_compile"
    if ext.value == ".c":
        return "c_compile"
    if ext.value == ".m":
        return "objc_compile"
    if ext.value == ".mm":
        return "objcxx_compile"
    elif ext.value in (".s", ".sx", ".S", ".asm", ".asmpp"):
        return "asm_compile"
    elif ext.value == ".cu":
        return "cuda_compile"
    elif ext.value == ".hip":
        return "hip_compile"
    elif ext.value == ".bc":
        return "bitcode_compile"
    else:
        # This should be unreachable as long as we handle all enum values
        fail("Unknown extension: " + ext.value)

def _get_compile_base(toolchain: CxxToolchainInfo, compiler_info: typing.Any) -> cmd_args:
    """
    Given a compiler info returned by _get_compiler_info, form the base compile args.
    """

    if toolchain.remap_cwd and compiler_info.compiler_type in ["clang", "clang_windows", "clang_cl"]:
        return cmd_args(toolchain.internal_tools.remap_cwd, compiler_info.compiler)
    else:
        return cmd_args(compiler_info.compiler)

def _dep_file_type(ext: CxxExtension) -> [DepFileType, None]:
    # Raw assembly doesn't make sense to capture dep files for.
    # .S is preprocessed assembly, but some builds use it with
    # assemblers that don't support -MF, so leave depfiles off.
    if ext.value in (".s", ".S", ".asm"):
        return None
    elif ext.value == ".hip":
        # TODO (T118797886): HipCompilerInfo doesn't have dep files processor.
        # Should it?
        return None
    elif ext.value == ".bc":
        # Bitcode doesn't have depfiles
        return None

    # Return the file type as well
    if ext.value in (".cpp", ".cc", ".cl", ".mm", ".cxx", ".c++", ".h", ".hpp", ".hh", ".h++", ".hxx"):
        return DepFileType("cpp")
    elif ext.value in (".c", ".m"):
        return DepFileType("c")
    elif ext.value == ".cu":
        return DepFileType("cuda")
    elif ext.value in (".asmpp", ".sx"):
        return DepFileType("asm")
    else:
        # This should be unreachable as long as we handle all enum values
        fail("Unknown C++ extension: " + ext.value)

def _add_compiler_info_flags(compiler_info: typing.Any) -> list:
    cmd = []
    cmd.append(compiler_info.preprocessor_flags or [])
    cmd.append(compiler_info.compiler_flags or [])

    return cmd

def _add_compiler_type_flags(ctx: AnalysisContext, compiler_type: str, ext: CxxExtension) -> list:
    cmd = []
    cmd.append(get_flags_for_reproducible_build(ctx, compiler_type))

    if ext.value not in (".asm", ".asmpp"):
        # Clang's asm compiler doesn't support colorful output, so we skip this there.
        cmd.append(get_flags_for_colorful_output(compiler_type))

    return cmd

def _compiler_type_flags_anon_impl(ctx: AnalysisContext):
    is_nasm = ctx.attrs.compiler_type == "nasm"
    args = _add_compiler_type_flags(ctx, ctx.attrs.compiler_type, CxxExtension(ctx.attrs.src_extension))
    content = create_cmd_args(is_nasm, ctx.attrs.is_xcode_argsfile, *args)
    argsfile_artifact, _ = ctx.actions.write("compiler_type_args", content, allow_args = True)

    return [DefaultInfo(default_outputs = [argsfile_artifact])]

_compiler_type_flags_anon_rule = anon_rule(
    impl = _compiler_type_flags_anon_impl,
    attrs = {
        "compiler_type": attrs.string(doc = "The compiler type. Examples: clang, gcc, nasm"),
        "is_xcode_argsfile": attrs.bool(doc = "Apply xcode specific formatting to the argsfile."),
        "src_extension": attrs.string(doc = "The extension of the source file being compiled. See `CxxExtension` enum."),
    },
    artifact_promise_mappings = {
        "argsfile": lambda x: x[DefaultInfo].default_outputs[0],
    },
    doc = "Creates compiler flags argsfile for a given compiler type. " +
          "The argsfile is shared between targets, thus reducing resource usage.",
)

def create_cmd_args(is_nasm: bool, is_xcode_argsfile: bool, *args) -> cmd_args:
    if is_xcode_argsfile:
        return cmd_args(replace_regex = XCODE_ARG_SUBSTITUTIONS, *args)
    elif is_nasm:
        return cmd_args(*args)
    else:
        return cmd_args(quote = "shell", *args)

def _mk_argsfiles(
        ctx: AnalysisContext,
        impl_params: CxxRuleConstructorParams,
        compiler_info: typing.Any,
        preprocessor: CPreprocessorInfo,
        ext: CxxExtension,
        headers_tag: ArtifactTag,
        is_xcode_argsfile: bool,
        is_precompile: bool = False,
        filename_prefix: str = "",
        uses_experimental_content_based_path_hashing: bool = False) -> CompileArgsfile:
    """
    Generate and return an {ext}.argsfile artifact and command args that utilize the argsfile.
    """
    is_nasm = compiler_info.compiler_type == "nasm"

    # prefix example: .cpp.xcode_
    filename_prefix = "{src_file_extension}.{xcode_prefix}{filename_prefix}".format(
        src_file_extension = ext.value,
        xcode_prefix = "xcode_" if is_xcode_argsfile else "",
        filename_prefix = filename_prefix,
    )

    argsfiles = []
    args_list = []

    def mk_argsfile(filename: str, args) -> Artifact:
        content = create_cmd_args(is_nasm, is_xcode_argsfile, args)
        argsfile, _ = ctx.actions.write(
            filename,
            content,
            allow_args = True,
            uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing,
        )
        return argsfile

    def make_toolchain_argsfile():
        compiler_info_flags = _add_compiler_info_flags(compiler_info)

        # Use the argsfile from the compiler info if it exists.
        if compiler_info.argsfile and not is_xcode_argsfile:
            compiler_info_argsfile = compiler_info.argsfile
        elif compiler_info.argsfile_xcode and is_xcode_argsfile:
            compiler_info_argsfile = compiler_info.argsfile_xcode
        else:
            # filename example: .cpp.toolchain_cxx_args
            compiler_info_filename = filename_prefix + "toolchain_cxx_args"
            compiler_info_argsfile = mk_argsfile(compiler_info_filename, compiler_info_flags)

        argsfiles.append(compiler_info_argsfile)
        args_list.append(compiler_info_flags)

    make_toolchain_argsfile()

    def make_compiler_type_argsfile():
        compiler_type_flags = _add_compiler_type_flags(ctx, compiler_info.compiler_type, ext)

        if impl_params.anon_targets_allowed:
            compiler_type_flags_anon_target = ctx.actions.anon_target(_compiler_type_flags_anon_rule, {
                "compiler_type": compiler_info.compiler_type,
                "is_xcode_argsfile": is_xcode_argsfile,
                "src_extension": ext.value,
            })
            compiler_type_argsfile_artifact = compiler_type_flags_anon_target.artifact("argsfile")
        else:
            compiler_type_argsfile_artifact = mk_argsfile(
                filename_prefix + "compiler_type_args",
                compiler_type_flags,
            )
        argsfiles.append(compiler_type_argsfile_artifact)
        args_list.append(compiler_type_flags)

    make_compiler_type_argsfile()

    def make_deps_argsfile():
        deps_args = []
        if is_precompile:
            # TODO(nml): We don't support dep files for now in precompile.
            deps_args.append(preprocessor.set.project_as_args("precompile_args"))
        else:
            deps_args.append(headers_tag.tag_artifacts(preprocessor.set.project_as_args("args")))

        # Different preprocessors will contain whether to use modules,
        # and the modulemap to use, so we need to get the final outcome.
        if preprocessor.set.reduce("uses_modules"):
            deps_args.append(headers_tag.tag_artifacts(preprocessor.set.project_as_args("modular_args")))

        # filename example: .cpp.deps_cxx_args
        deps_argsfile_filename = filename_prefix + "deps_cxx_args"
        argsfiles.append(mk_argsfile(deps_argsfile_filename, deps_args))
        args_list.extend(deps_args)

    make_deps_argsfile()

    def make_target_argsfile():
        target_args = cmd_args(
            # preprocessor
            impl_params.preprocessor_flags,
            cxx_by_language_ext(impl_params.lang_preprocessor_flags, ext.value),
            cxx_by_platform(ctx, impl_params.platform_preprocessor_flags),
            cxx_by_platform(ctx, cxx_by_language_ext(impl_params.lang_platform_preprocessor_flags, ext.value)),
            get_flags_for_compiler_type(compiler_info.compiler_type),

            # compiler
            cxx_by_language_ext(impl_params.lang_compiler_flags, ext.value),
            cxx_by_platform(ctx, impl_params.platform_compiler_flags),
            cxx_by_platform(ctx, cxx_by_language_ext(impl_params.lang_platform_compiler_flags, ext.value)),

            # ctx.attrs.compiler_flags need to come last to preserve buck1 ordering, this prevents compiler
            # flags ordering-dependent build errors
            impl_params.compiler_flags,
            headers_tag.tag_artifacts(preprocessor.set.project_as_args("include_dirs")),
        )

        # Workaround as that's not precompiled, but working just as prefix header.
        # Another thing is that it's clang specific, should be generalized.
        if hasattr(ctx.attrs, "precompiled_header") and ctx.attrs.precompiled_header != None:
            target_args.add(["-include", headers_tag.tag_artifacts(ctx.attrs.precompiled_header[CPrecompiledHeaderInfo].header)])
        if hasattr(ctx.attrs, "prefix_header") and ctx.attrs.prefix_header != None:
            target_args.add(["-include", headers_tag.tag_artifacts(ctx.attrs.prefix_header)])

        # filename example: .cpp.target_cxx_args
        target_argsfile_filename = filename_prefix + "target_cxx_args"
        argsfiles.append(mk_argsfile(target_argsfile_filename, target_args))
        args_list.append(target_args)

    make_target_argsfile()

    # Create a copy of the args so that we can continue to modify it later.
    args_without_file_prefix_args = cmd_args(args_list)

    def make_file_prefix_argsfile():
        if is_precompile:
            # The precompile_args field overrides these.
            return

        # Put file_prefix_args in argsfile, make sure they do not appear when evaluating $(cxxppflags)
        # to avoid "argument too long" errors
        file_prefix_args = headers_tag.tag_artifacts(preprocessor.set.project_as_args("file_prefix_args"))

        # filename example: .cpp.file_prefix_cxx_args
        file_prefix_args_filename = filename_prefix + "file_prefix_cxx_args"
        argsfiles.append(mk_argsfile(file_prefix_args_filename, file_prefix_args))
        args_list.append(file_prefix_args)

    make_file_prefix_argsfile()

    if is_xcode_argsfile:
        file_args = cmd_args(argsfiles, format = "@{}")
    elif is_nasm:
        file_args = cmd_args(argsfiles, format = "-@{}")
    else:
        file_args = cmd_args(argsfiles, format = "@{}", quote = "shell")

    # filename example: .cpp.cxx_compile_argsfile
    file_name = filename_prefix + "cxx_compile_argsfile"

    # For Xcode to parse argsfiles of argsfiles, the paths in the former must be absolute.
    argsfile, _ = ctx.actions.write(
        file_name,
        file_args,
        allow_args = True,
        absolute = is_xcode_argsfile,
        uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing,
    )

    args = create_cmd_args(is_nasm, is_xcode_argsfile, args_list)
    input_args = [args, file_args]

    cmd_form = cmd_args(
        argsfile,
        format = "-@{}" if is_nasm else "@{}",
        hidden = input_args,
    )

    return CompileArgsfile(
        file = argsfile,
        cmd_form = cmd_form,
        args = args,
        args_without_file_prefix_args = args_without_file_prefix_args,
    )

def _mk_header_units_argsfile(
        ctx: AnalysisContext,
        compiler_info: typing.Any,
        preprocessor: CPreprocessorInfo,
        ext: CxxExtension,
        filename_prefix: str = "",
        uses_experimental_content_based_path_hashing: bool = False) -> CompileArgsfile | None:
    """
    Generate and return an argsfile artifact containing all header unit options, and
    command args that utilize the argsfile.
    """
    if not preprocessor.set:
        return None
    if _get_category(ext) != "cxx_compile":
        return None
    if not _compiler_supports_header_units(compiler_info):
        return None

    file_name = "{}.{}header_units_args".format(ext.value, filename_prefix)
    args = cmd_args()
    args.add([
        # TODO(nml): We only support Clang 17+, which don't need/want the extra -f
        # arguments when compiling C++20. Clang 15 is too buggy to work properly, but if
        # you wanted to try, you would need the below options at the very least, to get
        # started:
        # "-fmodules",
        # "-fno-implicit-modules",
        # "-fno-implicit-module-maps",
        "-Wno-experimental-header-units",
        "-Wno-ambiguous-macro",
    ])

    # TODO(nml): Tag args with headers_tag.tag_artifacts() once -MD -MF reports correct
    # usage of PCMs. See T225373444 and _mk_header_units_argsfile() below.
    args.add(preprocessor.set.project_as_args("header_units_args"))
    file_args = cmd_args(args, quote = "shell")
    argsfile, _ = ctx.actions.write(
        file_name,
        file_args,
        allow_args = True,
        uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing,
    )
    cmd_form = cmd_args(argsfile, format = "@{}", hidden = file_args)

    return CompileArgsfile(
        file = argsfile,
        cmd_form = cmd_form,
        args = file_args,
        args_without_file_prefix_args = args,
    )

def _get_dep_tracking_mode(toolchain: Provider, file_type: DepFileType) -> DepTrackingMode:
    if file_type == DepFileType("cpp") or file_type == DepFileType("c"):
        return toolchain.cpp_dep_tracking_mode
    elif file_type == DepFileType("cuda"):
        return toolchain.cuda_dep_tracking_mode
    else:
        return DepTrackingMode("makefile")

def _generate_base_compile_command(
        ctx: AnalysisContext,
        impl_params: CxxRuleConstructorParams,
        pre: CPreprocessorInfo,
        headers_tag: ArtifactTag,
        ext: CxxExtension,
        filename_prefix: str = "",
        uses_experimental_content_based_path_hashing: bool = False) -> CxxCompileCommand:
    """
    Generate a common part of a compile command that is shared by all sources
    with a given extension.
    """
    toolchain = get_cxx_toolchain_info(ctx)
    compiler_info = _get_compiler_info(toolchain, ext)
    base_compile_cmd = _get_compile_base(toolchain, compiler_info)
    category = _get_category(ext)

    headers_dep_files = None
    dep_file_file_type_hint = _dep_file_type(ext)
    if dep_file_file_type_hint != None and toolchain.use_dep_files:
        tracking_mode = _get_dep_tracking_mode(toolchain, dep_file_file_type_hint)
        mk_dep_files_flags = get_headers_dep_files_flags_factory(tracking_mode)
        if mk_dep_files_flags:
            headers_dep_files = HeadersDepFiles(
                processor = cmd_args(toolchain.internal_tools.dep_file_processor),
                mk_flags = mk_dep_files_flags,
                tag = headers_tag,
                dep_tracking_mode = tracking_mode,
            )

    def gen_argsfiles(**kwargs):
        return _mk_argsfiles(
            ctx,
            impl_params,
            compiler_info,
            pre,
            ext,
            headers_tag,
            is_precompile = False,
            filename_prefix = filename_prefix,
            uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing,
            **kwargs
        )

    argsfile = gen_argsfiles(is_xcode_argsfile = False)
    xcode_argsfile = gen_argsfiles(is_xcode_argsfile = True)

    header_units_argsfile = _mk_header_units_argsfile(
        ctx,
        compiler_info,
        pre,
        ext,
        filename_prefix,
        uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing,
    )

    allow_cache_upload = cxx_attrs_get_allow_cache_upload(ctx.attrs, default = compiler_info.allow_cache_upload)
    return CxxCompileCommand(
        base_compile_cmd = base_compile_cmd,
        argsfile = argsfile,
        xcode_argsfile = xcode_argsfile,
        header_units_argsfile = header_units_argsfile,
        headers_dep_files = headers_dep_files,
        compiler_type = compiler_info.compiler_type,
        category = category,
        allow_cache_upload = allow_cache_upload,
    )
