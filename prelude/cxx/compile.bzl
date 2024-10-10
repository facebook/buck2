# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//cxx:cxx_utility.bzl", "cxx_attrs_get_allow_cache_upload")
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

# Supported assembly extensions
AsmExtensions = enum(
    ".s",
    ".sx",
    ".S",
    ".asm",
    ".asmpp",
)

# Supported Cxx file extensions
CxxExtension = enum(
    ".cpp",
    ".cc",
    ".cxx",
    ".c++",
    ".c",
    ".m",
    ".mm",
    ".cu",
    ".hip",
    ".h",
    ".hpp",
    ".hh",
    ".h++",
    ".hxx",
    ".bc",
    *AsmExtensions.values()
)

# Header files included in compilation databases
HeaderExtension = enum(
    ".h",
    ".hpp",
    ".hh",
    ".h++",
    ".hxx",
    ".cuh",
)

# File types for dep files
DepFileType = enum(
    "cpp",
    "c",
    "cuda",
    "asm",
)

_HeadersDepFiles = record(
    # An executable to wrap the actual command with for post-processing of dep
    # files into the format that Buck2 recognizes (i.e. one artifact per line).
    processor = field(cmd_args),
    # The tag that was added to headers.
    tag = field(ArtifactTag),
    # A function that produces new cmd_args to append to the compile command to
    # get it to emit the dep file. This will receive the output dep file as an
    # input.
    mk_flags = field(typing.Callable),
    # Dependency tracking mode to know how to generate dep file
    dep_tracking_mode = field(DepTrackingMode),
)

# Information about how to compile a source file of particular extension.
_CxxCompileCommand = record(
    # The compiler and any args which are independent of the rule.
    base_compile_cmd = field(cmd_args),
    # The argsfile of arguments from the rule and it's dependencies.
    argsfile = field(CompileArgsfile),
    # The argsfile to use for Xcode integration.
    xcode_argsfile = field(CompileArgsfile),
    # The argsfile containing exported header units args (for precompilation).
    header_units_argsfile = field(CompileArgsfile | None),
    # The argsfile containing all header units args (for actual compilation).
    private_header_units_argsfile = field(CompileArgsfile | None),
    headers_dep_files = field([_HeadersDepFiles, None]),
    compiler_type = field(str),
    # The action category
    category = field(str),
    allow_cache_upload = field(bool),
)

# Information about how to compile a source file.
CxxSrcCompileCommand = record(
    # Source file to compile.
    src = field(Artifact),
    # If we have multiple source entries with same files but different flags,
    # specify an index so we can differentiate them. Otherwise, use None.
    index = field([int, None], None),
    # The CxxCompileCommand to use to compile this file.
    cxx_compile_cmd = field(_CxxCompileCommand),
    # Arguments specific to the source file.
    args = field(list[typing.Any]),
    # Is this a header file?
    is_header = field(bool, False),
    # The index store factory to use to generate index store for this source file.
    index_store_factory = field(typing.Callable | None, None),
    error_handler = field([typing.Callable, None], None),
)

_CxxSrcPrecompileCommand = record(
    # Source file to compile.
    src = field(Artifact),
    # The CxxCompileCommand to use to compile this file.
    cxx_compile_cmd = field(_CxxCompileCommand),
    # Arguments specific to the source file.
    args = field(list[typing.Any]),
    # Extra argsfile to include after any other header units argsfile but before the
    # main argsfiles.
    extra_argsfile = field([CompileArgsfile, None], None),
)

# Output of creating compile commands for Cxx source files.
CxxCompileCommandOutput = record(
    # List of compile commands for each source file.
    src_compile_cmds = field(list[CxxSrcCompileCommand], default = []),
    # Base compile commands for each source file extension.
    base_compile_cmds = field(dict[CxxExtension, _CxxCompileCommand], default = {}),
    # Argsfiles generated for compiling these source files.
    argsfiles = field(CompileArgsfiles, default = CompileArgsfiles()),
    # List of compile commands for use in compilation database generation.
    comp_db_compile_cmds = field(list[CxxSrcCompileCommand], default = []),
)

CxxCompileOutput = record(
    # The compiled `.o` file.
    object = field(Artifact),
    object_format = field(CxxObjectFormat, CxxObjectFormat("native")),
    object_has_external_debug_info = field(bool, False),
    # Externally referenced debug info, which doesn't get linked with the
    # object (e.g. the above `.o` when using `-gsplit-dwarf=single` or the
    # the `.dwo` when using `-gsplit-dwarf=split`).
    external_debug_info = field(Artifact | None, None),
    clang_remarks = field(Artifact | None, None),
    clang_trace = field(Artifact | None, None),
    gcno_file = field(Artifact | None, None),
    index_store = field(Artifact | None, None),
    assembly = field(Artifact | None, None),
    diagnostics = field(Artifact | None, None),
    preproc = field(Artifact | None, None),
)

CxxCompileFlavor = enum(
    # Default compilation witout alterations
    "default",
    # Produces position independent compile outputs
    "pic",
    # Produces position independent compile outputs
    # using optimization flags from toolchain
    "pic_optimized",
)

_XCODE_ARG_SUBSTITUTION = [
    (regex("-filter-error=.+"), "-fcolor-diagnostics"),
    (regex("-filter-ignore=.+"), "-fcolor-diagnostics"),
    (regex("-filter-warning=.+"), "-fcolor-diagnostics"),
    # @oss-disable: (regex("-fobjc-export-direct-methods"), "-fcolor-diagnostics"), 
    # @oss-disable: (regex("-fpika-runtime-checks"), "-fcolor-diagnostics"), 
]

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
        add_coverage_instrumentation_compiler_flags: bool,
        header_preprocessor_info: CPreprocessorInfo = CPreprocessorInfo()) -> CxxCompileCommandOutput:
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
    cxx_compile_cmd_by_ext = {}  # type: dict[CxxExtension, _CxxCompileCommand]
    argsfile_by_ext = {}  # type: dict[str, CompileArgsfile]
    xcode_argsfile_by_ext = {}  # type: dict[str, CompileArgsfile]

    src_extensions = collect_source_extensions(srcs_with_flags, extension_for_plain_headers)

    # Deduplicate shared arguments to save memory. If we compile multiple files
    # of the same extension they will have some of the same flags. Save on
    # allocations by caching and reusing these objects.
    for ext in src_extensions:
        cmd = _generate_base_compile_command(ctx, impl_params, pre, header_preprocessor_info, headers_tag, ext)
        cxx_compile_cmd_by_ext[ext] = cmd
        argsfile_by_ext[ext.value] = cmd.argsfile
        xcode_argsfile_by_ext[ext.value] = cmd.xcode_argsfile

    # only specify error_handler if one exists
    error_handler_args = {}
    if impl_params.error_handler:
        error_handler_args["error_handler"] = impl_params.error_handler

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

        src_compile_command = CxxSrcCompileCommand(src = src.file, cxx_compile_cmd = cxx_compile_cmd, args = src_args, index = src.index, is_header = src.is_header, index_store_factory = impl_params.index_store_factory, **error_handler_args)
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

def _compile_index_store(ctx: AnalysisContext, src_compile_cmd: CxxSrcCompileCommand, toolchain: CxxToolchainInfo, compile_cmd: cmd_args, pic: bool) -> Artifact | None:
    if src_compile_cmd.index_store_factory:
        return src_compile_cmd.index_store_factory(ctx, src_compile_cmd, toolchain, compile_cmd, pic)
    return None

def _compile_single_cxx(
        ctx: AnalysisContext,
        toolchain: CxxToolchainInfo,
        default_object_format: CxxObjectFormat,
        bitcode_args: cmd_args,
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
    object = ctx.actions.declare_output(
        "__objects__",
        "{}.{}".format(filename_base, toolchain.linker_info.object_file_extension),
    )

    compiler_type = src_compile_cmd.cxx_compile_cmd.compiler_type
    cmd = _get_base_compile_cmd(
        bitcode_args = bitcode_args,
        src_compile_cmd = src_compile_cmd,
        pic = pic,
        use_header_units = use_header_units,
        output_args = cmd_args(get_output_flags(compiler_type, object)),
    )
    cmd.add(cmd_args(optimization_flags))

    action_dep_files = {}

    headers_dep_files = src_compile_cmd.cxx_compile_cmd.headers_dep_files
    if headers_dep_files:
        dep_file = ctx.actions.declare_output(
            paths.join("__dep_files__", filename_base),
        ).as_output()

        processor_flags, compiler_flags = headers_dep_files.mk_flags(ctx.actions, filename_base, src_compile_cmd.src)
        cmd.add(compiler_flags)

        # API: First argument is the dep file source path, second is the
        # dep file destination path, other arguments are the actual compile
        # command.
        cmd = cmd_args([
            headers_dep_files.processor,
            headers_dep_files.dep_tracking_mode.value,
            processor_flags,
            headers_dep_files.tag.tag_artifacts(dep_file),
            cmd,
        ])

        action_dep_files["headers"] = headers_dep_files.tag

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

    # only specify error_handler if one exists
    error_handler_args = {}
    if src_compile_cmd.error_handler:
        error_handler_args["error_handler"] = src_compile_cmd.error_handler

    ctx.actions.run(
        cmd,
        category = src_compile_cmd.cxx_compile_cmd.category,
        identifier = identifier,
        dep_files = action_dep_files,
        allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
        allow_dep_file_cache_upload = False,
        **error_handler_args
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
    index_store = _compile_index_store(ctx, src_compile_cmd, toolchain, compile_index_store_cmd, pic)

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
            output_args = cmd_args("-S", get_output_flags(compiler_type, assembly)),
        )
        ctx.actions.run(
            assembly_cmd,
            category = src_compile_cmd.cxx_compile_cmd.category,
            identifier = identifier + " (assembly)",
            allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
            allow_dep_file_cache_upload = False,
            **error_handler_args
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
            output_args = cmd_args("-fsyntax-only"),
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
            **error_handler_args
        )
    else:
        diagnostics = None

    # Generate pre-processed sources
    preproc = ctx.actions.declare_output(
        "__preprocessed__",
        "{}.{}".format(filename_base, "i"),
    )
    preproc_cmd = _get_base_compile_cmd(bitcode_args, src_compile_cmd, pic, cmd_args("-E", "-dD", get_output_flags(compiler_type, preproc)))
    ctx.actions.run(
        preproc_cmd,
        category = src_compile_cmd.cxx_compile_cmd.category,
        identifier = identifier + " (preprocessor)",
        allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
        allow_dep_file_cache_upload = False,
        **error_handler_args
    )

    return CxxCompileOutput(
        object = object,
        object_format = object_format,
        object_has_external_debug_info = object_has_external_debug_info,
        clang_remarks = clang_remarks,
        clang_trace = clang_trace,
        gcno_file = gcno_file,
        index_store = index_store,
        assembly = assembly,
        diagnostics = diagnostics,
        preproc = preproc,
    )

def _get_base_compile_cmd(
        bitcode_args: cmd_args,
        src_compile_cmd: CxxSrcCompileCommand,
        pic: bool,
        output_args: cmd_args | None = None,
        use_header_units: bool = False) -> cmd_args:
    """
    Construct a shared compile command for a single CXX source based on
    `src_compile_command` and other compilation options.
    """
    cmd = cmd_args(src_compile_cmd.cxx_compile_cmd.base_compile_cmd)
    if output_args:
        cmd.add(output_args)

    compiler_type = src_compile_cmd.cxx_compile_cmd.compiler_type

    args = cmd_args()

    if pic:
        args.add(get_pic_flags(compiler_type))

    if use_header_units and src_compile_cmd.cxx_compile_cmd.private_header_units_argsfile:
        args.add(src_compile_cmd.cxx_compile_cmd.private_header_units_argsfile.cmd_form)

    args.add(src_compile_cmd.cxx_compile_cmd.argsfile.cmd_form)
    args.add(src_compile_cmd.args)

    cmd.add(args)
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
    bitcode_args = cmd_args()
    if linker_info.lto_mode == LtoMode("none"):
        if toolchain.object_format == CxxObjectFormat("bitcode"):
            bitcode_args.add("-emit-llvm")
            default_object_format = CxxObjectFormat("bitcode")
        elif toolchain.object_format == CxxObjectFormat("embedded-bitcode"):
            bitcode_args.add("-fembed-bitcode")
            default_object_format = CxxObjectFormat("embedded-bitcode")
    else:
        # LTO always produces bitcode object in any mode (thin, full, etc)
        default_object_format = CxxObjectFormat("bitcode")

    objects = []
    for src_compile_cmd in src_compile_cmds:
        cxx_compile_output = _compile_single_cxx(
            ctx = ctx,
            toolchain = toolchain,
            default_object_format = default_object_format,
            bitcode_args = bitcode_args,
            optimization_flags = toolchain.optimization_compiler_flags_EXPERIMENTAL if flavor == CxxCompileFlavor("pic_optimized") else [],
            src_compile_cmd = src_compile_cmd,
            pic = flavor != CxxCompileFlavor("default"),
            provide_syntax_only = provide_syntax_only,
            use_header_units = use_header_units,
        )
        objects.append(cxx_compile_output)

    return objects

def _compiler_supports_header_units(compiler_info: typing.Any):
    return (compiler_info.compiler_type == "clang" and
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
    if header.name.endswith("-inl.h"):
        return False
    if header.name.endswith(".tcc"):
        return False
    if header.name.endswith("-pre.h"):
        return False
    if header.name.endswith("-post.h"):
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
        cmd: _CxxCompileCommand) -> _CxxSrcPrecompileCommand:
    include_dirs = flatten([x.include_dirs for x in preprocessors])
    converted_headers = [
        _convert_raw_header(ctx, raw_header, include_dirs)
        for raw_header in flatten([x.raw_headers for x in preprocessors])
    ]
    headers = [
        header
        for header in flatten([x.headers for x in preprocessors]) + converted_headers
        if (_is_standalone_header(header) if header_group == None else regex_match(header_group, header.name))
    ]

    module_name = _get_module_name(ctx, group_name)
    import_name = _get_import_filename(ctx, group_name)
    input_header = ctx.actions.write(module_name, "")

    import_stub = ctx.actions.write(
        import_name,
        """
#ifdef FACEBOOK_CPP_HEADER_UNIT
export
#endif
import \"{}\";
""".format(module_name),
    )

    modulemap_headers = []
    symlinked_files = {}
    for header in headers:
        path = paths.normalize(paths.join(header.namespace, header.name))
        symlinked_files[path] = import_stub
        modulemap_headers.append(path)

    modulemap_content = """
module "{}" {{
  header "{}"
  export *
}}
""".format(module_name, module_name)
    modulemap_file = ctx.actions.write("module.modulemap" + group_name, modulemap_content)

    src_dir = ctx.actions.symlinked_dir(
        "header-unit" + group_name,
        symlinked_files | {
            module_name: input_header,
            import_name: import_stub,
            "module.modulemap": modulemap_file,
        },
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
            name = "export" + group_name,
            ext = CxxExtension(".cpp"),
        )

    for header in headers:
        args.extend(["-include", paths.join(header.namespace, header.name)])
    args.extend(["-xc++-user-header", "-fmodule-header"])
    args.extend(["-fmodule-name={}".format(module_name)])
    args.extend(["-Xclang", cmd_args(input_header, format = "-fmodules-embed-file={}")])
    args.extend(["--precompile", input_header])

    return _CxxSrcPrecompileCommand(
        src = src_dir,
        cxx_compile_cmd = cmd,
        args = args,
        extra_argsfile = extra_argsfile,
    )

def _precompile_single_cxx(
        ctx: AnalysisContext,
        impl_params: CxxRuleConstructorParams,
        group_name: str,
        src_compile_cmd: _CxxSrcPrecompileCommand) -> HeaderUnit:
    identifier = src_compile_cmd.src.short_path

    filename = "{}.pcm".format(identifier)
    module = ctx.actions.declare_output("__pcm_files__", filename)

    cmd = cmd_args(src_compile_cmd.cxx_compile_cmd.base_compile_cmd)
    if src_compile_cmd.cxx_compile_cmd.header_units_argsfile:
        cmd.add(src_compile_cmd.cxx_compile_cmd.header_units_argsfile.cmd_form)
    if src_compile_cmd.extra_argsfile:
        cmd.add(src_compile_cmd.extra_argsfile.cmd_form)
    cmd.add(src_compile_cmd.cxx_compile_cmd.argsfile.cmd_form)
    cmd.add(src_compile_cmd.args)
    cmd.add(["-o", module.as_output()])

    action_dep_files = {}
    headers_dep_files = src_compile_cmd.cxx_compile_cmd.headers_dep_files
    if headers_dep_files:
        dep_file = ctx.actions.declare_output(
            paths.join("__dep_files__", identifier),
        ).as_output()

        processor_flags, compiler_flags = headers_dep_files.mk_flags(
            ctx.actions,
            identifier,
            src_compile_cmd.src,
        )
        cmd.add(compiler_flags)

        # API: First argument is the dep file source path, second is the
        # dep file destination path, other arguments are the actual compile
        # command.
        cmd = cmd_args([
            headers_dep_files.processor,
            headers_dep_files.dep_tracking_mode.value,
            processor_flags,
            headers_dep_files.tag.tag_artifacts(dep_file),
            cmd,
        ])
        action_dep_files["headers"] = headers_dep_files.tag

    ctx.actions.run(
        cmd,
        category = "cxx_precompile",
        identifier = identifier,
        dep_files = action_dep_files,
        allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
        allow_dep_file_cache_upload = False,
    )

    return HeaderUnit(
        name = _get_module_name(ctx, group_name),
        module = module,
        include_dir = src_compile_cmd.src,
        import_include = _get_import_filename(ctx, group_name) if impl_params.export_header_unit == "preload" else None,
    )

def precompile_cxx(
        ctx: AnalysisContext,
        impl_params: CxxRuleConstructorParams,
        preprocessors: list[CPreprocessor],
        compile_cmd_output: CxxCompileCommandOutput) -> list[CPreprocessor]:
    """
    Produces header units for the target and returns a list of preprocessors enabling
    them; depending on those preprocessors will allow the corresponding module to load.
    """
    toolchain = get_cxx_toolchain_info(ctx)
    if not _compiler_supports_header_units(toolchain.cxx_compiler_info):
        return []

    ext = CxxExtension(".cpp")
    if ext not in compile_cmd_output.base_compile_cmds:
        return []
    cmd = compile_cmd_output.base_compile_cmds[ext]

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
        header_unit = _precompile_single_cxx(ctx, impl_params, "", precompile_cmd)
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
            header_unit = _precompile_single_cxx(ctx, impl_params, name, precompile_cmd)
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
    if ext.value in (".cpp", ".cc", ".mm", ".cxx", ".c++", ".h", ".hpp", ".hh", ".h++", ".hxx", ".bc"):
        compiler_info = toolchain.cxx_compiler_info
    elif ext.value in (".c", ".m"):
        compiler_info = toolchain.c_compiler_info
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
    if ext.value in (".cpp", ".cc", ".cxx", ".c++", ".h", ".hpp", ".hh", ".h++", ".hxx"):
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
    if ext.value in (".cpp", ".cc", ".mm", ".cxx", ".c++", ".h", ".hpp", ".hh", ".h++", ".hxx"):
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

def _add_compiler_info_flags(ctx: AnalysisContext, compiler_info: typing.Any, ext: CxxExtension) -> list:
    cmd = []
    cmd.append(compiler_info.preprocessor_flags or [])
    cmd.append(compiler_info.compiler_flags or [])
    cmd.append(get_flags_for_reproducible_build(ctx, compiler_info.compiler_type))

    if ext.value not in (".asm", ".asmpp"):
        # Clang's asm compiler doesn't support colorful output, so we skip this there.
        cmd.append(get_flags_for_colorful_output(compiler_info.compiler_type))

    return cmd

def _mk_argsfile(
        ctx: AnalysisContext,
        file_name: str,
        args_list: list,
        is_nasm: bool,
        is_xcode_argsfile: bool) -> Artifact:
    if is_xcode_argsfile:
        replace_regex = []
        for re, sub in _XCODE_ARG_SUBSTITUTION:
            replace_regex.append((re, sub))
        file_args = cmd_args(args_list, replace_regex = replace_regex)
    else:
        file_args = cmd_args(args_list) if is_nasm else cmd_args(args_list, quote = "shell")
    argsfile, _ = ctx.actions.write(file_name, file_args, allow_args = True)
    return argsfile

def _mk_argsfiles(
        ctx: AnalysisContext,
        impl_params: CxxRuleConstructorParams,
        compiler_info: typing.Any,
        preprocessor: CPreprocessorInfo,
        ext: CxxExtension,
        headers_tag: ArtifactTag,
        is_xcode_argsfile: bool) -> CompileArgsfile:
    """
    Generate and return an {ext}.argsfile artifact and command args that utilize the argsfile.
    """
    is_nasm = compiler_info.compiler_type == "nasm"
    filename_prefix = "xcode_" if is_xcode_argsfile else ""

    argsfiles = []
    args_list = []

    compiler_info_flags = _add_compiler_info_flags(ctx, compiler_info, ext)
    compiler_info_filename = ext.value + ".{}toolchain_cxx_args".format(filename_prefix)
    argsfiles.append(_mk_argsfile(ctx, compiler_info_filename, compiler_info_flags, is_nasm, is_xcode_argsfile))
    args_list.append(compiler_info_flags)

    deps_args = []
    deps_args.append(headers_tag.tag_artifacts(preprocessor.set.project_as_args("args")))

    # Different preprocessors will contain whether to use modules,
    # and the modulemap to use, so we need to get the final outcome.
    if preprocessor.set.reduce("uses_modules"):
        deps_args.append(headers_tag.tag_artifacts(preprocessor.set.project_as_args("modular_args")))

    deps_argsfile_filename = ext.value + ".{}deps_cxx_args".format(filename_prefix)
    argsfiles.append(_mk_argsfile(ctx, deps_argsfile_filename, deps_args, is_nasm, is_xcode_argsfile))
    args_list.extend(deps_args)

    target_args = []
    target_args.append(_preprocessor_flags(ctx, impl_params, ext.value))
    target_args.append(get_flags_for_compiler_type(compiler_info.compiler_type))
    target_args.append(_compiler_flags(ctx, impl_params, ext.value))
    target_args.append(headers_tag.tag_artifacts(preprocessor.set.project_as_args("include_dirs")))

    # Workaround as that's not precompiled, but working just as prefix header.
    # Another thing is that it's clang specific, should be generalized.
    if hasattr(ctx.attrs, "precompiled_header") and ctx.attrs.precompiled_header != None:
        target_args.append(["-include", headers_tag.tag_artifacts(ctx.attrs.precompiled_header[CPrecompiledHeaderInfo].header)])
    if hasattr(ctx.attrs, "prefix_header") and ctx.attrs.prefix_header != None:
        target_args.append(["-include", headers_tag.tag_artifacts(ctx.attrs.prefix_header)])

    target_argsfile_filename = ext.value + ".{}target_cxx_args".format(filename_prefix)
    argsfiles.append(_mk_argsfile(ctx, target_argsfile_filename, target_args, is_nasm, is_xcode_argsfile))
    args_list.extend(target_args)

    # Create a copy of the args so that we can continue to modify it later.
    args_without_file_prefix_args = cmd_args(args_list)

    # Put file_prefix_args in argsfile, make sure they do not appear when evaluating $(cxxppflags)
    # to avoid "argument too long" errors
    file_prefix_args = headers_tag.tag_artifacts(cmd_args(preprocessor.set.project_as_args("file_prefix_args")))
    file_prefix_args_filename = ext.value + ".{}file_prefix_cxx_args".format(filename_prefix)
    argsfiles.append(_mk_argsfile(ctx, file_prefix_args_filename, [file_prefix_args], is_nasm, is_xcode_argsfile))
    args_list.append(file_prefix_args)

    if is_xcode_argsfile:
        replace_regex = []
        for re, sub in _XCODE_ARG_SUBSTITUTION:
            replace_regex.append((re, sub))
        args = cmd_args(args_list, replace_regex = replace_regex)
        file_args = cmd_args(argsfiles, format = "@{}")
    else:
        args = cmd_args(args_list) if is_nasm else cmd_args(args_list, quote = "shell")
        file_args = cmd_args(argsfiles, format = "-@{}") if is_nasm else cmd_args(argsfiles, format = "@{}", quote = "shell")

    file_name = ext.value + ".{}cxx_compile_argsfile".format(filename_prefix)

    # For Xcode to parse argsfiles of argsfiles, the paths in the former must be absolute.
    argsfile, _ = ctx.actions.write(file_name, file_args, allow_args = True, absolute = is_xcode_argsfile)

    input_args = [args, file_args]

    format = "-@{}" if is_nasm else "@{}"
    cmd_form = cmd_args(argsfile, format = format, hidden = input_args)

    return CompileArgsfile(
        file = argsfile,
        cmd_form = cmd_form,
        input_args = input_args,
        args = args,
        args_without_file_prefix_args = args_without_file_prefix_args,
    )

def _compiler_flags(ctx: AnalysisContext, impl_params: CxxRuleConstructorParams, ext: str) -> list[typing.Any]:
    return (
        cxx_by_language_ext(impl_params.lang_compiler_flags, ext) +
        flatten(cxx_by_platform(ctx, impl_params.platform_compiler_flags)) +
        flatten(cxx_by_platform(ctx, cxx_by_language_ext(impl_params.lang_platform_compiler_flags, ext))) +
        # ctx.attrs.compiler_flags need to come last to preserve buck1 ordering, this prevents compiler
        # flags ordering-dependent build errors
        impl_params.compiler_flags
    )

def _preprocessor_flags(ctx: AnalysisContext, impl_params: CxxRuleConstructorParams, ext: str) -> list[typing.Any]:
    return (
        impl_params.preprocessor_flags +
        cxx_by_language_ext(impl_params.lang_preprocessor_flags, ext) +
        flatten(cxx_by_platform(ctx, impl_params.platform_preprocessor_flags)) +
        flatten(cxx_by_platform(ctx, cxx_by_language_ext(impl_params.lang_platform_preprocessor_flags, ext)))
    )

def _mk_header_units_argsfile(
        ctx: AnalysisContext,
        compiler_info: typing.Any,
        preprocessor: CPreprocessorInfo,
        name: str,
        ext: CxxExtension) -> CompileArgsfile | None:
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

    file_name = "{}.{}.header_units_args".format(ext.value, name)
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
    # usage of PCMs.
    args.add(preprocessor.set.project_as_args("header_units_args"))
    input_args = [args]
    file_args = cmd_args(args, quote = "shell")
    argsfile, _ = ctx.actions.write(file_name, file_args, allow_args = True)
    cmd_form = cmd_args(argsfile, format = "@{}", hidden = input_args)

    return CompileArgsfile(
        file = argsfile,
        cmd_form = cmd_form,
        input_args = input_args,
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
        header_pre: CPreprocessorInfo,
        headers_tag: ArtifactTag,
        ext: CxxExtension) -> _CxxCompileCommand:
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
            headers_dep_files = _HeadersDepFiles(
                processor = cmd_args(toolchain.internal_tools.dep_file_processor),
                mk_flags = mk_dep_files_flags,
                tag = headers_tag,
                dep_tracking_mode = tracking_mode,
            )

    argsfile = _mk_argsfiles(ctx, impl_params, compiler_info, pre, ext, headers_tag, False)
    xcode_argsfile = _mk_argsfiles(ctx, impl_params, compiler_info, pre, ext, headers_tag, True)
    header_units_argsfile = _mk_header_units_argsfile(ctx, compiler_info, header_pre, "public", ext)
    private_header_units_argsfile = _mk_header_units_argsfile(ctx, compiler_info, pre, "private", ext)

    allow_cache_upload = cxx_attrs_get_allow_cache_upload(ctx.attrs, default = compiler_info.allow_cache_upload)
    return _CxxCompileCommand(
        base_compile_cmd = base_compile_cmd,
        argsfile = argsfile,
        xcode_argsfile = xcode_argsfile,
        header_units_argsfile = header_units_argsfile,
        private_header_units_argsfile = private_header_units_argsfile,
        headers_dep_files = headers_dep_files,
        compiler_type = compiler_info.compiler_type,
        category = category,
        allow_cache_upload = allow_cache_upload,
    )
