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
load("@prelude//utils:set.bzl", "set")
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
    "CPrecompiledHeaderInfo",
)
load(":platform.bzl", "cxx_by_platform")
load(
    ":preprocessor.bzl",
    "CPreprocessor",  # @unused Used as a type
    "CPreprocessorInfo",  # @unused Used as a type
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
)

# Output of creating compile commands for Cxx source files.
CxxCompileCommandOutput = record(
    # List of compile commands for each source file.
    src_compile_cmds = field(list[CxxSrcCompileCommand], default = []),
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

def collect_extensions(srcs: list[CxxSrcWithFlags]) -> list[CxxExtension]:
    """
    Collect extensions of source files while doing light normalization.
    """

    duplicates = {
        ".c++": ".cpp",
        ".cc": ".cpp",
        ".cxx": ".cpp",
    }

    extensions = set([CxxExtension(duplicates.get(src.file.extension, src.file.extension)) for src in srcs])
    return extensions.list()

def default_source_extension_for_plain_header(rule_type: str) -> CxxExtension:
    """
    Returns default source file extension to use to get get compiler flags for plain .h headers.
    """

    # Default to (Objective-)C++ instead of plain (Objective-)C as it is more likely to be compatible with both.
    return CxxExtension(".mm") if rule_type.startswith("apple_") else CxxExtension(".cpp")

def detect_source_extension_for_plain_headers(exts: list[CxxExtension], rule_type: str) -> CxxExtension:
    """
    For a given list source files determine which source file extension
    to use to get compiler flags for plain .h headers.
    """

    exts = set(exts)

    # Assembly doesn't need any special handling as included files tend to have .asm extension themselves.
    # And the presence of assembly in the target doesn't tell us anything about the language of .h files.
    for asm_ext in AsmExtensions:
        exts.remove(asm_ext)

    if exts.size() == 0:
        return default_source_extension_for_plain_header(rule_type)

    if exts.size() == 1:
        return exts.list()[0]
    if exts.contains(CxxExtension(".hip")):
        return CxxExtension(".hip")
    if exts.contains(CxxExtension(".cu")):
        return CxxExtension(".cu")
    if exts.contains(CxxExtension(".mm")):
        return CxxExtension(".mm")
    if exts.contains(CxxExtension(".cpp")) and exts.contains(CxxExtension(".m")):
        return CxxExtension(".mm")
    if exts.contains(CxxExtension(".cpp")):
        return CxxExtension(".cpp")
    if exts.contains(CxxExtension(".m")):
        return CxxExtension(".m")
    return CxxExtension(".c")

def collect_source_extensions(
        srcs: list[CxxSrcWithFlags],
        default_for_headers: CxxExtension) -> list[CxxExtension]:
    """
    Return unique source extensions from a list of source and header files where
    header extensions are mapped to corresponding source extensions.
    """
    source_extensions = set([get_source_extension(src, default_for_headers) for src in srcs])
    return source_extensions.list()

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
        inherited_preprocessor_infos: list[CPreprocessorInfo]) -> CxxCompileCommandOutput:
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
        cmd = _generate_base_compile_command(ctx, impl_params, pre, headers_tag, ext)
        cxx_compile_cmd_by_ext[ext] = cmd
        argsfile_by_ext[ext.value] = cmd.argsfile
        xcode_argsfile_by_ext[ext.value] = cmd.xcode_argsfile

    for src in srcs_with_flags:
        src_args = []
        src_args.extend(src.flags)

        ext = get_source_extension(src, extension_for_plain_headers)

        if src.is_header:
            language_mode = get_header_language_mode(ext)
            src_args.extend(["-x", language_mode] if language_mode else [])

        cxx_compile_cmd = cxx_compile_cmd_by_ext[ext]
        src_args.extend(["-c", src.file])

        src_compile_command = CxxSrcCompileCommand(src = src.file, cxx_compile_cmd = cxx_compile_cmd, args = src_args, index = src.index, is_header = src.is_header)
        if src.is_header:
            hdr_compile_cmds.append(src_compile_command)
        else:
            src_compile_cmds.append(src_compile_command)

    argsfile_by_ext.update(impl_params.additional.argsfiles.relative)
    xcode_argsfile_by_ext.update(impl_params.additional.argsfiles.xcode)

    return CxxCompileCommandOutput(
        src_compile_cmds = src_compile_cmds,
        argsfiles = CompileArgsfiles(
            relative = argsfile_by_ext,
            xcode = xcode_argsfile_by_ext,
        ),
        comp_db_compile_cmds = src_compile_cmds + hdr_compile_cmds,
    )

def _compile_single_cxx(
        ctx: AnalysisContext,
        toolchain: CxxToolchainInfo,
        default_object_format: CxxObjectFormat,
        bitcode_args: cmd_args,
        src_compile_cmd: CxxSrcCompileCommand,
        pic: bool) -> CxxCompileOutput:
    """
    Construct a final compile command for a single CXX source based on
    `src_compile_command` and other compilation options.
    """

    identifier = src_compile_cmd.src.short_path
    if src_compile_cmd.index != None:
        # Add a unique postfix if we have duplicate source files with different flags
        identifier = identifier + "_" + str(src_compile_cmd.index)

    filename_base = identifier + (".pic" if pic else "")
    object = ctx.actions.declare_output(
        "__objects__",
        "{}.{}".format(filename_base, toolchain.linker_info.object_file_extension),
    )

    cmd = cmd_args(src_compile_cmd.cxx_compile_cmd.base_compile_cmd)

    compiler_type = src_compile_cmd.cxx_compile_cmd.compiler_type
    cmd.add(get_output_flags(compiler_type, object))

    args = cmd_args()

    if pic:
        args.add(get_pic_flags(compiler_type))

    args.add(src_compile_cmd.cxx_compile_cmd.argsfile.cmd_form)
    args.add(src_compile_cmd.args)

    cmd.add(args)
    cmd.add(bitcode_args)

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

    if pic:
        identifier += " (pic)"

    clang_remarks = None
    if toolchain.clang_remarks and compiler_type == "clang":
        args.add(["-fsave-optimization-record", "-fdiagnostics-show-hotness", "-foptimization-record-passes=" + toolchain.clang_remarks])
        clang_remarks = ctx.actions.declare_output(
            paths.join("__objects__", "{}.opt.yaml".format(filename_base)),
        )
        cmd.hidden(clang_remarks.as_output())

    clang_trace = None
    if toolchain.clang_trace and compiler_type == "clang":
        args.add(["-ftime-trace"])
        clang_trace = ctx.actions.declare_output(
            paths.join("__objects__", "{}.json".format(filename_base)),
        )
        cmd.hidden(clang_trace.as_output())

    ctx.actions.run(
        cmd,
        category = src_compile_cmd.cxx_compile_cmd.category,
        identifier = identifier,
        dep_files = action_dep_files,
        allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
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

    return CxxCompileOutput(
        object = object,
        object_format = object_format,
        object_has_external_debug_info = object_has_external_debug_info,
        clang_remarks = clang_remarks,
        clang_trace = clang_trace,
    )

def compile_cxx(
        ctx: AnalysisContext,
        src_compile_cmds: list[CxxSrcCompileCommand],
        pic: bool = False) -> list[CxxCompileOutput]:
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
            ctx,
            toolchain,
            default_object_format,
            bitcode_args,
            src_compile_cmd,
            pic,
        )
        objects.append(cxx_compile_output)

    return objects

def cxx_objects_sub_targets(outs: list[CxxCompileOutput]) -> dict[str, list[Provider]]:
    objects_sub_targets = {}
    for obj in outs:
        sub_targets = {}
        if obj.clang_trace:
            sub_targets["clang-trace"] = [DefaultInfo(obj.clang_trace)]
        if obj.clang_remarks:
            sub_targets["clang-remarks"] = [DefaultInfo(obj.clang_remarks)]
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
    if ext.value in (".cpp", ".cc", ".mm", ".cxx", ".c++", ".h", ".hpp", ".hh", ".h++", ".hxx"):
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
    else:
        # This should be unreachable as long as we handle all enum values
        fail("Unknown extension: " + ext.value)

def _get_compile_base(compiler_info: typing.Any) -> cmd_args:
    """
    Given a compiler info returned by _get_compiler_info, form the base compile args.
    """

    cmd = cmd_args(compiler_info.compiler)

    return cmd

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

def _add_compiler_info_flags(ctx: AnalysisContext, compiler_info: typing.Any, ext: CxxExtension, cmd: cmd_args):
    cmd.add(compiler_info.preprocessor_flags or [])
    cmd.add(compiler_info.compiler_flags or [])
    cmd.add(get_flags_for_reproducible_build(ctx, compiler_info.compiler_type))

    if ext.value not in (".asm", ".asmpp"):
        # Clang's asm compiler doesn't support colorful output, so we skip this there.
        cmd.add(get_flags_for_colorful_output(compiler_info.compiler_type))

def _mk_argsfile(
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
    args = cmd_args()

    _add_compiler_info_flags(ctx, compiler_info, ext, args)

    args.add(headers_tag.tag_artifacts(preprocessor.set.project_as_args("args")))

    # Different preprocessors will contain whether to use modules,
    # and the modulemap to use, so we need to get the final outcome.
    if preprocessor.set.reduce("uses_modules"):
        args.add(headers_tag.tag_artifacts(preprocessor.set.project_as_args("modular_args")))

    args.add(_preprocessor_flags(ctx, impl_params, ext.value))
    args.add(get_flags_for_compiler_type(compiler_info.compiler_type))
    args.add(_compiler_flags(ctx, impl_params, ext.value))
    args.add(headers_tag.tag_artifacts(preprocessor.set.project_as_args("include_dirs")))

    # Workaround as that's not precompiled, but working just as prefix header.
    # Another thing is that it's clang specific, should be generalized.
    if hasattr(ctx.attrs, "precompiled_header") and ctx.attrs.precompiled_header != None:
        args.add(["-include", headers_tag.tag_artifacts(ctx.attrs.precompiled_header[CPrecompiledHeaderInfo].header)])
    if hasattr(ctx.attrs, "prefix_header") and ctx.attrs.prefix_header != None:
        args.add(["-include", headers_tag.tag_artifacts(ctx.attrs.prefix_header)])

    # Create a copy of the args so that we can continue to modify it later.
    args_without_file_prefix_args = cmd_args(args)

    # Put file_prefix_args in argsfile directly, make sure they do not appear when evaluating $(cxxppflags)
    # to avoid "argument too long" errors
    args.add(headers_tag.tag_artifacts(cmd_args(preprocessor.set.project_as_args("file_prefix_args"))))

    if is_xcode_argsfile:
        for re, sub in _XCODE_ARG_SUBSTITUTION:
            args.replace_regex(re, sub)
        file_args = args
    else:
        file_args = cmd_args(args, quote = "shell")

    file_name = ext.value + ("-xcode.argsfile" if is_xcode_argsfile else ".argsfile")
    argsfile, _ = ctx.actions.write(file_name, file_args, allow_args = True)

    input_args = [args]

    cmd_form = cmd_args(argsfile, format = "@{}").hidden(input_args)

    return CompileArgsfile(
        file = argsfile,
        cmd_form = cmd_form,
        input_args = input_args,
        args = file_args,
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
        ext: CxxExtension) -> _CxxCompileCommand:
    """
    Generate a common part of a compile command that is shared by all sources
    with a given extension.
    """
    toolchain = get_cxx_toolchain_info(ctx)
    compiler_info = _get_compiler_info(toolchain, ext)
    base_compile_cmd = _get_compile_base(compiler_info)
    category = _get_category(ext)

    headers_dep_files = None
    dep_file_file_type_hint = _dep_file_type(ext)
    if dep_file_file_type_hint != None and toolchain.use_dep_files:
        tracking_mode = _get_dep_tracking_mode(toolchain, dep_file_file_type_hint)
        mk_dep_files_flags = get_headers_dep_files_flags_factory(tracking_mode)
        if mk_dep_files_flags:
            headers_dep_files = _HeadersDepFiles(
                processor = cmd_args(compiler_info.dep_files_processor),
                mk_flags = mk_dep_files_flags,
                tag = headers_tag,
                dep_tracking_mode = tracking_mode,
            )

    argsfile = _mk_argsfile(ctx, impl_params, compiler_info, pre, ext, headers_tag, False)
    xcode_argsfile = _mk_argsfile(ctx, impl_params, compiler_info, pre, ext, headers_tag, True)

    allow_cache_upload = cxx_attrs_get_allow_cache_upload(ctx.attrs, default = compiler_info.allow_cache_upload)
    return _CxxCompileCommand(
        base_compile_cmd = base_compile_cmd,
        argsfile = argsfile,
        xcode_argsfile = xcode_argsfile,
        headers_dep_files = headers_dep_files,
        compiler_type = compiler_info.compiler_type,
        category = category,
        allow_cache_upload = allow_cache_upload,
    )
