# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":argsfiles.bzl", "CompileArgsfile", "CompileArgsfiles")
load(":cxx_toolchain_types.bzl", "CxxObjectFormat", "DepTrackingMode")

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
    ".cl",
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

HeadersDepFiles = record(
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
CxxCompileCommand = record(
    # The compiler and any args which are independent of the rule.
    base_compile_cmd = field(cmd_args),
    # The argsfile of arguments from the rule and it's dependencies.
    argsfile = field(CompileArgsfile),
    # The argsfile to use for Xcode integration.
    xcode_argsfile = field(CompileArgsfile),
    # The argsfile containing header units args.
    header_units_argsfile = field(CompileArgsfile | None),
    headers_dep_files = field([HeadersDepFiles, None]),
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
    cxx_compile_cmd = field(CxxCompileCommand),
    # Arguments specific to the source file.
    args = field(list[typing.Any]),
    # Is this a header file?
    is_header = field(bool, False),
    # The index store factory to use to generate index store for this source file.
    index_store_factory = field(typing.Callable | None, None),
    error_handler = field([typing.Callable, None], None),
)

CxxSrcPrecompileCommand = record(
    # Source file to compile.
    src = field(Artifact),
    # The CxxCompileCommand to use to compile this file.
    cxx_compile_cmd = field(CxxCompileCommand),
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
    base_compile_cmds = field(dict[CxxExtension, CxxCompileCommand], default = {}),
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
    # Plain dump of running NVCC -dryrun which will be used for genearting an
    # NVCC compilation plan.
    nvcc_dryrun = field(Artifact | None, None),
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
