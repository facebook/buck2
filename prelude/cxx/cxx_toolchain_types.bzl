LinkerType = ["gnu", "darwin", "windows"]

# TODO(T110378149): Consider whether it makes sense to move these things to
# configurations/constraints rather than part of the toolchain.
LinkerInfo = provider(fields = [
    "archiver",
    "archive_contents",
    "archive_objects_locally",
    # "archiver_platform",
    # Whether to run native links locally.  We support this for fbcode platforms
    # to avoid issues with C++ static links (see comment in
    # `platform/cxx_toolchains.bzl` for details).
    "link_binaries_locally",
    # Whether to run native shared library links locally. For certain use cases
    # (e.g., large Apple frameworks), it's more efficient to link locally due
    # GiBs of object files (which can also lead to RE errors/timesouts etc).
    "link_libraries_locally",
    "link_style",  # LinkStyle.type
    "link_weight",  # int.type
    "linker",
    "linker_flags",
    "mk_shlib_intf",
    "shlib_interfaces",
    "shared_dep_runtime_ld_flags",
    "static_dep_runtime_ld_flags",
    "static_pic_dep_runtime_ld_flags",
    "requires_archives",
    "requires_objects",
    "supports_distributed_thinlto",
    "independent_shlib_interface_linker_flags",
    "type",  # of "LinkerType" type
    "use_archiver_flags",
])

BinaryUtilitiesInfo = provider(fields = [
    "bolt_msdk",
    "dwp",
    "nm",
    "objcopy",
    "ranlib",
    "strip",
])

StripFlagsInfo = provider(fields = [
    "strip_debug_flags",  # [["str"], None]
    "strip_non_global_flags",  # [["str"], None]
    "strip_all_flags",  # [["str"], None]
])

# TODO(T110378147): There's a bunch of info encoded in random places in buck
# derived from information in these toolchains but hardcoded (for example,
# which file extensions are preprocessable/compilable). We should figure out
# how to move most of that into these toolchain infos.
# TODO(T110378146): The inclusion of compiler and preprocessor in here is really
# just a legacy thing that was never cleaned up. Historically, buck supported a
# mode where compilation was done in two, explicitly separate phases
# (preprocess and then compile). We don't support that today, and including
# both of these mostly just ends up with added complexity and with us
# duplicating flags in command lines.

# In cxx_library, we support a bunch of different types of files (ex. cuda),
# the toolchain for these follow this common pattern.
_compiler_fields = [
    "compiler",
    "compiler_type",
    "compiler_flags",
    "preprocessor",
    "preprocessor_type",
    "preprocessor_flags",
    "dep_files_processor",
]

HipCompilerInfo = provider(fields = _compiler_fields)
CudaCompilerInfo = provider(fields = _compiler_fields)
CCompilerInfo = provider(fields = _compiler_fields)
CxxCompilerInfo = provider(fields = _compiler_fields)
AsmCompilerInfo = provider(fields = _compiler_fields)
AsCompilerInfo = provider(fields = _compiler_fields)

DistLtoToolsInfo = provider(
    fields = ["planner", "opt", "prepare", "copy"],
)

# TODO(T110378094): We should consider if we can change this from a hardcoded
# list of compiler_info to something more general. We could maybe do a list of
# compiler_info where each one also declares what extensions it supports.
# TODO(T110378145): Could we split up this Info so that each of the compilers
# could be provided by different dependencies? That would allow a target to
# only depend on the compilers it actually needs.
CxxToolchainInfo = provider(fields = [
    "conflicting_header_basename_allowlist",
    "use_distributed_thinlto",
    "header_mode",
    "headers_as_raw_headers_mode",
    "linker_info",
    "binary_utilities_info",
    "c_compiler_info",
    "cxx_compiler_info",
    "asm_compiler_info",
    "as_compiler_info",
    "hip_compiler_info",
    "cuda_compiler_info",
    "mk_comp_db",
    "mk_hmap",
    "dist_lto_tools_info",
    "use_dep_files",
    "strip_flags_info",
    "split_dwarf_enabled",
    "bolt_enabled",
])

# Stores "platform"/flavor name used to resolve *platform_* arguments
CxxPlatformInfo = provider(fields = [
    "name",
])

def _validate_linker_info(info: LinkerInfo.type):
    if info.requires_archives and info.requires_objects:
        fail("only one of `requires_archives` and `requires_objects` can be enabled")

    if info.supports_distributed_thinlto and not info.requires_objects:
        fail("distributed thinlto requires enabling `requires_objects`")

def cxx_toolchain_infos(
        platform_name,
        c_compiler_info,
        cxx_compiler_info,
        linker_info,
        binary_utilities_info,
        header_mode,
        headers_as_raw_headers_mode = None,
        conflicting_header_basename_allowlist = [],
        asm_compiler_info = None,
        as_compiler_info = None,
        hip_compiler_info = None,
        cuda_compiler_info = None,
        mk_comp_db = None,
        mk_hmap = None,
        use_distributed_thinlto = False,
        use_dep_files = False,
        strip_flags_info = None,
        dist_lto_tools_info: [DistLtoToolsInfo.type, None] = None,
        split_dwarf_enabled = False,
        bolt_enabled = False):
    """
    Creates the collection of cxx-toolchain Infos for a cxx toolchain.

    c and c++ compiler infos are required, as is a linker info. The rest are
    optional, and it will be an error if any cxx_library or other rules have srcs
    of those other types.
    """

    # TODO(T110378099): verify types of the inner info objects.
    _validate_linker_info(linker_info)

    toolchain_info = CxxToolchainInfo(
        conflicting_header_basename_allowlist = conflicting_header_basename_allowlist,
        header_mode = header_mode,
        headers_as_raw_headers_mode = headers_as_raw_headers_mode,
        linker_info = linker_info,
        binary_utilities_info = binary_utilities_info,
        c_compiler_info = c_compiler_info,
        cxx_compiler_info = cxx_compiler_info,
        asm_compiler_info = asm_compiler_info,
        as_compiler_info = as_compiler_info,
        hip_compiler_info = hip_compiler_info,
        cuda_compiler_info = cuda_compiler_info,
        mk_comp_db = mk_comp_db,
        mk_hmap = mk_hmap,
        dist_lto_tools_info = dist_lto_tools_info,
        use_distributed_thinlto = use_distributed_thinlto,
        use_dep_files = use_dep_files,
        strip_flags_info = strip_flags_info,
        split_dwarf_enabled = split_dwarf_enabled,
        bolt_enabled = bolt_enabled,
    )

    # Provide placeholder mappings, used primarily by cxx_genrule.
    # We don't support these buck1 placeholders since we can't take an argument.
    # $(ldflags-pic-filter <pattern>)
    # $(ldflags-shared-filter <pattern>)
    # $(ldflags-static-filter <pattern>)
    unkeyed_variables = {
        "cc": c_compiler_info.compiler,
        "cflags": _shell_quote(c_compiler_info.compiler_flags),
        "cppflags": _shell_quote(c_compiler_info.preprocessor_flags),
        "cxx": cxx_compiler_info.compiler,
        "cxxflags": _shell_quote(cxx_compiler_info.compiler_flags),
        "cxxppflags": _shell_quote(cxx_compiler_info.preprocessor_flags),
        "ld": linker_info.linker,
        # NOTE(agallagher): The arg-less variants of the ldflags macro are
        # identical, and are just separate to match v1's behavior (ideally,
        # we just have a single `ldflags` macro for this case).
        "ldflags-shared": _shell_quote(linker_info.linker_flags),
        "ldflags-static": _shell_quote(linker_info.linker_flags),
        "ldflags-static-pic": _shell_quote(linker_info.linker_flags),
        # TODO(T110378148): $(platform-name) is almost unusued. Should we remove it?
        "platform-name": platform_name,
    }

    if as_compiler_info != None:
        unkeyed_variables["as"] = as_compiler_info.compiler
        unkeyed_variables["asflags"] = _shell_quote(as_compiler_info.compiler_flags)
        unkeyed_variables["asppflags"] = _shell_quote(as_compiler_info.preprocessor_flags)

    if cuda_compiler_info != None:
        unkeyed_variables["cuda"] = cuda_compiler_info.compiler
        unkeyed_variables["cudaflags"] = _shell_quote(cuda_compiler_info.compiler_flags)
    placeholders_info = TemplatePlaceholderInfo(unkeyed_variables = unkeyed_variables)
    return [toolchain_info, placeholders_info, CxxPlatformInfo(name = platform_name)]

def _shell_quote(xs):
    return cmd_args(xs, quote = "shell")

# export these things under a single "cxx" struct
cxx = struct(
    LinkerType = LinkerType,
    LinkerInfo = LinkerInfo,
    BinaryUtilitiesInfo = BinaryUtilitiesInfo,
    HipCompilerInfo = HipCompilerInfo,
    CudaCompilerInfo = CudaCompilerInfo,
    CCompilerInfo = CCompilerInfo,
    CxxCompilerInfo = CxxCompilerInfo,
    AsmCompilerInfo = AsmCompilerInfo,
    AsCompilerInfo = AsCompilerInfo,
    CxxToolchainInfo = CxxToolchainInfo,
    CxxPlatformInfo = CxxPlatformInfo,
    StripFlagsInfo = StripFlagsInfo,
    DistLtoToolsInfo = DistLtoToolsInfo,
    cxx_toolchain_infos = cxx_toolchain_infos,
)
