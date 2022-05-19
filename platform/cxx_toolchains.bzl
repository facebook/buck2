load("@fbcode//buck2/platform:utils.bzl", "attr_info", "binary_attr", "bool_attr", "flags_attr", "optional_binary_attr", "optional_bool_attr", "optional_flags_attr", "optional_int_attr", "optional_string_attr", "read_string", "string_attr")
load("@fbcode//buck2/prelude/cxx:headers.bzl", "HeaderMode", "HeadersAsRawHeadersMode")
load("@fbcode//buck2/prelude/linking:link_info.bzl", "LinkStyle")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect", "expect_non_none", "value_or")
load("//buck2/prelude/cxx:cxx_toolchain_types.bzl", "DistLtoToolsInfo")

DEFAULT_MK_COMP_DB = "@fbcode//buck2/prelude/cxx/tools:make_comp_db"
DEFAULT_DIST_LTO_TOOLS = "@fbcode//buck2/prelude/cxx/dist_lto/tools:dist_lto_tools"
DEFAULT_DEP_FILES_PROCESSOR = "@fbcode//buck2/prelude/cxx/tools:makefile_to_dep_file"

# TODO(cjhopman): Figure out a nice default value mechanism for these. Also,
# probably for each "language" type require that the preprocessor or compiler
# is available and if not skip the whole language.

# These were all the buckconfig flags set when checking `buck audit config
# cxx#flavor009-clang`, it might not be comprehensive
_buckconfig_cxx_toolchain_attrs = {
    "ar": binary_attr,
    "archive_contents": string_attr,
    "archiver_platform": string_attr,
    "as": binary_attr,
    "as_type": string_attr,
    "asflags": flags_attr,
    "asm": binary_attr,
    "asm_type": string_attr,
    "asmflags": flags_attr,
    "asmpp": binary_attr,
    "asmpp_type": string_attr,
    "asmppflags": flags_attr,
    "aspp": binary_attr,
    "aspp_type": string_attr,
    "asppflags": flags_attr,
    "cache_links": optional_bool_attr,
    "cc": binary_attr,
    "cc_type": string_attr,
    "cflags": flags_attr,
    "conflicting_header_basename_whitelist": flags_attr,
    "cpp": binary_attr,
    "cpp_type": string_attr,
    "cppflags": flags_attr,
    "cuda": binary_attr,
    "cuda_type": string_attr,
    "cudaflags": flags_attr,
    "cudapp": binary_attr,
    "cudapp_type": string_attr,
    "cudappflags": flags_attr,
    "cxx": binary_attr,
    "cxx_type": string_attr,
    "cxxflags": flags_attr,
    "cxxpp": binary_attr,
    "cxxpp_type": string_attr,
    "cxxppflags": flags_attr,
    "dwp": binary_attr,
    # "exported_headers_symlinks_enabled": string_attr,
    "header_mode": string_attr,
    "headers_as_raw_headers_mode": optional_string_attr,
    "hip": optional_binary_attr,
    "hip_type": optional_string_attr,
    "hipflags": optional_flags_attr,
    "hippp": optional_binary_attr,
    "hippp_type": optional_string_attr,
    "hipppflags": optional_flags_attr,
    "independent_shlib_interface_ldflags": flags_attr,
    "ld": binary_attr,
    "ldflags": flags_attr,
    "link_style": string_attr,
    "link_weight": optional_int_attr,
    "mk_comp_db": attr_info(reader = read_string, attr_type = attr.dep(providers = [RunInfo], default = DEFAULT_MK_COMP_DB)),
    "mk_shlib_intf": binary_attr,
    "nm": binary_attr,
    "objcopy": binary_attr,
    "ranlib": binary_attr,
    "requires_archives": bool_attr,
    "requires_objects": bool_attr,
    "shlib_interfaces": string_attr,
    "strip": binary_attr,
    "strip_all_flags": optional_flags_attr,
    "strip_debug_flags": optional_flags_attr,
    "strip_non_global_flags": optional_flags_attr,
    "supports_distributed_thinlto": bool_attr,
    "use_archiver_flags": optional_bool_attr,
    # NOTE: This is *not* the same attribute as in v1. We might eventually make
    # them the same but for now this lets us control dep files specifically in
    # Buck2 since that's a new feature being added..
    "use_dep_files": optional_bool_attr,
    "_dep_files_processor": attr_info(reader = read_string, attr_type = attr.dep(default = DEFAULT_DEP_FILES_PROCESSOR)),
    "_dist_lto_tools_info": attr_info(reader = read_string, attr_type = attr.dep(default = DEFAULT_DIST_LTO_TOOLS)),
}

def _hip_info(ctx: "context") -> [native.cxx.HipCompilerInfo.type, None]:
    # If we see a HIP compiler setting, require all other vals are set and fill
    # out a `HipCompilerInfo` provider.
    if ctx.attr.hip != None:
        return native.cxx.HipCompilerInfo(
            compiler = ctx.attr.hip[RunInfo],
            compiler_type = expect_non_none(ctx.attr.hip_type),
            compiler_flags = cmd_args(expect_non_none(ctx.attr.hipflags)),
            preprocessor = expect_non_none(ctx.attr.hippp)[RunInfo],
            preprocessor_type = expect_non_none(ctx.attr.hippp_type),
            preprocessor_flags = cmd_args(expect_non_none(ctx.attr.hipppflags)),
        )
    else:
        expect(ctx.attr.hip_type == None)
        expect(ctx.attr.hipflags == None)
        expect(ctx.attr.hippp == None)
        expect(ctx.attr.hippp_type == None)
        expect(ctx.attr.hipppflags == None)
        return None

# TODO(cjhopman): This duplicates a lot of the cxx_toolchain impl. We should
# probably have the config-backed version just convert the config values to
# appropriate cxx_toolchain attrs in the macro layer.
def _config_backed_toolchain_impl(ctx):
    c_info = native.cxx.CCompilerInfo(
        compiler = ctx.attr.cc[RunInfo],
        compiler_type = ctx.attr.cc_type,
        compiler_flags = cmd_args(ctx.attr.cflags),
        preprocessor = ctx.attr.cpp[RunInfo],
        preprocessor_type = ctx.attr.cpp_type,
        preprocessor_flags = cmd_args(ctx.attr.cppflags),
        dep_files_processor = ctx.attr._dep_files_processor[RunInfo],
    )
    cxx_info = native.cxx.CxxCompilerInfo(
        compiler = ctx.attr.cxx[RunInfo],
        compiler_type = ctx.attr.cxx_type,
        compiler_flags = cmd_args(ctx.attr.cxxflags),
        preprocessor = ctx.attr.cxxpp[RunInfo],
        preprocessor_type = ctx.attr.cxxpp_type,
        preprocessor_flags = cmd_args(ctx.attr.cxxppflags),
        dep_files_processor = ctx.attr._dep_files_processor[RunInfo],
    )
    asm_info = native.cxx.AsmCompilerInfo(
        compiler = ctx.attr.asm[RunInfo],
        compiler_type = ctx.attr.asm_type,
        compiler_flags = cmd_args(ctx.attr.asmflags),
        preprocessor = ctx.attr.asmpp[RunInfo],
        preprocessor_type = ctx.attr.asmpp_type,
        preprocessor_flags = cmd_args(ctx.attr.asmppflags),
        dep_files_processor = ctx.attr._dep_files_processor[RunInfo],
    )
    as_info = native.cxx.AsCompilerInfo(
        compiler = getattr(ctx.attr, "as")[RunInfo],
        compiler_type = ctx.attr.as_type,
        compiler_flags = cmd_args(ctx.attr.asflags),
        preprocessor = ctx.attr.aspp[RunInfo],
        preprocessor_type = ctx.attr.aspp_type,
        preprocessor_flags = cmd_args(ctx.attr.asppflags),
        dep_files_processor = ctx.attr._dep_files_processor[RunInfo],
    )
    cuda_info = native.cxx.CudaCompilerInfo(
        compiler = ctx.attr.cuda[RunInfo],
        compiler_type = ctx.attr.cuda_type,
        compiler_flags = cmd_args(ctx.attr.cudaflags),
        preprocessor = ctx.attr.cudapp[RunInfo],
        preprocessor_type = ctx.attr.cudapp_type,
        preprocessor_flags = cmd_args(ctx.attr.cudappflags),
        dep_files_processor = ctx.attr._dep_files_processor[RunInfo],
    )
    strip_flags_info = native.cxx.StripFlagsInfo(
        strip_debug_flags = ctx.attr.strip_debug_flags,
        strip_non_global_flags = ctx.attr.strip_non_global_flags,
        strip_all_flags = ctx.attr.strip_all_flags,
    )

    linker_info = native.cxx.LinkerInfo(
        archiver = ctx.attr.ar[RunInfo],
        archive_contents = ctx.attr.archive_contents,
        # This is a v2-only setting that does not have an equivalent v1
        # (nor do we want to introduce a v1 config for it)
        archive_objects_locally = False,
        # In v1, fbcode platforms set `cxx.cache_links = false` for statically
        # linked modes, to avoid scaling issues with statically linked native
        # binaries:
        # 1) We rely on non-deterministic build info stamping which is performed
        #    at link time, and which isn't currently supported on RE.
        # 2) Bigger binary rules are currently too large to link on RE:
        #    https://fb.prod.workplace.com/groups/2841112232843497/posts/2953023738319012.
        # 3) Currently, running static links remotely means we cache them in RE
        #    and static binaries have inherent scalability issues which can trample
        #    caches (e.g. T10696178).
        link_binaries_locally = not value_or(ctx.attr.cache_links, True),
        link_libraries_locally = False,
        # NOTE(agallagher): It'd be nice to set the default link style via
        # `cxx.link_style`, but opt now to match v1 behavior and avoid breaking
        # existing flows (e.g. some builds in xplat can't currently link
        # dynamically).
        link_style = LinkStyle("static"),
        link_weight = value_or(ctx.attr.link_weight, 1),
        linker = ctx.attr.ld[RunInfo],
        linker_flags = cmd_args(ctx.attr.ldflags),
        mk_shlib_intf = ctx.attr.mk_shlib_intf,
        shlib_interfaces = ctx.attr.shlib_interfaces,
        independent_shlib_interface_linker_flags = ctx.attr.independent_shlib_interface_ldflags,
        requires_archives = value_or(ctx.attr.requires_archives, True),
        requires_objects = value_or(ctx.attr.requires_objects, False),
        supports_distributed_thinlto = ctx.attr.supports_distributed_thinlto,
        shared_dep_runtime_ld_flags = [],
        static_dep_runtime_ld_flags = [],
        static_pic_dep_runtime_ld_flags = [],
        type = "gnu",
        use_archiver_flags = value_or(ctx.attr.use_archiver_flags, True),
    )

    utilities_info = native.cxx.BinaryUtilitiesInfo(
        nm = ctx.attr.nm[RunInfo],
        objcopy = ctx.attr.objcopy[RunInfo],
        ranlib = ctx.attr.ranlib[RunInfo],
        strip = ctx.attr.strip[RunInfo],
        dwp = ctx.attr.dwp[RunInfo],
    )

    # Parse raw headers mode.
    headers_as_raw_headers_mode = None
    if ctx.attr.headers_as_raw_headers_mode != None:
        headers_as_raw_headers_mode = HeadersAsRawHeadersMode(ctx.attr.headers_as_raw_headers_mode)

    return [
        DefaultInfo(),
    ] + native.cxx.cxx_toolchain_infos(
        platform_name = ctx.attr.name,
        linker_info = linker_info,
        binary_utilities_info = utilities_info,
        c_compiler_info = c_info,
        cxx_compiler_info = cxx_info,
        asm_compiler_info = asm_info,
        as_compiler_info = as_info,
        cuda_compiler_info = cuda_info,
        hip_compiler_info = _hip_info(ctx),
        header_mode = _header_mode_or_default(ctx.attr.header_mode, c_info.compiler_type, cxx_info.compiler_type),
        headers_as_raw_headers_mode = headers_as_raw_headers_mode,
        conflicting_header_basename_allowlist = ctx.attr.conflicting_header_basename_whitelist,
        mk_comp_db = ctx.attr.mk_comp_db,
        dist_lto_tools_info = ctx.attr._dist_lto_tools_info[DistLtoToolsInfo],
        use_dep_files = value_or(ctx.attr.use_dep_files, True),
        strip_flags_info = strip_flags_info,
    )

def _header_mode_or_default(header_mode_value: [None, str.type], c_compiler_type: str.type, cxx_compiler_type: str.type) -> HeaderMode.type:
    # Parse header mode.
    if header_mode_value != None:
        return HeaderMode(header_mode_value)
    elif c_compiler_type == "clang" and cxx_compiler_type == "clang":
        return HeaderMode("symlink-tree-with-header-map")
    else:
        return HeaderMode("symlink-tree-only")

def config_backed_cxx_toolchain(name, flavor, **kwargs):
    sections = ["fbcode-platform-cxx#" + flavor, "cxx"]

    for (key, info) in _buckconfig_cxx_toolchain_attrs.items():
        if key in kwargs:
            continue
        val = None
        for section in sections:
            val = info.reader(section, key)
            if val != None:
                break
        kwargs[key] = val

    _config_backed_cxx_toolchain_rule(
        name = name,
        **kwargs
    )

_config_backed_cxx_toolchain_rule = rule(
    implementation = _config_backed_toolchain_impl,
    attrs = {k: v.attr_type for (k, v) in _buckconfig_cxx_toolchain_attrs.items()},
)

def _pick(override, underlying):
    return cmd_args(override) if override != None else underlying

def _pick_bin(override, underlying):
    return override[RunInfo] if override != None else underlying

def _pick_dep(override, underlying):
    return override if override != None else underlying

def _cxx_toolchain_override(ctx):
    base_toolchain = ctx.attr.base[native.cxx.CxxToolchainInfo]
    base_as_info = base_toolchain.as_compiler_info
    as_info = native.cxx.AsCompilerInfo(
        compiler = _pick_bin(ctx.attr.as_compiler, base_as_info.compiler),
        compiler_type = base_as_info.compiler_type,
        compiler_flags = _pick(ctx.attr.as_compiler_flags, base_as_info.compiler_flags),
        preprocessor = _pick_bin(ctx.attr.as_compiler, base_as_info.preprocessor),
        preprocessor_type = base_as_info.preprocessor_type,
        preprocessor_flags = _pick(ctx.attr.as_preprocessor_flags, base_as_info.preprocessor_flags),
        dep_files_processor = base_as_info.dep_files_processor,
    )
    base_c_info = base_toolchain.c_compiler_info
    c_info = native.cxx.CCompilerInfo(
        compiler = _pick_bin(ctx.attr.c_compiler, base_c_info.compiler),
        compiler_type = base_c_info.compiler_type,
        compiler_flags = _pick(ctx.attr.c_compiler_flags, base_c_info.compiler_flags),
        preprocessor = _pick_bin(ctx.attr.c_compiler, base_c_info.preprocessor),
        preprocessor_type = base_c_info.preprocessor_type,
        preprocessor_flags = _pick(ctx.attr.c_preprocessor_flags, base_c_info.preprocessor_flags),
        dep_files_processor = base_c_info.dep_files_processor,
    )
    base_cxx_info = base_toolchain.cxx_compiler_info
    cxx_info = native.cxx.CxxCompilerInfo(
        compiler = _pick_bin(ctx.attr.cxx_compiler, base_cxx_info.compiler),
        compiler_type = base_cxx_info.compiler_type,
        compiler_flags = _pick(ctx.attr.cxx_compiler_flags, base_cxx_info.compiler_flags),
        preprocessor = _pick_bin(ctx.attr.cxx_compiler, base_cxx_info.preprocessor),
        preprocessor_type = base_cxx_info.preprocessor_type,
        preprocessor_flags = _pick(ctx.attr.cxx_preprocessor_flags, base_cxx_info.preprocessor_flags),
        dep_files_processor = base_cxx_info.dep_files_processor,
    )
    base_linker_info = base_toolchain.linker_info
    linker_info = native.cxx.LinkerInfo(
        archiver = _pick_bin(ctx.attr.archiver, base_linker_info.archiver),
        archive_contents = base_linker_info.archive_contents,
        archive_objects_locally = value_or(ctx.attr.archive_objects_locally, base_linker_info.archive_objects_locally),
        link_binaries_locally = value_or(ctx.attr.link_binaries_locally, base_linker_info.link_binaries_locally),
        link_libraries_locally = value_or(ctx.attr.link_libraries_locally, base_linker_info.link_libraries_locally),
        link_style = LinkStyle(ctx.attr.link_style) if ctx.attr.link_style != None else base_linker_info.link_style,
        link_weight = value_or(ctx.attr.link_weight, base_linker_info.link_weight),
        linker = _pick_bin(ctx.attr.linker, base_linker_info.linker),
        linker_flags = _pick(ctx.attr.linker_flags, base_linker_info.linker_flags),
        shlib_interfaces = base_linker_info.shlib_interfaces,
        mk_shlib_intf = _pick_dep(ctx.attr.mk_shlib_intf, base_linker_info.mk_shlib_intf),
        requires_archives = base_linker_info.requires_archives,
        requires_objects = base_linker_info.requires_objects,
        supports_distributed_thinlto = base_linker_info.supports_distributed_thinlto,
        independent_shlib_interface_linker_flags = base_linker_info.independent_shlib_interface_linker_flags,
        shared_dep_runtime_ld_flags = [],
        static_dep_runtime_ld_flags = [],
        static_pic_dep_runtime_ld_flags = [],
        type = ctx.attr.linker_type if ctx.attr.linker_type != None else base_linker_info.type,
        use_archiver_flags = value_or(ctx.attr.use_archiver_flags, base_linker_info.use_archiver_flags),
    )

    base_binary_utilities_info = base_toolchain.binary_utilities_info
    binary_utilities_info = native.cxx.BinaryUtilitiesInfo(
        nm = _pick_bin(ctx.attr.nm, base_binary_utilities_info.nm),
        objcopy = _pick_bin(ctx.attr.objcopy, base_binary_utilities_info.objcopy),
        ranlib = _pick_bin(ctx.attr.ranlib, base_binary_utilities_info.ranlib),
        strip = _pick_bin(ctx.attr.strip, base_binary_utilities_info.strip),
        dwp = base_binary_utilities_info.dwp,
    )

    base_strip_flags_info = base_toolchain.strip_flags_info
    strip_flags_info = native.cxx.StripFlagsInfo(
        strip_debug_flags = _pick(ctx.attr.strip_debug_flags, base_strip_flags_info.strip_debug_flags),
        strip_non_global_flags = _pick(ctx.attr.strip_non_global_flags, base_strip_flags_info.strip_non_global_flags),
        strip_all_flags = _pick(ctx.attr.strip_all_flags, base_strip_flags_info.strip_all_flags),
    )

    return [
        DefaultInfo(),
    ] + native.cxx.cxx_toolchain_infos(
        platform_name = ctx.attr.platform_name if ctx.attr.platform_name != None else ctx.attr.base[native.cxx.CxxPlatformInfo].name,
        linker_info = linker_info,
        as_compiler_info = as_info,
        binary_utilities_info = binary_utilities_info,
        c_compiler_info = c_info,
        cxx_compiler_info = cxx_info,
        # the rest are used without overrides
        asm_compiler_info = base_toolchain.asm_compiler_info,
        cuda_compiler_info = base_toolchain.cuda_compiler_info,
        hip_compiler_info = base_toolchain.hip_compiler_info,
        header_mode = HeaderMode(ctx.attr.header_mode) if ctx.attr.header_mode != None else base_toolchain.header_mode,
        headers_as_raw_headers_mode = base_toolchain.headers_as_raw_headers_mode,
        mk_comp_db = _pick_bin(ctx.attr.mk_comp_db, base_toolchain.mk_comp_db),
        mk_hmap = _pick_bin(ctx.attr.mk_hmap, base_toolchain.mk_hmap),
        dist_lto_tools_info = base_toolchain.dist_lto_tools_info,
        use_dep_files = base_toolchain.use_dep_files,
        conflicting_header_basename_allowlist = base_toolchain.conflicting_header_basename_allowlist,
        strip_flags_info = strip_flags_info,
    )

cxx_toolchain_override = rule(
    implementation = _cxx_toolchain_override,
    attrs = {
        "archive_objects_locally": attr.option(attr.bool()),
        "archiver": attr.option(attr.dep(providers = [RunInfo])),
        "as_compiler": attr.option(attr.dep(providers = [RunInfo])),
        "as_compiler_flags": attr.option(attr.list(attr.arg())),
        "as_preprocessor_flags": attr.option(attr.list(attr.arg())),
        "base": attr.dep(providers = [native.cxx.CxxToolchainInfo]),
        "c_compiler": attr.option(attr.dep(providers = [RunInfo])),
        "c_compiler_flags": attr.option(attr.list(attr.arg())),
        "c_preprocessor_flags": attr.option(attr.list(attr.arg())),
        "cxx_compiler": attr.option(attr.dep(providers = [RunInfo])),
        "cxx_compiler_flags": attr.option(attr.list(attr.arg())),
        "cxx_preprocessor_flags": attr.option(attr.list(attr.arg())),
        "header_mode": attr.option(attr.enum(HeaderMode.values()), default = None),
        "link_binaries_locally": attr.option(attr.bool()),
        "link_libraries_locally": attr.option(attr.bool()),
        "link_style": attr.option(attr.enum(LinkStyle.values()), default = None),
        "link_weight": attr.option(attr.int(), default = None),
        "linker": attr.option(attr.dep(providers = [RunInfo])),
        "linker_flags": attr.option(attr.list(attr.arg())),
        "linker_type": attr.option(attr.enum(native.cxx.LinkerType), default = None),
        "mk_comp_db": attr.option(attr.dep(providers = [RunInfo], default = DEFAULT_MK_COMP_DB)),
        "mk_hmap": attr.dep(default = "fbsource//xplat/buck2/tools/cxx:hmap_wrapper"),
        "mk_shlib_intf": attr.option(attr.dep(providers = [RunInfo])),
        "nm": attr.option(attr.dep(providers = [RunInfo])),
        "objcopy": attr.option(attr.dep(providers = [RunInfo])),
        "platform_name": attr.option(attr.string(), default = None),
        "ranlib": attr.option(attr.dep(providers = [RunInfo])),
        "strip": attr.option(attr.dep(providers = [RunInfo])),
        "strip_all_flags": attr.option(attr.list(attr.arg()), default = None),
        "strip_debug_flags": attr.option(attr.list(attr.arg()), default = None),
        "strip_non_global_flags": attr.option(attr.list(attr.arg()), default = None),
        "use_archiver_flags": attr.option(attr.bool()),
    },
)

def _cxx_hacks_impl(_ctx):
    return [DefaultInfo(), TemplatePlaceholderInfo(
        unkeyed_variables = {
            "cxx-header-tree": "/dev/null/HACK-CXX-HEADER-TREE",
            "output-dwo-dir": "/dev/null/HACK-OUTPUT-DWO-DIR",
        },
    )]

cxx_hacks = rule(
    implementation = _cxx_hacks_impl,
    attrs = {
    },
)
