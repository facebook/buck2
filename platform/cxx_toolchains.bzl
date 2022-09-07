load("@fbcode//buck2/platform:utils.bzl", "attr_info", "binary_attr", "binary_exec_attr", "bool_attr", "flags_attr", "optional_binary_attr", "optional_binary_exec_attr", "optional_bool_attr", "optional_flags_attr", "optional_int_attr", "optional_string_attr", "read_string", "string_attr")
load("@fbcode_macros//build_defs:fbcode_toolchains.bzl", "fbcode_toolchains")
load("@fbsource//tools/build_defs:dict_defs.bzl", "dict_defs")
load("@fbsource//tools/build_defs:selects.bzl", "selects")
load("@prelude//cxx:cxx_toolchain_types.bzl", "DistLtoToolsInfo")
load("@prelude//cxx:debug.bzl", "SplitDebugMode")
load("@prelude//cxx:headers.bzl", "HeaderMode", "HeadersAsRawHeadersMode")
load("@prelude//linking:link_info.bzl", "LinkStyle")
load("@prelude//linking:lto.bzl", "LtoMode")
load("@prelude//utils:utils.bzl", "expect", "expect_non_none", "value_or")

DEFAULT_MK_COMP_DB = "@prelude//cxx/tools:make_comp_db"
DEFAULT_DIST_LTO_TOOLS = "@prelude//cxx/dist_lto/tools:dist_lto_tools"
DEFAULT_DEP_FILES_PROCESSOR = "@prelude//cxx/tools:makefile_to_dep_file"

# TODO(cjhopman): Figure out a nice default value mechanism for these. Also,
# probably for each "language" type require that the preprocessor or compiler
# is available and if not skip the whole language.

# These were all the buckconfig flags set when checking `buck audit config
# cxx#flavor009-clang`, it might not be comprehensive
def _buckconfig_cxx_toolchain_attrs(is_toolchain):
    tool_attr = binary_exec_attr if is_toolchain else binary_attr
    optional_tool_attr = optional_binary_exec_attr if is_toolchain else optional_binary_attr
    attrs_dep = attrs.exec_dep if is_toolchain else attrs.dep
    return {
        "ar": tool_attr,
        "archive_contents": string_attr,
        "archiver_platform": string_attr,
        "as": tool_attr,
        "as_type": string_attr,
        "asflags": flags_attr,
        "asm": optional_tool_attr,
        "asm_type": optional_string_attr,
        "asmflags": optional_flags_attr,
        "asmpp": optional_tool_attr,
        "asmpp_type": optional_string_attr,
        "asmppflags": optional_flags_attr,
        "aspp": tool_attr,
        "aspp_type": string_attr,
        "asppflags": flags_attr,
        "bolt_enabled": bool_attr,
        "bolt_msdk": tool_attr,
        "cache_links": optional_bool_attr,
        "cc": tool_attr,
        "cc_type": string_attr,
        "cflags": flags_attr,
        "conflicting_header_basename_whitelist": flags_attr,
        "cpp": tool_attr,
        "cpp_type": string_attr,
        "cppflags": flags_attr,
        "cuda": optional_tool_attr,
        "cuda_type": optional_string_attr,
        "cudaflags": optional_flags_attr,
        "cudapp": optional_tool_attr,
        "cudapp_type": optional_string_attr,
        "cudappflags": optional_flags_attr,
        "cxx": tool_attr,
        "cxx_type": string_attr,
        "cxxflags": flags_attr,
        "cxxpp": tool_attr,
        "cxxpp_type": string_attr,
        "cxxppflags": flags_attr,
        "dwp": tool_attr,
        # "exported_headers_symlinks_enabled": string_attr,
        "header_mode": string_attr,
        "headers_as_raw_headers_mode": optional_string_attr,
        "hip": optional_tool_attr,
        "hip_type": optional_string_attr,
        "hipflags": optional_flags_attr,
        "hippp": optional_tool_attr,
        "hippp_type": optional_string_attr,
        "hipppflags": optional_flags_attr,
        "independent_shlib_interface_ldflags": flags_attr,
        "ld": tool_attr,
        "ldflags": flags_attr,
        "link_style": string_attr,
        "link_weight": optional_int_attr,
        "mk_comp_db": attr_info(reader = read_string, attr_type = attrs_dep(providers = [RunInfo], default = DEFAULT_MK_COMP_DB)),
        "mk_shlib_intf": tool_attr,
        "nm": tool_attr,
        "objcopy": tool_attr,
        "ranlib": tool_attr,
        "requires_archives": bool_attr,
        "requires_objects": bool_attr,
        "shlib_interfaces": string_attr,
        # NOTE: This is *not* the same attribute as in v1. In v1 it was used via `fbcode.split-dwarf`
        "split_dwarf_enabled": bool_attr,
        "strip": tool_attr,
        "strip_all_flags": optional_flags_attr,
        "strip_debug_flags": optional_flags_attr,
        "strip_non_global_flags": optional_flags_attr,
        "supports_distributed_thinlto": bool_attr,
        "use_archiver_flags": optional_bool_attr,
        # NOTE: This is *not* the same attribute as in v1. We might eventually make
        # them the same but for now this lets us control dep files specifically in
        # Buck2 since that's a new feature being added..
        "use_dep_files": optional_bool_attr,
        "_dep_files_processor": attr_info(reader = read_string, attr_type = attrs_dep(default = DEFAULT_DEP_FILES_PROCESSOR)),
        "_dist_lto_tools_info": attr_info(reader = read_string, attr_type = attrs_dep(default = DEFAULT_DIST_LTO_TOOLS)),
    }

def _attrs(is_toolchain):
    attrs_dep = attrs.exec_dep if is_toolchain else attrs.dep
    return dict_defs.add(
        {k: v.attr_type for (k, v) in _buckconfig_cxx_toolchain_attrs(is_toolchain).items()},
        dict(
            platform_name = attrs.option(attrs.string()),
            mk_hmap = attrs_dep(providers = [RunInfo], default = "fbsource//xplat/buck2/tools/cxx:hmap_wrapper"),
            lto_mode = attrs.enum(LtoMode.values(), default = "none"),
            split_debug_mode = attrs.enum(SplitDebugMode.values(), default = "none"),
        ),
    )

def _cuda_info(ctx: "context") -> [native.cxx.CudaCompilerInfo.type, None]:
    # If we see a HIP compiler setting, require all other vals are set and fill
    # out a `HipCompilerInfo` provider.
    if ctx.attrs.cuda != None:
        return native.cxx.CudaCompilerInfo(
            compiler = ctx.attrs.cuda[RunInfo],
            compiler_type = expect_non_none(ctx.attrs.cuda_type),
            compiler_flags = cmd_args(expect_non_none(ctx.attrs.cudaflags)),
            preprocessor = expect_non_none(ctx.attrs.cudapp)[RunInfo],
            preprocessor_type = expect_non_none(ctx.attrs.cudapp_type),
            preprocessor_flags = cmd_args(expect_non_none(ctx.attrs.cudappflags)),
            dep_files_processor = ctx.attrs._dep_files_processor[RunInfo],
        )
    else:
        expect(ctx.attrs.cuda_type == None)
        expect(ctx.attrs.cudaflags == None)
        expect(ctx.attrs.cudapp == None)
        expect(ctx.attrs.cudapp_type == None)
        expect(ctx.attrs.cudappflags == None)
        return None

def _hip_info(ctx: "context") -> [native.cxx.HipCompilerInfo.type, None]:
    # If we see a HIP compiler setting, require all other vals are set and fill
    # out a `HipCompilerInfo` provider.
    if ctx.attrs.hip != None:
        return native.cxx.HipCompilerInfo(
            compiler = ctx.attrs.hip[RunInfo],
            compiler_type = expect_non_none(ctx.attrs.hip_type),
            compiler_flags = cmd_args(expect_non_none(ctx.attrs.hipflags)),
            preprocessor = expect_non_none(ctx.attrs.hippp)[RunInfo],
            preprocessor_type = expect_non_none(ctx.attrs.hippp_type),
            preprocessor_flags = cmd_args(expect_non_none(ctx.attrs.hipppflags)),
        )
    else:
        expect(ctx.attrs.hip_type == None)
        expect(ctx.attrs.hipflags == None)
        expect(ctx.attrs.hippp == None)
        expect(ctx.attrs.hippp_type == None)
        expect(ctx.attrs.hipppflags == None)
        return None

def _asm_info(ctx: "context") -> [native.cxx.AsmCompilerInfo.type, None]:
    if ctx.attrs.asm != None:
        return native.cxx.AsmCompilerInfo(
            compiler = ctx.attrs.asm[RunInfo],
            compiler_type = ctx.attrs.asm_type,
            compiler_flags = cmd_args(ctx.attrs.asmflags),
            preprocessor = ctx.attrs.asmpp[RunInfo],
            preprocessor_type = ctx.attrs.asmpp_type,
            preprocessor_flags = cmd_args(ctx.attrs.asmppflags),
            dep_files_processor = ctx.attrs._dep_files_processor[RunInfo],
        )
    else:
        expect(ctx.attrs.asm_type == None)
        expect(ctx.attrs.asmflags == None)
        expect(ctx.attrs.asmpp == None)
        expect(ctx.attrs.asmpp_type == None)
        expect(ctx.attrs.asmppflags == None)
        return None

# TODO(cjhopman): This duplicates a lot of the cxx_toolchain impl. We should
# probably have the config-backed version just convert the config values to
# appropriate cxx_toolchain attrs in the macro layer.
def _cxx_toolchain_impl(ctx):
    c_info = native.cxx.CCompilerInfo(
        compiler = ctx.attrs.cc[RunInfo],
        compiler_type = ctx.attrs.cc_type,
        compiler_flags = cmd_args(ctx.attrs.cflags),
        preprocessor = ctx.attrs.cpp[RunInfo],
        preprocessor_type = ctx.attrs.cpp_type,
        preprocessor_flags = cmd_args(ctx.attrs.cppflags),
        dep_files_processor = ctx.attrs._dep_files_processor[RunInfo],
    )
    cxx_info = native.cxx.CxxCompilerInfo(
        compiler = ctx.attrs.cxx[RunInfo],
        compiler_type = ctx.attrs.cxx_type,
        compiler_flags = cmd_args(ctx.attrs.cxxflags),
        preprocessor = ctx.attrs.cxxpp[RunInfo],
        preprocessor_type = ctx.attrs.cxxpp_type,
        preprocessor_flags = cmd_args(ctx.attrs.cxxppflags),
        dep_files_processor = ctx.attrs._dep_files_processor[RunInfo],
    )
    as_info = native.cxx.AsCompilerInfo(
        compiler = getattr(ctx.attrs, "as")[RunInfo],
        compiler_type = ctx.attrs.as_type,
        compiler_flags = cmd_args(ctx.attrs.asflags),
        preprocessor = ctx.attrs.aspp[RunInfo],
        preprocessor_type = ctx.attrs.aspp_type,
        preprocessor_flags = cmd_args(ctx.attrs.asppflags),
        dep_files_processor = ctx.attrs._dep_files_processor[RunInfo],
    )
    strip_flags_info = native.cxx.StripFlagsInfo(
        strip_debug_flags = ctx.attrs.strip_debug_flags,
        strip_non_global_flags = ctx.attrs.strip_non_global_flags,
        strip_all_flags = ctx.attrs.strip_all_flags,
    )

    linker_info = native.cxx.LinkerInfo(
        archiver = ctx.attrs.ar[RunInfo],
        archive_contents = ctx.attrs.archive_contents,
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
        link_binaries_locally = not value_or(ctx.attrs.cache_links, True),
        link_libraries_locally = False,
        # NOTE(agallagher): It'd be nice to set the default link style via
        # `cxx.link_style`, but opt now to match v1 behavior and avoid breaking
        # existing flows (e.g. some builds in xplat can't currently link
        # dynamically).
        link_style = LinkStyle("static"),
        link_weight = value_or(ctx.attrs.link_weight, 1),
        linker = ctx.attrs.ld[RunInfo],
        linker_flags = cmd_args(ctx.attrs.ldflags),
        lto_mode = LtoMode(ctx.attrs.lto_mode),
        mk_shlib_intf = ctx.attrs.mk_shlib_intf,
        shlib_interfaces = ctx.attrs.shlib_interfaces,
        independent_shlib_interface_linker_flags = ctx.attrs.independent_shlib_interface_ldflags,
        requires_archives = value_or(ctx.attrs.requires_archives, True),
        requires_objects = value_or(ctx.attrs.requires_objects, False),
        supports_distributed_thinlto = ctx.attrs.supports_distributed_thinlto,
        shared_dep_runtime_ld_flags = [],
        static_dep_runtime_ld_flags = [],
        static_pic_dep_runtime_ld_flags = [],
        type = "gnu",
        use_archiver_flags = value_or(ctx.attrs.use_archiver_flags, True),
    )

    utilities_info = native.cxx.BinaryUtilitiesInfo(
        nm = ctx.attrs.nm[RunInfo],
        objcopy = ctx.attrs.objcopy[RunInfo],
        ranlib = ctx.attrs.ranlib[RunInfo],
        strip = ctx.attrs.strip[RunInfo],
        dwp = ctx.attrs.dwp[RunInfo],
        bolt_msdk = ctx.attrs.bolt_msdk[RunInfo],
    )

    # Parse raw headers mode.
    headers_as_raw_headers_mode = None
    if ctx.attrs.headers_as_raw_headers_mode != None:
        headers_as_raw_headers_mode = HeadersAsRawHeadersMode(ctx.attrs.headers_as_raw_headers_mode)

    return [
        DefaultInfo(),
    ] + native.cxx.cxx_toolchain_infos(
        platform_name = ctx.attrs.platform_name if ctx.attrs.platform_name != None else ctx.attrs.name,
        linker_info = linker_info,
        binary_utilities_info = utilities_info,
        bolt_enabled = ctx.attrs.bolt_enabled,
        c_compiler_info = c_info,
        cxx_compiler_info = cxx_info,
        asm_compiler_info = _asm_info(ctx),
        as_compiler_info = as_info,
        cuda_compiler_info = _cuda_info(ctx),
        hip_compiler_info = _hip_info(ctx),
        header_mode = _header_mode_or_default(ctx.attrs.header_mode, c_info.compiler_type, cxx_info.compiler_type),
        headers_as_raw_headers_mode = headers_as_raw_headers_mode,
        conflicting_header_basename_allowlist = ctx.attrs.conflicting_header_basename_whitelist,
        mk_comp_db = ctx.attrs.mk_comp_db,
        mk_hmap = ctx.attrs.mk_hmap[RunInfo],
        dist_lto_tools_info = ctx.attrs._dist_lto_tools_info[DistLtoToolsInfo],
        use_dep_files = value_or(ctx.attrs.use_dep_files, True),
        split_debug_mode = SplitDebugMode(ctx.attrs.split_debug_mode),
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

def _fbcode_config_sections(flavor):
    return [
        "fbcode-platform-cxx#" + flavor,
        "cxx",
    ]

def config_backed_cxx_toolchain(name, flavor, **kwargs):
    sections = _fbcode_config_sections(flavor)

    for (key, info) in _buckconfig_cxx_toolchain_attrs(is_toolchain = False).items():
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
    impl = _cxx_toolchain_impl,
    attrs = _attrs(is_toolchain = False),
)

def _read_config_val(info, key, flavor):
    for section in _fbcode_config_sections(flavor):
        val = info.reader(section, key)
        if val != None:
            return val
    return None

def cxx_fbcode_toolchain(name, **kwargs):
    for (key, info) in _buckconfig_cxx_toolchain_attrs(is_toolchain = True).items():
        if key in kwargs:
            continue

        # TODO(agallagher): We should eventually replace this config-reading
        # select when porting the source of truth from gen_modes.py.
        kwargs[key] = selects.apply(
            fbcode_toolchains.LEGACY_V1_PLATFORMS,
            native.partial(_read_config_val, info, key),
        )

    cxx_toolchain(
        name = name,
        platform_name = fbcode_toolchains.LEGACY_V1_PLATFORMS,
        lto_mode = select({
            "DEFAULT": "none",
            "ovr_config//build_mode/constraints:lto-fat": "fat",
            "ovr_config//build_mode/constraints:lto-monolithic": "monolithic",
            "ovr_config//build_mode/constraints:lto-thin": "thin",
        }),
        # TODO(christylee): fbcode tools rely on the behavior that dwp subtargets
        # are always available, we should change this to use dwp subtargets only
        # when available.
        split_debug_mode = select({
            "DEFAULT": "none",
            "ovr_config//build_mode/constraints:split-dwarf": "single",
        }),
        **kwargs
    )

cxx_toolchain = rule(
    impl = _cxx_toolchain_impl,
    is_toolchain_rule = True,
    attrs = _attrs(is_toolchain = True),
)

def _pick(override, underlying):
    return cmd_args(override) if override != None else underlying

def _pick_bin(override, underlying):
    return override[RunInfo] if override != None else underlying

def _pick_dep(override, underlying):
    return override if override != None else underlying

def _cxx_toolchain_override(ctx):
    base_toolchain = ctx.attrs.base[native.cxx.CxxToolchainInfo]
    base_as_info = base_toolchain.as_compiler_info
    as_info = native.cxx.AsCompilerInfo(
        compiler = _pick_bin(ctx.attrs.as_compiler, base_as_info.compiler),
        compiler_type = base_as_info.compiler_type,
        compiler_flags = _pick(ctx.attrs.as_compiler_flags, base_as_info.compiler_flags),
        preprocessor = _pick_bin(ctx.attrs.as_compiler, base_as_info.preprocessor),
        preprocessor_type = base_as_info.preprocessor_type,
        preprocessor_flags = _pick(ctx.attrs.as_preprocessor_flags, base_as_info.preprocessor_flags),
        dep_files_processor = base_as_info.dep_files_processor,
    )
    base_c_info = base_toolchain.c_compiler_info
    c_info = native.cxx.CCompilerInfo(
        compiler = _pick_bin(ctx.attrs.c_compiler, base_c_info.compiler),
        compiler_type = base_c_info.compiler_type,
        compiler_flags = _pick(ctx.attrs.c_compiler_flags, base_c_info.compiler_flags),
        preprocessor = _pick_bin(ctx.attrs.c_compiler, base_c_info.preprocessor),
        preprocessor_type = base_c_info.preprocessor_type,
        preprocessor_flags = _pick(ctx.attrs.c_preprocessor_flags, base_c_info.preprocessor_flags),
        dep_files_processor = base_c_info.dep_files_processor,
    )
    base_cxx_info = base_toolchain.cxx_compiler_info
    cxx_info = native.cxx.CxxCompilerInfo(
        compiler = _pick_bin(ctx.attrs.cxx_compiler, base_cxx_info.compiler),
        compiler_type = base_cxx_info.compiler_type,
        compiler_flags = _pick(ctx.attrs.cxx_compiler_flags, base_cxx_info.compiler_flags),
        preprocessor = _pick_bin(ctx.attrs.cxx_compiler, base_cxx_info.preprocessor),
        preprocessor_type = base_cxx_info.preprocessor_type,
        preprocessor_flags = _pick(ctx.attrs.cxx_preprocessor_flags, base_cxx_info.preprocessor_flags),
        dep_files_processor = base_cxx_info.dep_files_processor,
    )
    base_linker_info = base_toolchain.linker_info
    linker_info = native.cxx.LinkerInfo(
        archiver = _pick_bin(ctx.attrs.archiver, base_linker_info.archiver),
        archive_contents = base_linker_info.archive_contents,
        archive_objects_locally = value_or(ctx.attrs.archive_objects_locally, base_linker_info.archive_objects_locally),
        link_binaries_locally = value_or(ctx.attrs.link_binaries_locally, base_linker_info.link_binaries_locally),
        link_libraries_locally = value_or(ctx.attrs.link_libraries_locally, base_linker_info.link_libraries_locally),
        link_style = LinkStyle(ctx.attrs.link_style) if ctx.attrs.link_style != None else base_linker_info.link_style,
        link_weight = value_or(ctx.attrs.link_weight, base_linker_info.link_weight),
        linker = _pick_bin(ctx.attrs.linker, base_linker_info.linker),
        linker_flags = _pick(ctx.attrs.linker_flags, base_linker_info.linker_flags),
        lto_mode = LtoMode(value_or(ctx.attrs.lto_mode, base_linker_info.lto_mode.value)),
        shlib_interfaces = base_linker_info.shlib_interfaces,
        mk_shlib_intf = _pick_dep(ctx.attrs.mk_shlib_intf, base_linker_info.mk_shlib_intf),
        requires_archives = base_linker_info.requires_archives,
        requires_objects = base_linker_info.requires_objects,
        supports_distributed_thinlto = base_linker_info.supports_distributed_thinlto,
        independent_shlib_interface_linker_flags = base_linker_info.independent_shlib_interface_linker_flags,
        shared_dep_runtime_ld_flags = [],
        static_dep_runtime_ld_flags = [],
        static_pic_dep_runtime_ld_flags = [],
        type = ctx.attrs.linker_type if ctx.attrs.linker_type != None else base_linker_info.type,
        use_archiver_flags = value_or(ctx.attrs.use_archiver_flags, base_linker_info.use_archiver_flags),
    )

    base_binary_utilities_info = base_toolchain.binary_utilities_info
    binary_utilities_info = native.cxx.BinaryUtilitiesInfo(
        nm = _pick_bin(ctx.attrs.nm, base_binary_utilities_info.nm),
        objcopy = _pick_bin(ctx.attrs.objcopy, base_binary_utilities_info.objcopy),
        ranlib = _pick_bin(ctx.attrs.ranlib, base_binary_utilities_info.ranlib),
        strip = _pick_bin(ctx.attrs.strip, base_binary_utilities_info.strip),
        dwp = base_binary_utilities_info.dwp,
        bolt_msdk = base_binary_utilities_info.bolt_msdk,
    )

    base_strip_flags_info = base_toolchain.strip_flags_info
    strip_flags_info = native.cxx.StripFlagsInfo(
        strip_debug_flags = _pick(ctx.attrs.strip_debug_flags, base_strip_flags_info.strip_debug_flags),
        strip_non_global_flags = _pick(ctx.attrs.strip_non_global_flags, base_strip_flags_info.strip_non_global_flags),
        strip_all_flags = _pick(ctx.attrs.strip_all_flags, base_strip_flags_info.strip_all_flags),
    )

    return [
        DefaultInfo(),
    ] + native.cxx.cxx_toolchain_infos(
        platform_name = ctx.attrs.platform_name if ctx.attrs.platform_name != None else ctx.attrs.base[native.cxx.CxxPlatformInfo].name,
        linker_info = linker_info,
        as_compiler_info = as_info,
        binary_utilities_info = binary_utilities_info,
        bolt_enabled = value_or(ctx.attrs.bolt_enabled, base_toolchain.bolt_enabled),
        c_compiler_info = c_info,
        cxx_compiler_info = cxx_info,
        # the rest are used without overrides
        asm_compiler_info = base_toolchain.asm_compiler_info,
        cuda_compiler_info = base_toolchain.cuda_compiler_info,
        hip_compiler_info = base_toolchain.hip_compiler_info,
        header_mode = HeaderMode(ctx.attrs.header_mode) if ctx.attrs.header_mode != None else base_toolchain.header_mode,
        headers_as_raw_headers_mode = base_toolchain.headers_as_raw_headers_mode,
        mk_comp_db = _pick_bin(ctx.attrs.mk_comp_db, base_toolchain.mk_comp_db),
        mk_hmap = _pick_bin(ctx.attrs.mk_hmap, base_toolchain.mk_hmap),
        dist_lto_tools_info = base_toolchain.dist_lto_tools_info,
        use_dep_files = base_toolchain.use_dep_files,
        conflicting_header_basename_allowlist = base_toolchain.conflicting_header_basename_allowlist,
        strip_flags_info = strip_flags_info,
        split_debug_mode = SplitDebugMode(value_or(ctx.attrs.split_debug_mode, base_toolchain.split_debug_mode.value)),
    )

cxx_toolchain_override = rule(
    impl = _cxx_toolchain_override,
    attrs = {
        "archive_objects_locally": attrs.option(attrs.bool()),
        "archiver": attrs.option(attrs.dep(providers = [RunInfo])),
        "as_compiler": attrs.option(attrs.dep(providers = [RunInfo])),
        "as_compiler_flags": attrs.option(attrs.list(attrs.arg())),
        "as_preprocessor_flags": attrs.option(attrs.list(attrs.arg())),
        "base": attrs.dep(providers = [native.cxx.CxxToolchainInfo]),
        "bolt_enabled": attrs.option(attrs.bool()),
        "c_compiler": attrs.option(attrs.dep(providers = [RunInfo])),
        "c_compiler_flags": attrs.option(attrs.list(attrs.arg())),
        "c_preprocessor_flags": attrs.option(attrs.list(attrs.arg())),
        "cxx_compiler": attrs.option(attrs.dep(providers = [RunInfo])),
        "cxx_compiler_flags": attrs.option(attrs.list(attrs.arg())),
        "cxx_preprocessor_flags": attrs.option(attrs.list(attrs.arg())),
        "header_mode": attrs.option(attrs.enum(HeaderMode.values()), default = None),
        "link_binaries_locally": attrs.option(attrs.bool()),
        "link_libraries_locally": attrs.option(attrs.bool()),
        "link_style": attrs.option(attrs.enum(LinkStyle.values()), default = None),
        "link_weight": attrs.option(attrs.int(), default = None),
        "linker": attrs.option(attrs.dep(providers = [RunInfo])),
        "linker_flags": attrs.option(attrs.list(attrs.arg())),
        "linker_type": attrs.option(attrs.enum(native.cxx.LinkerType), default = None),
        "lto_mode": attrs.option(attrs.enum(LtoMode.values())),
        "mk_comp_db": attrs.option(attrs.dep(providers = [RunInfo], default = DEFAULT_MK_COMP_DB)),
        "mk_hmap": attrs.dep(default = "fbsource//xplat/buck2/tools/cxx:hmap_wrapper"),
        "mk_shlib_intf": attrs.option(attrs.dep(providers = [RunInfo])),
        "nm": attrs.option(attrs.dep(providers = [RunInfo])),
        "objcopy": attrs.option(attrs.dep(providers = [RunInfo])),
        "platform_name": attrs.option(attrs.string(), default = None),
        "ranlib": attrs.option(attrs.dep(providers = [RunInfo])),
        "split_debug_mode": attrs.option(attrs.enum(SplitDebugMode.values())),
        "strip": attrs.option(attrs.dep(providers = [RunInfo])),
        "strip_all_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "strip_debug_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "strip_non_global_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "use_archiver_flags": attrs.option(attrs.bool()),
    },
)

def _cxx_toolchain_prefix(ctx: "context") -> ["provider"]:
    base_toolchain = ctx.attrs.base[native.cxx.CxxToolchainInfo]

    base_c_info = base_toolchain.c_compiler_info
    if ctx.attrs.c_compiler == None:
        c_info = base_c_info
    else:
        c_info = native.cxx.CCompilerInfo(
            compiler = RunInfo(args = cmd_args(ctx.attrs.c_compiler[RunInfo], base_c_info.compiler)),
            compiler_flags = base_c_info.compiler_flags,
            compiler_type = base_c_info.compiler_type,
            preprocessor = base_c_info.preprocessor,
            preprocessor_type = base_c_info.preprocessor_type,
            preprocessor_flags = base_c_info.preprocessor_flags,
            dep_files_processor = base_c_info.dep_files_processor,
        )

    base_cxx_info = base_toolchain.cxx_compiler_info
    if ctx.attrs.cxx_compiler == None:
        cxx_info = base_cxx_info
    else:
        cxx_info = native.cxx.CxxCompilerInfo(
            compiler = RunInfo(args = cmd_args(ctx.attrs.cxx_compiler[RunInfo], base_cxx_info.compiler)),
            compiler_flags = base_cxx_info.compiler_flags,
            compiler_type = base_cxx_info.compiler_type,
            preprocessor = base_cxx_info.preprocessor,
            preprocessor_type = base_cxx_info.preprocessor_type,
            preprocessor_flags = base_cxx_info.preprocessor_flags,
            dep_files_processor = base_cxx_info.dep_files_processor,
        )

    base_linker_info = base_toolchain.linker_info
    if ctx.attrs.linker == None:
        linker_info = base_linker_info
    else:
        linker_info = native.cxx.LinkerInfo(
            linker = RunInfo(args = cmd_args(ctx.attrs.linker[RunInfo], base_linker_info.linker)),
            linker_flags = base_linker_info.linker_flags,
            archiver = base_linker_info.archiver,
            archive_contents = base_linker_info.archive_contents,
            archive_objects_locally = base_linker_info.archive_objects_locally,
            link_binaries_locally = base_linker_info.link_binaries_locally,
            link_libraries_locally = base_linker_info.link_libraries_locally,
            link_style = base_linker_info.link_style,
            link_weight = base_linker_info.link_weight,
            lto_mode = base_linker_info.lto_mode,
            shlib_interfaces = base_linker_info.shlib_interfaces,
            mk_shlib_intf = _pick_dep(ctx.attrs.mk_shlib_intf, base_linker_info.mk_shlib_intf),
            requires_archives = base_linker_info.requires_archives,
            requires_objects = base_linker_info.requires_objects,
            supports_distributed_thinlto = base_linker_info.supports_distributed_thinlto,
            independent_shlib_interface_linker_flags = base_linker_info.independent_shlib_interface_linker_flags,
            shared_dep_runtime_ld_flags = base_linker_info.shared_dep_runtime_ld_flags,
            static_dep_runtime_ld_flags = base_linker_info.static_dep_runtime_ld_flags,
            static_pic_dep_runtime_ld_flags = base_linker_info.static_pic_dep_runtime_ld_flags,
            type = base_linker_info.type,
            use_archiver_flags = base_linker_info.use_archiver_flags,
        )

    return [
        DefaultInfo(),
    ] + native.cxx.cxx_toolchain_infos(
        platform_name = ctx.attrs.platform_name if ctx.attrs.platform_name != None else ctx.attrs.base[native.cxx.CxxPlatformInfo].name,
        linker_info = linker_info,
        c_compiler_info = c_info,
        cxx_compiler_info = cxx_info,
        # the rest are used without overrides
        as_compiler_info = base_toolchain.as_compiler_info,
        asm_compiler_info = base_toolchain.asm_compiler_info,
        binary_utilities_info = base_toolchain.binary_utilities_info,
        bolt_enabled = base_toolchain.bolt_enabled,
        cuda_compiler_info = base_toolchain.cuda_compiler_info,
        hip_compiler_info = base_toolchain.hip_compiler_info,
        header_mode = base_toolchain.header_mode,
        headers_as_raw_headers_mode = base_toolchain.headers_as_raw_headers_mode,
        mk_comp_db = base_toolchain.mk_comp_db,
        mk_hmap = base_toolchain.mk_hmap,
        dist_lto_tools_info = base_toolchain.dist_lto_tools_info,
        use_dep_files = base_toolchain.use_dep_files,
        conflicting_header_basename_allowlist = base_toolchain.conflicting_header_basename_allowlist,
        strip_flags_info = base_toolchain.strip_flags_info,
        split_debug_mode = base_toolchain.split_debug_mode,
    )

cxx_toolchain_prefix = rule(
    impl = _cxx_toolchain_prefix,
    attrs = {
        "base": attrs.dep(providers = [native.cxx.CxxToolchainInfo]),
        "c_compiler": attrs.option(attrs.dep(providers = [RunInfo])),
        "cxx_compiler": attrs.option(attrs.dep(providers = [RunInfo])),
        "linker": attrs.option(attrs.dep(providers = [RunInfo])),
        "mk_shlib_intf": attrs.option(attrs.dep(providers = [RunInfo])),
        "platform_name": attrs.option(attrs.string(), default = None),
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
    impl = _cxx_hacks_impl,
    attrs = {
    },
)
