load("@fbcode//buck2/prelude/cxx:cxx_toolchain_types.bzl", "AsCompilerInfo", "AsmCompilerInfo", "BinaryUtilitiesInfo", "CCompilerInfo", "CudaCompilerInfo", "CxxCompilerInfo", "HipCompilerInfo", "LinkerInfo", "StripFlagsInfo", "cxx_toolchain_infos")
load("@fbcode//buck2/prelude/cxx:headers.bzl", "HeaderMode", "HeadersAsRawHeadersMode")
load("@fbcode//buck2/prelude/linking:link_info.bzl", "LinkStyle")
load("@fbcode//buck2/prelude/utils:utils.bzl", "value_or")

def cxx_toolchain_impl(ctx):
    c_info = CCompilerInfo(
        compiler = ctx.attr.c_compiler[RunInfo],
        compiler_type = ctx.attr.c_compiler_type or ctx.attr.compiler_type,
        compiler_flags = cmd_args(ctx.attr.c_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attr.c_preprocessor_flags),
    )
    cxx_info = CxxCompilerInfo(
        compiler = ctx.attr.cxx_compiler[RunInfo],
        compiler_type = ctx.attr.cxx_compiler_type or ctx.attr.compiler_type,
        compiler_flags = cmd_args(ctx.attr.cxx_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attr.cxx_preprocessor_flags),
    )
    asm_info = AsmCompilerInfo(
        compiler = ctx.attr.asm_compiler[RunInfo],
        compiler_type = ctx.attr.asm_compiler_type or ctx.attr.compiler_type,
        compiler_flags = cmd_args(ctx.attr.asm_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attr.asm_preprocessor_flags),
    ) if ctx.attr.asm_compiler else None
    as_info = AsCompilerInfo(
        compiler = ctx.attr.assembler[RunInfo],
        compiler_type = ctx.attr.assembler_type or ctx.attr.compiler_type,
        compiler_flags = cmd_args(ctx.attr.assembler_flags),
        preprocessor_flags = cmd_args(ctx.attr.assembler_preprocessor_flags),
    ) if ctx.attr.assembler else None
    cuda_info = CudaCompilerInfo(
        compiler = ctx.attr.cuda_compiler[RunInfo],
        compiler_type = ctx.attr.cuda_compiler_type or ctx.attr.compiler_type,
        compiler_flags = cmd_args(ctx.attr.cuda_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attr.cuda_preprocessor_flags),
    ) if ctx.attr.cuda_compiler else None
    hip_info = HipCompilerInfo(
        compiler = ctx.attr.hip_compiler[RunInfo],
        compiler_type = ctx.attr.hip_compiler_type or ctx.attr.compiler_type,
        compiler_flags = cmd_args(ctx.attr.hip_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attr.hip_preprocessor_flags),
    ) if ctx.attr.hip_compiler else None

    linker_info = LinkerInfo(
        archiver = ctx.attr.archiver[RunInfo],
        archive_contents = ctx.attr.archive_contents,
        archive_objects_locally = False,
        link_binaries_locally = not value_or(ctx.attr.cache_links, True),
        link_libraries_locally = False,
        link_style = LinkStyle("static"),
        link_weight = 1,
        linker = ctx.attr.linker[RunInfo],
        linker_flags = cmd_args(ctx.attr.linker_flags),
        shlib_interfaces = "disabled",
        independent_shlib_interface_linker_flags = ctx.attr.shared_library_interface_flags,
        requires_archives = value_or(ctx.attr.requires_archives, True),
        requires_objects = value_or(ctx.attr.requires_objects, False),
        supports_distributed_thinlto = ctx.attr.supports_distributed_thinlto,
        shared_dep_runtime_ld_flags = ctx.attr.shared_dep_runtime_ld_flags,
        static_dep_runtime_ld_flags = ctx.attr.static_dep_runtime_ld_flags,
        static_pic_dep_runtime_ld_flags = ctx.attr.static_pic_dep_runtime_ld_flags,
        type = ctx.attr.linker_type,
        use_archiver_flags = ctx.attr.use_archiver_flags,
    )

    utilities_info = BinaryUtilitiesInfo(
        nm = ctx.attr.nm[RunInfo],
        objcopy = ctx.attr.objcopy_for_shared_library_interface[RunInfo],
        ranlib = ctx.attr.ranlib[RunInfo],
        strip = ctx.attr.strip[RunInfo],
        dwp = None,
    )

    strip_flags_info = StripFlagsInfo(
        strip_debug_flags = ctx.attr.strip_debug_flags,
        strip_non_global_flags = ctx.attr.strip_non_global_flags,
        strip_all_flags = ctx.attr.strip_all_flags,
    )

    return [
        DefaultInfo(),
    ] + cxx_toolchain_infos(
        platform_name = ctx.attr.platform_name or ctx.attr.name,
        linker_info = linker_info,
        binary_utilities_info = utilities_info,
        c_compiler_info = c_info,
        cxx_compiler_info = cxx_info,
        asm_compiler_info = asm_info,
        as_compiler_info = as_info,
        cuda_compiler_info = cuda_info,
        hip_compiler_info = hip_info,
        header_mode = _get_header_mode(ctx),
        headers_as_raw_headers_mode = HeadersAsRawHeadersMode(ctx.attr.headers_as_raw_headers_mode) if ctx.attr.headers_as_raw_headers_mode != None else None,
        conflicting_header_basename_allowlist = ctx.attr.conflicting_header_basename_exemptions,
        mk_hmap = ctx.attr._mk_hmap[RunInfo],
        mk_comp_db = ctx.attr._mk_comp_db,
        strip_flags_info = strip_flags_info,
    )

def _get_header_mode(ctx: "context") -> HeaderMode.type:
    if ctx.attr.use_header_map:
        if ctx.attr.private_headers_symlinks_enabled or ctx.attr.public_headers_symlinks_enabled:
            return HeaderMode("symlink_tree_with_header_map")
        else:
            return HeaderMode("header_map_only")
    else:
        return HeaderMode("symlink_tree_only")
