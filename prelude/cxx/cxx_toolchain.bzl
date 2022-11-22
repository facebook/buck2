# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_toolchain_types.bzl", "AsCompilerInfo", "AsmCompilerInfo", "BinaryUtilitiesInfo", "CCompilerInfo", "CudaCompilerInfo", "CxxCompilerInfo", "HipCompilerInfo", "LinkerInfo", "StripFlagsInfo", "cxx_toolchain_infos")
load("@prelude//cxx:debug.bzl", "SplitDebugMode")
load("@prelude//cxx:headers.bzl", "HeaderMode", "HeadersAsRawHeadersMode")
load("@prelude//linking:link_info.bzl", "LinkStyle")
load("@prelude//linking:lto.bzl", "LtoMode")
load("@prelude//utils:utils.bzl", "value_or")

def cxx_toolchain_impl(ctx):
    c_info = CCompilerInfo(
        compiler = ctx.attrs.c_compiler[RunInfo],
        compiler_type = ctx.attrs.c_compiler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.c_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attrs.c_preprocessor_flags),
    )
    cxx_info = CxxCompilerInfo(
        compiler = ctx.attrs.cxx_compiler[RunInfo],
        compiler_type = ctx.attrs.cxx_compiler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.cxx_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attrs.cxx_preprocessor_flags),
    )
    asm_info = AsmCompilerInfo(
        compiler = ctx.attrs.asm_compiler[RunInfo],
        compiler_type = ctx.attrs.asm_compiler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.asm_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attrs.asm_preprocessor_flags),
    ) if ctx.attrs.asm_compiler else None
    as_info = AsCompilerInfo(
        compiler = ctx.attrs.assembler[RunInfo],
        compiler_type = ctx.attrs.assembler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.assembler_flags),
        preprocessor_flags = cmd_args(ctx.attrs.assembler_preprocessor_flags),
    ) if ctx.attrs.assembler else None
    cuda_info = CudaCompilerInfo(
        compiler = ctx.attrs.cuda_compiler[RunInfo],
        compiler_type = ctx.attrs.cuda_compiler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.cuda_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attrs.cuda_preprocessor_flags),
    ) if ctx.attrs.cuda_compiler else None
    hip_info = HipCompilerInfo(
        compiler = ctx.attrs.hip_compiler[RunInfo],
        compiler_type = ctx.attrs.hip_compiler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.hip_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attrs.hip_preprocessor_flags),
    ) if ctx.attrs.hip_compiler else None

    linker_info = LinkerInfo(
        archiver = ctx.attrs.archiver[RunInfo],
        archiver_supports_argfiles = ctx.attrs.archiver_supports_argfiles,
        archive_contents = ctx.attrs.archive_contents,
        archive_objects_locally = False,
        binary_extension = value_or(ctx.attrs.binary_extension, ""),
        link_binaries_locally = not value_or(ctx.attrs.cache_links, True),
        link_libraries_locally = False,
        link_style = LinkStyle("static"),
        link_weight = 1,
        link_ordering = ctx.attrs.link_ordering,
        linker = ctx.attrs.linker[RunInfo],
        linker_flags = cmd_args(ctx.attrs.linker_flags),
        lto_mode = LtoMode("none"),
        object_file_extension = ctx.attrs.object_file_extension or "o",
        shlib_interfaces = "disabled",
        independent_shlib_interface_linker_flags = ctx.attrs.shared_library_interface_flags,
        requires_archives = value_or(ctx.attrs.requires_archives, True),
        requires_objects = value_or(ctx.attrs.requires_objects, False),
        supports_distributed_thinlto = ctx.attrs.supports_distributed_thinlto,
        shared_dep_runtime_ld_flags = ctx.attrs.shared_dep_runtime_ld_flags,
        shared_library_extension = ctx.attrs.shared_library_extension or "so",
        static_dep_runtime_ld_flags = ctx.attrs.static_dep_runtime_ld_flags,
        static_library_extension = ctx.attrs.static_library_extension or "a",
        static_pic_dep_runtime_ld_flags = ctx.attrs.static_pic_dep_runtime_ld_flags,
        type = ctx.attrs.linker_type,
        use_archiver_flags = ctx.attrs.use_archiver_flags,
    )

    utilities_info = BinaryUtilitiesInfo(
        nm = ctx.attrs.nm[RunInfo],
        objcopy = ctx.attrs.objcopy_for_shared_library_interface[RunInfo],
        ranlib = ctx.attrs.ranlib[RunInfo] if ctx.attrs.ranlib else None,
        strip = ctx.attrs.strip[RunInfo],
        dwp = None,
        bolt_msdk = None,
    )

    strip_flags_info = StripFlagsInfo(
        strip_debug_flags = ctx.attrs.strip_debug_flags,
        strip_non_global_flags = ctx.attrs.strip_non_global_flags,
        strip_all_flags = ctx.attrs.strip_all_flags,
    )

    return [
        DefaultInfo(),
    ] + cxx_toolchain_infos(
        platform_name = ctx.attrs.platform_name or ctx.attrs.name,
        linker_info = linker_info,
        binary_utilities_info = utilities_info,
        bolt_enabled = value_or(ctx.attrs.bolt_enabled, False),
        c_compiler_info = c_info,
        cxx_compiler_info = cxx_info,
        asm_compiler_info = asm_info,
        as_compiler_info = as_info,
        cuda_compiler_info = cuda_info,
        hip_compiler_info = hip_info,
        header_mode = _get_header_mode(ctx),
        headers_as_raw_headers_mode = HeadersAsRawHeadersMode(ctx.attrs.headers_as_raw_headers_mode) if ctx.attrs.headers_as_raw_headers_mode != None else None,
        conflicting_header_basename_allowlist = ctx.attrs.conflicting_header_basename_exemptions,
        mk_hmap = ctx.attrs._mk_hmap[RunInfo],
        mk_comp_db = ctx.attrs._mk_comp_db,
        split_debug_mode = SplitDebugMode(ctx.attrs.split_debug_mode),
        strip_flags_info = strip_flags_info,
    )

def _get_header_mode(ctx: "context") -> HeaderMode.type:
    if ctx.attrs.use_header_map:
        if ctx.attrs.private_headers_symlinks_enabled or ctx.attrs.public_headers_symlinks_enabled:
            return HeaderMode("symlink_tree_with_header_map")
        else:
            return HeaderMode("header_map_only")
    else:
        return HeaderMode("symlink_tree_only")
