# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:is_full_meta_repo.bzl", "is_full_meta_repo")
load(
    "@prelude//cxx:cxx_toolchain_types.bzl",
    "AsCompilerInfo",
    "AsmCompilerInfo",
    "BinaryUtilitiesInfo",
    "CCompilerInfo",
    "CudaCompilerInfo",
    "CvtresCompilerInfo",
    "CxxCompilerInfo",
    "CxxInternalTools",
    "CxxObjectFormat",
    "DepTrackingMode",
    "HipCompilerInfo",
    "LinkerInfo",
    "LinkerType",
    "ObjcCompilerInfo",
    "ObjcxxCompilerInfo",
    "PicBehavior",
    "RcCompilerInfo",
    "ShlibInterfacesMode",
    "StripFlagsInfo",
    "cxx_toolchain_infos",
)
load("@prelude//cxx:cxx_utility.bzl", "cxx_toolchain_allow_cache_upload_args")
load("@prelude//cxx:debug.bzl", "SplitDebugMode")
load("@prelude//cxx:headers.bzl", "HeaderMode", "HeadersAsRawHeadersMode", "RawHeadersAsHeadersMode")
load("@prelude//cxx:linker.bzl", "LINKERS", "is_pdb_generated")
load("@prelude//cxx:target_sdk_version.bzl", "get_toolchain_target_sdk_version")
load("@prelude//decls:cxx_rules.bzl", "cxx_rules")
load("@prelude//linking:link_info.bzl", "LinkOrdering", "LinkStyle")
load("@prelude//linking:lto.bzl", "LtoMode", "lto_compiler_flags")
load("@prelude//utils:utils.bzl", "flatten", "value_or")

def cxx_toolchain_impl(ctx):
    c_compiler = _get_maybe_wrapped_msvc(ctx.attrs.c_compiler[RunInfo], ctx.attrs.c_compiler_type or ctx.attrs.compiler_type, ctx.attrs._msvc_hermetic_exec[RunInfo])

    lto_mode = LtoMode(ctx.attrs.lto_mode)
    if lto_mode != LtoMode("none"):
        object_format = "bitcode"
    else:
        object_format = ctx.attrs.object_format if ctx.attrs.object_format else "native"

    c_lto_flags = lto_compiler_flags(lto_mode)

    c_info = CCompilerInfo(
        compiler = c_compiler,
        compiler_type = ctx.attrs.c_compiler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.c_compiler_flags, c_lto_flags),
        preprocessor = c_compiler,
        preprocessor_flags = cmd_args(ctx.attrs.c_preprocessor_flags),
        allow_cache_upload = ctx.attrs.c_compiler_allow_cache_upload,
    )
    objc_info = ObjcCompilerInfo(
        compiler = c_compiler,
        compiler_type = ctx.attrs.c_compiler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.c_compiler_flags, c_lto_flags, ctx.attrs.objc_compiler_flags),
        preprocessor = c_compiler,
        preprocessor_flags = cmd_args(ctx.attrs.c_preprocessor_flags),
        allow_cache_upload = ctx.attrs.c_compiler_allow_cache_upload,
    )
    cxx_compiler = _get_maybe_wrapped_msvc(ctx.attrs.cxx_compiler[RunInfo], ctx.attrs.cxx_compiler_type or ctx.attrs.compiler_type, ctx.attrs._msvc_hermetic_exec[RunInfo])
    cxx_info = CxxCompilerInfo(
        compiler = cxx_compiler,
        compiler_type = ctx.attrs.cxx_compiler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.cxx_compiler_flags, c_lto_flags),
        preprocessor = cxx_compiler,
        preprocessor_flags = cmd_args(ctx.attrs.cxx_preprocessor_flags),
        allow_cache_upload = ctx.attrs.cxx_compiler_allow_cache_upload,
        supports_two_phase_compilation = ctx.attrs.supports_two_phase_compilation,
    )
    objcxx_info = ObjcxxCompilerInfo(
        compiler = cxx_compiler,
        compiler_type = ctx.attrs.cxx_compiler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.cxx_compiler_flags, c_lto_flags, ctx.attrs.objcxx_compiler_flags),
        preprocessor = cxx_compiler,
        preprocessor_flags = cmd_args(ctx.attrs.cxx_preprocessor_flags),
        allow_cache_upload = ctx.attrs.cxx_compiler_allow_cache_upload,
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
        allow_cache_upload = ctx.attrs.cuda_compiler_allow_cache_upload,
    ) if ctx.attrs.cuda_compiler else None
    hip_info = HipCompilerInfo(
        compiler = ctx.attrs.hip_compiler[RunInfo],
        compiler_type = ctx.attrs.hip_compiler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.hip_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attrs.hip_preprocessor_flags),
    ) if ctx.attrs.hip_compiler else None
    cvtres_info = CvtresCompilerInfo(
        compiler = ctx.attrs.cvtres_compiler[RunInfo],
        compiler_type = ctx.attrs.cvtres_compiler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.cvtres_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attrs.cvtres_preprocessor_flags),
    ) if ctx.attrs.cvtres_compiler else None
    rc_info = RcCompilerInfo(
        compiler = ctx.attrs.rc_compiler[RunInfo],
        compiler_type = ctx.attrs.rc_compiler_type or ctx.attrs.compiler_type,
        compiler_flags = cmd_args(ctx.attrs.rc_compiler_flags),
        preprocessor_flags = cmd_args(ctx.attrs.rc_preprocessor_flags),
    ) if ctx.attrs.rc_compiler else None

    linker_type = LinkerType(ctx.attrs.linker_type)
    linker_info = LinkerInfo(
        archiver = ctx.attrs.archiver[RunInfo],
        archiver_flags = cmd_args(ctx.attrs.archiver_flags),
        archiver_reads_inputs = ctx.attrs.archiver_reads_inputs,
        archiver_supports_argfiles = ctx.attrs.archiver_supports_argfiles,
        archiver_type = ctx.attrs.archiver_type,
        archive_contents = ctx.attrs.archive_contents,
        archive_objects_locally = False,
        archive_symbol_table = ctx.attrs.archive_symbol_table,
        binary_extension = value_or(ctx.attrs.binary_extension, ""),
        generate_linker_maps = ctx.attrs.generate_linker_maps,
        is_pdb_generated = is_pdb_generated(linker_type, ctx.attrs.linker_flags),
        link_binaries_locally = not value_or(ctx.attrs.cache_links, True),
        link_libraries_locally = False,
        link_style = LinkStyle(ctx.attrs.link_style),
        link_weight = ctx.attrs.link_weight,
        link_ordering = ctx.attrs.link_ordering,
        linker = ctx.attrs.linker[RunInfo],
        linker_flags = cmd_args(ctx.attrs.linker_flags, c_lto_flags),
        executable_linker_flags = ctx.attrs.executable_linker_flags,
        binary_linker_flags = ctx.attrs.binary_linker_flags,
        dist_thin_lto_codegen_flags = cmd_args(ctx.attrs.dist_thin_lto_codegen_flags) if ctx.attrs.dist_thin_lto_codegen_flags else cmd_args(),
        post_linker_flags = cmd_args(ctx.attrs.post_linker_flags),
        link_metadata_flag = ctx.attrs.link_metadata_flag,
        lto_mode = lto_mode,
        mk_shlib_intf = ctx.attrs.shared_library_interface_producer,
        object_file_extension = ctx.attrs.object_file_extension or "o",
        shlib_interfaces = ShlibInterfacesMode(ctx.attrs.shared_library_interface_mode),
        independent_shlib_interface_linker_flags = ctx.attrs.shared_library_interface_flags,
        requires_archives = value_or(ctx.attrs.requires_archives, True),
        requires_objects = value_or(ctx.attrs.requires_objects, False),
        sanitizer_runtime_enabled = ctx.attrs.sanitizer_runtime_enabled,
        sanitizer_runtime_files = flatten([runtime_file[DefaultInfo].default_outputs for runtime_file in ctx.attrs.sanitizer_runtime_files]),
        supports_distributed_thinlto = ctx.attrs.supports_distributed_thinlto,
        shared_dep_runtime_ld_flags = ctx.attrs.shared_dep_runtime_ld_flags,
        shared_library_name_default_prefix = _get_shared_library_name_default_prefix(ctx),
        shared_library_name_format = _get_shared_library_name_format(ctx),
        shared_library_versioned_name_format = _get_shared_library_versioned_name_format(ctx),
        static_dep_runtime_ld_flags = ctx.attrs.static_dep_runtime_ld_flags,
        static_library_extension = ctx.attrs.static_library_extension or "a",
        static_pic_dep_runtime_ld_flags = ctx.attrs.static_pic_dep_runtime_ld_flags,
        thin_lto_premerger_enabled = ctx.attrs.thin_lto_premerger_enabled,
        thin_lto_double_codegen_enabled = ctx.attrs.thin_lto_double_codegen_enabled,
        type = linker_type,
        use_archiver_flags = ctx.attrs.use_archiver_flags,
    )

    utilities_info = BinaryUtilitiesInfo(
        bolt = ctx.attrs.bolt[RunInfo] if ctx.attrs.bolt else None,
        custom_tools = {name: dep[RunInfo] for name, dep in ctx.attrs.custom_tools.items()},
        nm = ctx.attrs.nm[RunInfo],
        objcopy = ctx.attrs.objcopy_for_shared_library_interface[RunInfo],
        objdump = ctx.attrs.objdump[RunInfo] if ctx.attrs.objdump else None,
        ranlib = ctx.attrs.ranlib[RunInfo] if ctx.attrs.ranlib else None,
        strip = ctx.attrs.strip[RunInfo],
        dwp = ctx.attrs.dwp[RunInfo] if ctx.attrs.dwp else None,
        bolt_msdk = None,
    )

    strip_flags_info = StripFlagsInfo(
        strip_debug_flags = ctx.attrs.strip_debug_flags,
        strip_non_global_flags = ctx.attrs.strip_non_global_flags,
        strip_all_flags = ctx.attrs.strip_all_flags,
    )

    platform_name = ctx.attrs.platform_name or ctx.attrs.name
    return [
        DefaultInfo(),
    ] + cxx_toolchain_infos(
        as_compiler_info = as_info,
        asm_compiler_info = asm_info,
        binary_utilities_info = utilities_info,
        bolt_enabled = value_or(ctx.attrs.bolt_enabled, False),
        c_compiler_info = c_info,
        clang_remarks = ctx.attrs.clang_remarks,
        clang_trace = value_or(ctx.attrs.clang_trace, False),
        cpp_dep_tracking_mode = DepTrackingMode(ctx.attrs.cpp_dep_tracking_mode),
        cuda_compiler_info = cuda_info,
        cuda_dep_tracking_mode = DepTrackingMode(ctx.attrs.cuda_dep_tracking_mode),
        cvtres_compiler_info = cvtres_info,
        cxx_compiler_info = cxx_info,
        dumpbin_toolchain_path = ctx.attrs._dumpbin_toolchain_path[DefaultInfo].default_outputs[0] if ctx.attrs._dumpbin_toolchain_path else None,
        gcno_files = value_or(ctx.attrs.gcno_files, False),
        header_mode = _get_header_mode(ctx),
        headers_as_raw_headers_mode = HeadersAsRawHeadersMode(ctx.attrs.headers_as_raw_headers_mode) if ctx.attrs.headers_as_raw_headers_mode != None else None,
        hip_compiler_info = hip_info,
        internal_tools = ctx.attrs.internal_tools[CxxInternalTools],
        linker_info = linker_info,
        lipo = ctx.attrs.lipo[RunInfo] if ctx.attrs.lipo else None,
        llvm_cgdata = ctx.attrs.llvm_cgdata[RunInfo] if ctx.attrs.llvm_cgdata else None,
        llvm_link = ctx.attrs.llvm_link[RunInfo] if ctx.attrs.llvm_link else None,
        objc_compiler_info = objc_info,
        objcxx_compiler_info = objcxx_info,
        object_format = CxxObjectFormat(object_format),
        optimization_compiler_flags_EXPERIMENTAL = ctx.attrs.optimization_compiler_flags_EXPERIMENTAL,
        pic_behavior = PicBehavior(ctx.attrs.pic_behavior),
        platform_deps_aliases = ctx.attrs.platform_deps_aliases,
        platform_name = platform_name,
        raw_headers_as_headers_mode = RawHeadersAsHeadersMode(ctx.attrs.raw_headers_as_headers_mode) if ctx.attrs.raw_headers_as_headers_mode != None else None,
        rc_compiler_info = rc_info,
        remap_cwd = ctx.attrs.remap_cwd,
        split_debug_mode = SplitDebugMode(ctx.attrs.split_debug_mode),
        strip_flags_info = strip_flags_info,
        target_sdk_version = get_toolchain_target_sdk_version(ctx),
        # TODO(T138705365): Turn on dep files by default
        use_dep_files = value_or(ctx.attrs.use_dep_files, _get_default_use_dep_files(platform_name)),
    )

def cxx_toolchain_extra_attributes(is_toolchain_rule):
    dep_type = attrs.exec_dep if is_toolchain_rule else attrs.dep
    return {
        "archive_symbol_table": attrs.bool(default = True),
        "archiver": dep_type(providers = [RunInfo]),
        "archiver_reads_inputs": attrs.bool(default = True),
        "archiver_supports_argfiles": attrs.bool(default = False),
        "asm_compiler": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "asm_preprocessor": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "assembler": dep_type(providers = [RunInfo]),
        "assembler_preprocessor": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "bolt": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "bolt_enabled": attrs.bool(default = False),
        "c_compiler": dep_type(providers = [RunInfo]),
        "clang_remarks": attrs.option(attrs.string(), default = None),
        "clang_trace": attrs.option(attrs.bool(), default = None),
        "cpp_dep_tracking_mode": attrs.enum(DepTrackingMode.values(), default = "makefile"),
        "cuda_compiler": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "cuda_dep_tracking_mode": attrs.enum(DepTrackingMode.values(), default = "makefile"),
        "custom_tools": attrs.dict(key = attrs.string(), value = dep_type(providers = [RunInfo]), default = {}),
        "cvtres_compiler": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "cxx_compiler": dep_type(providers = [RunInfo]),
        "dwp": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "gcno_files": attrs.bool(default = False),
        "generate_linker_maps": attrs.bool(default = False),
        "hip_compiler": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "internal_tools": dep_type(providers = [CxxInternalTools], default = "prelude//cxx/tools:internal_tools"),
        "link_ordering": attrs.enum(LinkOrdering.values(), default = "preorder"),
        "link_weight": attrs.int(default = 1),
        "linker": dep_type(providers = [RunInfo]),
        "lipo": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "llvm_cgdata": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "llvm_link": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "lto_mode": attrs.enum(LtoMode.values(), default = "none"),
        # Darwin only: the minimum deployment target supported
        "min_sdk_version": attrs.option(attrs.string(), default = None),
        "nm": dep_type(providers = [RunInfo]),
        "objcopy_for_shared_library_interface": dep_type(providers = [RunInfo]),
        "objdump": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "object_format": attrs.enum(CxxObjectFormat.values(), default = "native"),
        "optimization_compiler_flags_EXPERIMENTAL": attrs.list(attrs.string(), default = []),
        "pic_behavior": attrs.enum(PicBehavior.values(), default = "supported"),
        # A placeholder tool that can be used to set up toolchain constraints.
        # Useful when fat and thin toolchahins share the same underlying tools via `command_alias()`,
        # which requires setting up separate platform-specific aliases with the correct constraints.
        "placeholder_tool": attrs.option(dep_type(providers = [RunInfo]), default = None),
        # Used for regex matching any 'platform_*' attributes.
        "platform_deps_aliases": attrs.option(attrs.list(attrs.string()), default = None),
        # Used for regex matching any 'platform_*' attributes.
        "platform_name": attrs.option(attrs.string(), default = None),
        "private_headers_symlinks_enabled": attrs.bool(default = True),
        "public_headers_symlinks_enabled": attrs.bool(default = True),
        "ranlib": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "rc_compiler": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "remap_cwd": attrs.bool(default = False),
        "requires_objects": attrs.bool(default = False),
        "sanitizer_runtime_enabled": attrs.bool(default = False),
        "sanitizer_runtime_files": attrs.set(attrs.dep(), sorted = True, default = []),  # Use `attrs.dep()` as it's not a tool, always propagate target platform
        "shared_library_interface_mode": attrs.enum(ShlibInterfacesMode.values(), default = "disabled"),
        "shared_library_interface_producer": attrs.option(dep_type(providers = [RunInfo]), default = None),
        "split_debug_mode": attrs.enum(SplitDebugMode.values(), default = "none"),
        "strip": dep_type(providers = [RunInfo]),
        "supports_distributed_thinlto": attrs.bool(default = False),
        "supports_two_phase_compilation": attrs.bool(default = False),
        # Darwin only: the deployment target to use for this build
        "target_sdk_version": attrs.option(attrs.string(), default = None),
        "thin_lto_double_codegen_enabled": attrs.bool(default = False),
        "thin_lto_premerger_enabled": attrs.bool(default = False),
        "use_archiver_flags": attrs.bool(default = True),
        "use_dep_files": attrs.option(attrs.bool(), default = None),
        # TODO(scottcao): Figure out a slightly better way to integrate this. In theory, this is only needed for clang toolchain.
        # If we were using msvc, we should be able to use dumpbin directly.
        "_dumpbin_toolchain_path": attrs.default_only(attrs.option(dep_type(providers = [DefaultInfo]), default = select({
            "DEFAULT": None,
            "ovr_config//os:windows": select({
                # Unfortunately, it seems like an unresolved select when resolve exec platforms causes the whole resolution
                # to fail, so I need a DEFAULT here when some target without cpu constraint tries to configure against the
                # windows exec platform.
                "DEFAULT": None,
                # FIXME: prelude// should be standalone (not refer to fbsource//)
                "ovr_config//cpu:x86_32": "fbsource//third-party/toolchains/visual_studio:cl_x86_and_tools",
                "ovr_config//cpu:x86_64": "fbsource//third-party/toolchains/visual_studio:cl_x64_and_tools",
            }),
        }) if is_full_meta_repo() else None)),
        "_msvc_hermetic_exec": attrs.default_only(dep_type(providers = [RunInfo], default = "prelude//windows/tools:msvc_hermetic_exec")),
    } | cxx_toolchain_allow_cache_upload_args()

def _cxx_toolchain_inheriting_target_platform_attrs():
    attrs = dict(cxx_rules.cxx_toolchain.attrs)
    attrs.update(cxx_toolchain_extra_attributes(is_toolchain_rule = True))
    return attrs

cxx_toolchain_inheriting_target_platform = rule(
    impl = cxx_toolchain_impl,
    attrs = _cxx_toolchain_inheriting_target_platform_attrs(),
    is_toolchain_rule = True,
)

_APPLE_PLATFORM_NAME_PREFIXES = [
    "iphonesimulator",
    "iphoneos",
    "maccatalyst",
    "macosx",
    "watchos",
    "watchsimulator",
    "appletvos",
    "appletvsimulator",
]

def _get_default_use_dep_files(platform_name: str) -> bool:
    # All Apple platforms use Clang which supports the standard dep files format
    for apple_platform_name_prefix in _APPLE_PLATFORM_NAME_PREFIXES:
        if apple_platform_name_prefix in platform_name:
            return True
    return False

def _get_header_mode(ctx: AnalysisContext) -> HeaderMode:
    if ctx.attrs.use_header_map:
        if ctx.attrs.private_headers_symlinks_enabled or ctx.attrs.public_headers_symlinks_enabled:
            return HeaderMode("symlink_tree_with_header_map")
        else:
            return HeaderMode("header_map_only")
    else:
        return HeaderMode("symlink_tree_only")

def _get_shared_library_name_default_prefix(ctx: AnalysisContext) -> str:
    extension = ctx.attrs.shared_library_extension
    return "" if extension == "dll" else "lib"

def _get_shared_library_name_format(ctx: AnalysisContext) -> str:
    linker_type = LinkerType(ctx.attrs.linker_type)
    extension = ctx.attrs.shared_library_extension
    if extension == "":
        extension = LINKERS[linker_type].default_shared_library_extension
    return "{}." + extension

def _get_shared_library_versioned_name_format(ctx: AnalysisContext) -> str:
    linker_type = LinkerType(ctx.attrs.linker_type)
    extension_format = ctx.attrs.shared_library_versioned_extension_format.replace("%s", "{}")
    if extension_format == "":
        extension_format = LINKERS[linker_type].default_shared_library_versioned_extension_format
    return "{}." + extension_format

def _get_maybe_wrapped_msvc(compiler: RunInfo, compiler_type: str, msvc_hermetic_exec: RunInfo) -> RunInfo:
    if compiler_type == "windows":
        return RunInfo(args = cmd_args(msvc_hermetic_exec, compiler))
    return compiler
