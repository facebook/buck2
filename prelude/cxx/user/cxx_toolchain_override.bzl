# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//cxx:cxx_toolchain_types.bzl",
    "AsCompilerInfo",
    "AsmCompilerInfo",
    "BinaryUtilitiesInfo",
    "CCompilerInfo",
    "CxxCompilerInfo",
    "CxxInternalTools",
    "CxxObjectFormat",
    "CxxPlatformInfo",
    "CxxToolchainInfo",
    "LinkerInfo",
    "LinkerType",
    "ObjcCompilerInfo",
    "ObjcxxCompilerInfo",
    "PicBehavior",
    "ShlibInterfacesMode",
    "StripFlagsInfo",
    "cxx_toolchain_infos",
)
load("@prelude//cxx:cxx_utility.bzl", "cxx_toolchain_allow_cache_upload_args")
load("@prelude//cxx:debug.bzl", "SplitDebugMode")
load("@prelude//cxx:headers.bzl", "HeaderMode")
load("@prelude//cxx:linker.bzl", "is_pdb_generated")
load("@prelude//cxx:target_sdk_version.bzl", "get_toolchain_target_sdk_version")
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
)
load("@prelude//linking:lto.bzl", "LtoMode")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")
load("@prelude//utils:pick.bzl", _pick = "pick", _pick_and_add = "pick_and_add", _pick_bin = "pick_bin", _pick_dep = "pick_dep", _pick_raw = "pick_raw")
load("@prelude//utils:utils.bzl", "flatten", "map_val", "value_or")

def _cxx_toolchain_override(ctx):
    base_toolchain = ctx.attrs.base[CxxToolchainInfo]
    base_as_info = base_toolchain.as_compiler_info
    as_info = None
    if base_as_info != None:
        as_info = AsCompilerInfo(
            compiler = _pick_bin(ctx.attrs.as_compiler, base_as_info.compiler),
            compiler_type = base_as_info.compiler_type,
            compiler_flags = _pick(ctx.attrs.as_compiler_flags, base_as_info.compiler_flags),
            preprocessor = _pick_bin(ctx.attrs.as_compiler, base_as_info.preprocessor),
            preprocessor_type = base_as_info.preprocessor_type,
            preprocessor_flags = _pick(ctx.attrs.as_preprocessor_flags, base_as_info.preprocessor_flags),
        )
    asm_info = base_toolchain.asm_compiler_info
    if asm_info != None:
        asm_info = AsmCompilerInfo(
            compiler = _pick_bin(ctx.attrs.asm_compiler, asm_info.compiler),
            compiler_type = _pick_raw(ctx.attrs.asm_compiler_type, asm_info.compiler_type),
            compiler_flags = _pick(ctx.attrs.asm_compiler_flags, asm_info.compiler_flags),
            preprocessor = _pick_bin(ctx.attrs.asm_compiler, asm_info.preprocessor),
            preprocessor_type = asm_info.preprocessor_type,
            preprocessor_flags = _pick(ctx.attrs.asm_preprocessor_flags, asm_info.preprocessor_flags),
        )
    base_c_info = base_toolchain.c_compiler_info
    c_info = CCompilerInfo(
        compiler = _pick_bin(ctx.attrs.c_compiler, base_c_info.compiler),
        compiler_type = base_c_info.compiler_type,
        compiler_flags = _pick_and_add(ctx.attrs.c_compiler_flags, ctx.attrs.additional_c_compiler_flags, base_c_info.compiler_flags),
        preprocessor = _pick_bin(ctx.attrs.c_compiler, base_c_info.preprocessor),
        preprocessor_type = base_c_info.preprocessor_type,
        preprocessor_flags = _pick(ctx.attrs.c_preprocessor_flags, base_c_info.preprocessor_flags),
        allow_cache_upload = _pick_raw(ctx.attrs.c_compiler_allow_cache_upload, base_c_info.allow_cache_upload),
    )
    base_objc_info = base_toolchain.objc_compiler_info
    objc_info = ObjcCompilerInfo(
        compiler = _pick_bin(ctx.attrs.c_compiler, base_c_info.compiler),
        compiler_type = base_c_info.compiler_type,
        compiler_flags = _pick_and_add(ctx.attrs.c_compiler_flags, ctx.attrs.additional_c_compiler_flags, base_objc_info.compiler_flags),
        preprocessor = _pick_bin(ctx.attrs.c_compiler, base_c_info.preprocessor),
        preprocessor_type = base_c_info.preprocessor_type,
        preprocessor_flags = _pick(ctx.attrs.c_preprocessor_flags, base_c_info.preprocessor_flags),
        allow_cache_upload = _pick_raw(ctx.attrs.c_compiler_allow_cache_upload, base_c_info.allow_cache_upload),
    )
    base_cxx_info = base_toolchain.cxx_compiler_info
    cxx_info = CxxCompilerInfo(
        compiler = _pick_bin(ctx.attrs.cxx_compiler, base_cxx_info.compiler),
        compiler_type = base_cxx_info.compiler_type,
        compiler_flags = _pick_and_add(ctx.attrs.cxx_compiler_flags, ctx.attrs.additional_cxx_compiler_flags, base_cxx_info.compiler_flags),
        preprocessor = _pick_bin(ctx.attrs.cxx_compiler, base_cxx_info.preprocessor),
        preprocessor_type = base_cxx_info.preprocessor_type,
        preprocessor_flags = _pick(ctx.attrs.cxx_preprocessor_flags, base_cxx_info.preprocessor_flags),
        allow_cache_upload = _pick_raw(ctx.attrs.cxx_compiler_allow_cache_upload, base_cxx_info.allow_cache_upload),
    )
    base_objcxx_info = base_toolchain.objcxx_compiler_info
    objcxx_info = ObjcxxCompilerInfo(
        compiler = _pick_bin(ctx.attrs.cxx_compiler, base_cxx_info.compiler),
        compiler_type = base_cxx_info.compiler_type,
        compiler_flags = _pick_and_add(ctx.attrs.cxx_compiler_flags, ctx.attrs.additional_cxx_compiler_flags, base_objcxx_info.compiler_flags),
        preprocessor = _pick_bin(ctx.attrs.cxx_compiler, base_cxx_info.preprocessor),
        preprocessor_type = base_cxx_info.preprocessor_type,
        preprocessor_flags = _pick(ctx.attrs.cxx_preprocessor_flags, base_cxx_info.preprocessor_flags),
        allow_cache_upload = _pick_raw(ctx.attrs.cxx_compiler_allow_cache_upload, base_cxx_info.allow_cache_upload),
    )
    base_linker_info = base_toolchain.linker_info
    linker_type = LinkerType(ctx.attrs.linker_type) if ctx.attrs.linker_type != None else base_linker_info.type
    pdb_expected = is_pdb_generated(linker_type, ctx.attrs.linker_flags) if ctx.attrs.linker_flags != None else base_linker_info.is_pdb_generated

    # This handles case when linker type is overridden to non-windows from
    # windows but linker flags are inherited.
    # When it's changed from non-windows to windows but flags are not changed,
    # we can't inspect base linker flags and disable PDB subtargets.
    # This shouldn't be a problem because to use windows linker after non-windows
    # linker flags should be changed as well.
    pdb_expected = linker_type == LinkerType("windows") and pdb_expected
    shlib_interfaces = ShlibInterfacesMode(ctx.attrs.shared_library_interface_mode) if ctx.attrs.shared_library_interface_mode else None
    sanitizer_runtime_files = flatten([runtime_file[DefaultInfo].default_outputs for runtime_file in ctx.attrs.sanitizer_runtime_files]) if ctx.attrs.sanitizer_runtime_files != None else None
    linker_info = LinkerInfo(
        archiver = _pick_bin(ctx.attrs.archiver, base_linker_info.archiver),
        archiver_flags = value_or(ctx.attrs.archiver_flags, base_linker_info.archiver_flags),
        archiver_type = value_or(ctx.attrs.archiver_type, base_linker_info.archiver_type),
        archiver_supports_argfiles = value_or(ctx.attrs.archiver_supports_argfiles, base_linker_info.archiver_supports_argfiles),
        archive_contents = value_or(ctx.attrs.archive_contents, base_linker_info.archive_contents),
        archive_objects_locally = value_or(ctx.attrs.archive_objects_locally, base_linker_info.archive_objects_locally),
        binary_extension = base_linker_info.binary_extension,
        generate_linker_maps = value_or(ctx.attrs.generate_linker_maps, base_linker_info.generate_linker_maps),
        link_binaries_locally = value_or(ctx.attrs.link_binaries_locally, base_linker_info.link_binaries_locally),
        link_libraries_locally = value_or(ctx.attrs.link_libraries_locally, base_linker_info.link_libraries_locally),
        link_style = LinkStyle(ctx.attrs.link_style) if ctx.attrs.link_style != None else base_linker_info.link_style,
        link_weight = value_or(ctx.attrs.link_weight, base_linker_info.link_weight),
        link_ordering = base_linker_info.link_ordering,
        linker = _pick_bin(ctx.attrs.linker, base_linker_info.linker),
        linker_flags = _pick(ctx.attrs.linker_flags, base_linker_info.linker_flags),
        post_linker_flags = _pick(ctx.attrs.post_linker_flags, base_linker_info.post_linker_flags),
        link_metadata_flag = _pick(ctx.attrs.link_metadata_flag, base_linker_info.link_metadata_flag),
        lto_mode = value_or(map_val(LtoMode, ctx.attrs.lto_mode), base_linker_info.lto_mode),
        object_file_extension = base_linker_info.object_file_extension,
        shlib_interfaces = value_or(shlib_interfaces, base_linker_info.shlib_interfaces),
        mk_shlib_intf = _pick_dep(ctx.attrs.mk_shlib_intf, base_linker_info.mk_shlib_intf),
        requires_archives = base_linker_info.requires_archives,
        requires_objects = base_linker_info.requires_objects,
        supports_distributed_thinlto = base_linker_info.supports_distributed_thinlto,
        independent_shlib_interface_linker_flags = base_linker_info.independent_shlib_interface_linker_flags,
        sanitizer_runtime_enabled = value_or(ctx.attrs.sanitizer_runtime_enabled, base_linker_info.sanitizer_runtime_enabled),
        sanitizer_runtime_files = value_or(sanitizer_runtime_files, base_linker_info.sanitizer_runtime_files),
        shared_dep_runtime_ld_flags = [],
        shared_library_name_default_prefix = ctx.attrs.shared_library_name_default_prefix if ctx.attrs.shared_library_name_default_prefix != None else base_linker_info.shared_library_name_default_prefix,
        shared_library_name_format = ctx.attrs.shared_library_name_format if ctx.attrs.shared_library_name_format != None else base_linker_info.shared_library_name_format,
        shared_library_versioned_name_format = ctx.attrs.shared_library_versioned_name_format if ctx.attrs.shared_library_versioned_name_format != None else base_linker_info.shared_library_versioned_name_format,
        static_dep_runtime_ld_flags = [],
        static_pic_dep_runtime_ld_flags = [],
        static_library_extension = base_linker_info.static_library_extension,
        type = linker_type,
        use_archiver_flags = value_or(ctx.attrs.use_archiver_flags, base_linker_info.use_archiver_flags),
        force_full_hybrid_if_capable = value_or(ctx.attrs.force_full_hybrid_if_capable, base_linker_info.force_full_hybrid_if_capable),
        is_pdb_generated = pdb_expected,
    )

    base_binary_utilities_info = base_toolchain.binary_utilities_info
    binary_utilities_info = BinaryUtilitiesInfo(
        nm = _pick_bin(ctx.attrs.nm, base_binary_utilities_info.nm),
        objcopy = _pick_bin(ctx.attrs.objcopy, base_binary_utilities_info.objcopy),
        objdump = _pick_bin(ctx.attrs.objdump, base_binary_utilities_info.objdump),
        ranlib = _pick_bin(ctx.attrs.ranlib, base_binary_utilities_info.ranlib),
        strip = _pick_bin(ctx.attrs.strip, base_binary_utilities_info.strip),
        dwp = base_binary_utilities_info.dwp,
        bolt_msdk = base_binary_utilities_info.bolt_msdk,
    )

    base_strip_flags_info = base_toolchain.strip_flags_info
    if base_strip_flags_info:
        strip_flags_info = StripFlagsInfo(
            strip_debug_flags = _pick(ctx.attrs.strip_debug_flags, base_strip_flags_info.strip_debug_flags),
            strip_non_global_flags = _pick(ctx.attrs.strip_non_global_flags, base_strip_flags_info.strip_non_global_flags),
            strip_all_flags = _pick(ctx.attrs.strip_all_flags, base_strip_flags_info.strip_all_flags),
        )
    else:
        strip_flags_info = None

    return [
        DefaultInfo(),
    ] + cxx_toolchain_infos(
        internal_tools = ctx.attrs._internal_tools[CxxInternalTools],
        platform_name = ctx.attrs.platform_name if ctx.attrs.platform_name != None else ctx.attrs.base[CxxPlatformInfo].name,
        platform_deps_aliases = ctx.attrs.platform_deps_aliases if ctx.attrs.platform_deps_aliases != None else [],
        linker_info = linker_info,
        as_compiler_info = as_info,
        asm_compiler_info = asm_info,
        binary_utilities_info = binary_utilities_info,
        bolt_enabled = value_or(ctx.attrs.bolt_enabled, base_toolchain.bolt_enabled),
        c_compiler_info = c_info,
        cxx_compiler_info = cxx_info,
        objc_compiler_info = objc_info,
        objcxx_compiler_info = objcxx_info,
        llvm_link = ctx.attrs.llvm_link if ctx.attrs.llvm_link != None else base_toolchain.llvm_link,
        # the rest are used without overrides
        cuda_compiler_info = base_toolchain.cuda_compiler_info,
        hip_compiler_info = base_toolchain.hip_compiler_info,
        header_mode = HeaderMode(ctx.attrs.header_mode) if ctx.attrs.header_mode != None else base_toolchain.header_mode,
        headers_as_raw_headers_mode = base_toolchain.headers_as_raw_headers_mode,
        use_dep_files = base_toolchain.use_dep_files,
        clang_remarks = base_toolchain.clang_remarks,
        gcno_files = base_toolchain.gcno_files,
        clang_trace = base_toolchain.clang_trace,
        object_format = CxxObjectFormat(ctx.attrs.object_format) if ctx.attrs.object_format != None else base_toolchain.object_format,
        conflicting_header_basename_allowlist = base_toolchain.conflicting_header_basename_allowlist,
        strip_flags_info = strip_flags_info,
        pic_behavior = PicBehavior(ctx.attrs.pic_behavior) if ctx.attrs.pic_behavior != None else base_toolchain.pic_behavior.value,
        split_debug_mode = SplitDebugMode(value_or(ctx.attrs.split_debug_mode, base_toolchain.split_debug_mode.value)),
        target_sdk_version = value_or(get_toolchain_target_sdk_version(ctx), base_toolchain.target_sdk_version),
    )

cxx_toolchain_override_registration_spec = RuleRegistrationSpec(
    name = "cxx_toolchain_override",
    impl = _cxx_toolchain_override,
    attrs = {
        "additional_c_compiler_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "additional_cxx_compiler_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "archive_contents": attrs.option(attrs.string(), default = None),
        "archive_objects_locally": attrs.option(attrs.bool(), default = None),
        "archiver": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "archiver_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "archiver_supports_argfiles": attrs.option(attrs.bool(), default = None),
        "archiver_type": attrs.option(attrs.string(), default = None),
        "as_compiler": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "as_compiler_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "as_preprocessor_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "asm_compiler": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "asm_compiler_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "asm_compiler_type": attrs.option(attrs.string(), default = None),
        "asm_preprocessor_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "base": attrs.toolchain_dep(providers = [CxxToolchainInfo]),
        "bolt_enabled": attrs.option(attrs.bool(), default = None),
        "c_compiler": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "c_compiler_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "c_preprocessor_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "cxx_compiler": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "cxx_compiler_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "cxx_preprocessor_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "force_full_hybrid_if_capable": attrs.option(attrs.bool(), default = None),
        "generate_linker_maps": attrs.option(attrs.bool(), default = None),
        "header_mode": attrs.option(attrs.enum(HeaderMode.values()), default = None),
        "link_binaries_locally": attrs.option(attrs.bool(), default = None),
        "link_libraries_locally": attrs.option(attrs.bool(), default = None),
        "link_metadata_flag": attrs.option(attrs.string(), default = None),
        "link_style": attrs.option(attrs.enum(LinkStyle.values()), default = None),
        "link_weight": attrs.option(attrs.int(), default = None),
        "linker": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "linker_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "linker_type": attrs.option(attrs.enum(LinkerType.values()), default = None),
        "lipo": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "llvm_link": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "lto_mode": attrs.option(attrs.enum(LtoMode.values()), default = None),
        "min_sdk_version": attrs.option(attrs.string(), default = None),
        "mk_shlib_intf": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "nm": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "objcopy": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "objdump": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "object_format": attrs.enum(CxxObjectFormat.values(), default = "native"),
        "pic_behavior": attrs.enum(PicBehavior.values(), default = "supported"),
        "platform_deps_aliases": attrs.option(attrs.list(attrs.string()), default = None),
        "platform_name": attrs.option(attrs.string(), default = None),
        "post_linker_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "ranlib": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "sanitizer_runtime_enabled": attrs.bool(default = False),
        "sanitizer_runtime_files": attrs.option(attrs.set(attrs.dep(), sorted = True, default = []), default = None),  # Use `attrs.dep()` as it's not a tool, always propagate target platform
        "shared_library_interface_mode": attrs.option(attrs.enum(ShlibInterfacesMode.values()), default = None),
        "shared_library_name_default_prefix": attrs.option(attrs.string(), default = None),
        "shared_library_name_format": attrs.option(attrs.string(), default = None),
        "shared_library_versioned_name_format": attrs.option(attrs.string(), default = None),
        "split_debug_mode": attrs.option(attrs.enum(SplitDebugMode.values()), default = None),
        "strip": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "strip_all_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "strip_debug_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "strip_non_global_flags": attrs.option(attrs.list(attrs.arg()), default = None),
        "target_sdk_version": attrs.option(attrs.string(), default = None),
        "use_archiver_flags": attrs.option(attrs.bool(), default = None),
        "_internal_tools": attrs.default_only(attrs.exec_dep(providers = [CxxInternalTools], default = "prelude//cxx/tools:internal_tools")),
    } | cxx_toolchain_allow_cache_upload_args(),
    is_toolchain_rule = True,
)
