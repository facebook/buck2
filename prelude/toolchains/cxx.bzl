# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//cxx:cxx_toolchain_types.bzl",
    "BinaryUtilitiesInfo",
    "CCompilerInfo",
    "CxxCompilerInfo",
    "CxxPlatformInfo",
    "CxxToolchainInfo",
    "LinkerInfo",
)
load(
    "@prelude//cxx:headers.bzl",
    "HeaderMode",
)
load(
    "@prelude//cxx:linker.bzl",
    "is_pdb_generated",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
)

def _system_cxx_toolchain_impl(ctx):
    """
    A very simple toolchain that is hardcoded to the current environment.
    """
    archiver_type = "gnu"
    linker_type = "gnu"
    if host_info().os.is_macos:
        linker_type = "darwin"
    elif host_info().os.is_windows:
        linker_type = "windows"
        archiver_type = "windows"
    return [
        DefaultInfo(),
        CxxToolchainInfo(
            mk_comp_db = ctx.attrs.make_comp_db,
            linker_info = LinkerInfo(
                linker = RunInfo(args = [ctx.attrs.linker]),
                linker_flags = ["-fuse-ld=lld"] + ctx.attrs.link_flags,
                archiver = RunInfo(args = ["ar", "rcs"]),
                archiver_type = archiver_type,
                generate_linker_maps = False,
                type = linker_type,
                link_binaries_locally = True,
                archive_objects_locally = True,
                use_archiver_flags = False,
                static_dep_runtime_ld_flags = [],
                static_pic_dep_runtime_ld_flags = [],
                shared_dep_runtime_ld_flags = [],
                independent_shlib_interface_linker_flags = [],
                shlib_interfaces = "disabled",
                link_style = LinkStyle(ctx.attrs.link_style),
                link_weight = 1,
                binary_extension = "",
                object_file_extension = "o",
                shared_library_name_format = "lib{}.so",
                shared_library_versioned_name_format = "lib{}.so.{}",
                static_library_extension = "a",
                force_full_hybrid_if_capable = False,
                is_pdb_generated = is_pdb_generated(linker_type, ctx.attrs.link_flags),
            ),
            bolt_enabled = False,
            binary_utilities_info = BinaryUtilitiesInfo(
                nm = RunInfo(args = ["nm"]),
                # objcopy = ctx.attrs.objcopy_for_shared_library_interface[RunInfo],
                # ranlib = ctx.attrs.ranlib[RunInfo],
                ranlib = RunInfo(args = ["ranlib"]),
                strip = RunInfo(args = ["strip"]),
                dwp = None,
                bolt_msdk = None,
            ),
            cxx_compiler_info = CxxCompilerInfo(
                compiler = RunInfo(args = [ctx.attrs.cxx_compiler]),
                preprocessor_flags = [],
                compiler_flags = ctx.attrs.cxx_flags,
                compiler_type = ctx.attrs.compiler_type,
            ),
            c_compiler_info = CCompilerInfo(
                compiler = RunInfo(args = [ctx.attrs.compiler]),
                preprocessor_flags = [],
                compiler_flags = ctx.attrs.c_flags,
                compiler_type = ctx.attrs.compiler_type,
            ),
            as_compiler_info = CCompilerInfo(
                compiler = RunInfo(args = [ctx.attrs.compiler]),
                compiler_type = ctx.attrs.compiler_type,
            ),
            asm_compiler_info = CCompilerInfo(
                compiler = RunInfo(args = [ctx.attrs.compiler]),
                compiler_type = ctx.attrs.compiler_type,
            ),
            header_mode = HeaderMode("symlink_tree_only"),
        ),
        CxxPlatformInfo(name = "x86_64"),
    ]

# Use clang, since thats available everywhere and what we have tested with.
system_cxx_toolchain = rule(
    impl = _system_cxx_toolchain_impl,
    attrs = {
        "c_flags": attrs.list(attrs.string(), default = []),
        "compiler": attrs.string(default = "clang"),
        "compiler_type": attrs.string(default = "clang"),  # one of CxxToolProviderType
        "cxx_compiler": attrs.string(default = "clang++"),
        "cxx_flags": attrs.list(attrs.string(), default = []),
        "link_flags": attrs.list(attrs.string(), default = []),
        "link_style": attrs.string(default = "shared"),
        "linker": attrs.string(default = "clang++"),
        "make_comp_db": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//cxx/tools:make_comp_db")),
    },
    is_toolchain_rule = True,
)
