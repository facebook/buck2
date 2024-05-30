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
    "CvtresCompilerInfo",
    "CxxCompilerInfo",
    "CxxPlatformInfo",
    "CxxToolchainInfo",
    "LinkerInfo",
    "PicBehavior",
    "RcCompilerInfo",
    "ShlibInterfacesMode",
)
load("@prelude//cxx:headers.bzl", "HeaderMode")
load("@prelude//cxx:linker.bzl", "is_pdb_generated")
load("@prelude//linking:link_info.bzl", "LinkOrdering", "LinkStyle")
load("@prelude//linking:lto.bzl", "LtoMode")

SystemCxxToolchainInfo = provider(
    fields = {
        "compiler": provider_field(typing.Any, default = None),
        "compiler_type": provider_field(typing.Any, default = None),
        "cxx_compiler": provider_field(typing.Any, default = None),
        "asm_compiler": provider_field(typing.Any, default = None),
        "asm_compiler_type": provider_field(typing.Any, default = None),
        "rc_compiler": provider_field(typing.Any, default = None),
        "cvtres_compiler": provider_field(typing.Any, default = None),
        "archiver": provider_field(typing.Any, default = None),
        "archiver_type": provider_field(typing.Any, default = None),
        "linker": provider_field(typing.Any, default = None),
        "linker_type": provider_field(typing.Any, default = None),
        "os": provider_field(typing.Any, default = None),
    },
)

def _system_cxx_toolchain_impl(ctx: AnalysisContext):
    """
    A very simple toolchain that is hardcoded to the current environment.
    """

    compiler = ctx.attrs.compiler[SystemCxxToolchainInfo]

    archiver_supports_argfiles = compiler.os != "macos"
    additional_linker_flags = ["-fuse-ld=lld"] if compiler.os == "linux" and compiler.linker != "g++" and compiler.cxx_compiler != "g++" else []

    if compiler.os == "windows":
        linker_type = "windows"
        binary_extension = "exe"
        object_file_extension = "obj"
        static_library_extension = "lib"
        shared_library_name_default_prefix = ""
        shared_library_name_format = "{}.dll"
        shared_library_versioned_name_format = "{}.dll"
        pic_behavior = PicBehavior("not_supported")
    else:
        binary_extension = ""
        object_file_extension = "o"
        static_library_extension = "a"
        shared_library_name_default_prefix = "lib"
        shared_library_name_format = "{}.so"
        shared_library_versioned_name_format = "{}.so.{}"

        if compiler.os == "macos":
            linker_type = "darwin"
            pic_behavior = PicBehavior("always_enabled")
        else:
            linker_type = "gnu"
            pic_behavior = PicBehavior("supported")

    if compiler.compiler_type == "clang":
        llvm_link = RunInfo(args = ["llvm-link"])
    else:
        llvm_link = None

    return [
        DefaultInfo(),
        CxxToolchainInfo(
            mk_comp_db = ctx.attrs.make_comp_db,
            linker_info = LinkerInfo(
                linker = _run_info(compiler.linker),
                linker_flags = additional_linker_flags + ctx.attrs.link_flags,
                post_linker_flags = ctx.attrs.post_link_flags,
                archiver = _run_info(compiler.archiver),
                archiver_type = compiler.archiver_type,
                archiver_supports_argfiles = archiver_supports_argfiles,
                generate_linker_maps = False,
                lto_mode = LtoMode("none"),
                type = linker_type,
                link_binaries_locally = True,
                archive_objects_locally = True,
                use_archiver_flags = True,
                static_dep_runtime_ld_flags = [],
                static_pic_dep_runtime_ld_flags = [],
                shared_dep_runtime_ld_flags = [],
                independent_shlib_interface_linker_flags = [],
                shlib_interfaces = ShlibInterfacesMode("disabled"),
                link_style = LinkStyle(ctx.attrs.link_style),
                link_weight = 1,
                binary_extension = binary_extension,
                object_file_extension = object_file_extension,
                shared_library_name_default_prefix = shared_library_name_default_prefix,
                shared_library_name_format = shared_library_name_format,
                shared_library_versioned_name_format = shared_library_versioned_name_format,
                static_library_extension = static_library_extension,
                force_full_hybrid_if_capable = False,
                is_pdb_generated = is_pdb_generated(linker_type, ctx.attrs.link_flags),
                link_ordering = ctx.attrs.link_ordering,
            ),
            bolt_enabled = False,
            binary_utilities_info = BinaryUtilitiesInfo(
                nm = RunInfo(args = ["nm"]),
                objcopy = RunInfo(args = ["objcopy"]),
                objdump = RunInfo(args = ["objdump"]),
                ranlib = RunInfo(args = ["ranlib"]),
                strip = RunInfo(args = ["strip"]),
                dwp = None,
                bolt_msdk = None,
            ),
            cxx_compiler_info = CxxCompilerInfo(
                compiler = _run_info(compiler.cxx_compiler),
                preprocessor_flags = [],
                compiler_flags = ctx.attrs.cxx_flags,
                compiler_type = compiler.compiler_type,
            ),
            c_compiler_info = CCompilerInfo(
                compiler = _run_info(compiler.compiler),
                preprocessor_flags = [],
                compiler_flags = ctx.attrs.c_flags,
                compiler_type = compiler.compiler_type,
            ),
            as_compiler_info = CCompilerInfo(
                compiler = _run_info(compiler.compiler),
                compiler_type = compiler.compiler_type,
            ),
            asm_compiler_info = CCompilerInfo(
                compiler = _run_info(compiler.asm_compiler),
                compiler_type = compiler.asm_compiler_type,
            ),
            cvtres_compiler_info = CvtresCompilerInfo(
                compiler = _run_info(compiler.cvtres_compiler),
                preprocessor_flags = [],
                compiler_flags = ctx.attrs.cvtres_flags,
                compiler_type = compiler.compiler_type,
            ),
            rc_compiler_info = RcCompilerInfo(
                compiler = _run_info(compiler.rc_compiler),
                preprocessor_flags = [],
                compiler_flags = ctx.attrs.rc_flags,
                compiler_type = compiler.compiler_type,
            ),
            header_mode = HeaderMode("symlink_tree_only"),
            cpp_dep_tracking_mode = ctx.attrs.cpp_dep_tracking_mode,
            pic_behavior = pic_behavior,
            llvm_link = llvm_link,
        ),
        CxxPlatformInfo(name = "x86_64"),
    ]

def _run_info(args):
    return None if args == None else RunInfo(args = [args])

system_cxx_toolchain = rule(
    impl = _system_cxx_toolchain_impl,
    attrs = {
        "compiler": attrs.exec_dep(providers = [SystemCxxToolchainInfo], default = select({
            "DEFAULT": "prelude//toolchains/cxx/clang:path_clang_tools",
            "config//os:windows": "prelude//toolchains/msvc:msvc_tools",
        })),
        "c_flags": attrs.list(attrs.string(), default = []),
        "cpp_dep_tracking_mode": attrs.string(default = "makefile"),
        "cvtres_flags": attrs.list(attrs.string(), default = []),
        "cxx_flags": attrs.list(attrs.string(), default = []),
        "link_flags": attrs.list(attrs.string(), default = []),
        "link_ordering": attrs.option(attrs.enum(LinkOrdering.values()), default = None),
        "link_style": attrs.string(default = "shared"),
        "make_comp_db": attrs.default_only(attrs.exec_dep(providers = [RunInfo], default = "prelude//cxx/tools:make_comp_db")),
        "post_link_flags": attrs.list(attrs.string(), default = []),
        "rc_flags": attrs.list(attrs.string(), default = []),
    },
    is_toolchain_rule = True,
)
