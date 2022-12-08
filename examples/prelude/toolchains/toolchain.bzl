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
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
)
load(
    "@prelude//python:toolchain.bzl",
    "PythonPlatformInfo",
    "PythonToolchainInfo",
)
load(
    "@prelude//ocaml/providers.bzl",
    "OCamlPlatformInfo",
    "OCamlToolchainInfo",
)

DEFAULT_MAKE_COMP_DB = "@prelude//cxx/tools:make_comp_db"
DEFAULT_MAKE_PEX_INPLACE = "@prelude//python/tools:make_pex_inplace"
DEFAULT_MAKE_PEX_MODULES = "@prelude//python/tools:make_pex_modules"

def _cxx_toolchain(ctx):
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
                linker = RunInfo(args = ["g++"]),
                linker_flags = [
                    "-lstdc++",
                ],
                archiver = RunInfo(args = ["ar", "rcs"]),
                archiver_type = archiver_type,
                type = linker_type,
                link_binaries_locally = True,
                archive_objects_locally = True,
                use_archiver_flags = False,
                static_dep_runtime_ld_flags = [],
                static_pic_dep_runtime_ld_flags = [],
                shared_dep_runtime_ld_flags = [],
                independent_shlib_interface_linker_flags = [],
                mk_shlib_intf = ctx.attrs.make_shlib_intf,
                link_style = LinkStyle(ctx.attrs.link_style),
                link_weight = 1,
                binary_extension = "",
                object_file_extension = "o",
                shared_library_name_format = "lib{}.so",
                shared_library_versioned_name_format = "lib{}.so.{}",
                static_library_extension = "a",
                force_full_hybrid_if_capable = False,
            ),
            bolt_enabled = False,
            binary_utilities_info = BinaryUtilitiesInfo(
                nm = RunInfo(args = ["nm"]),
                # objcopy = ctx.attrs.objcopy_for_shared_library_interface[RunInfo],
                # ranlib = ctx.attrs.ranlib[RunInfo],
                ranlib = RunInfo(args = ["raninfo"]),
                strip = RunInfo(args = ["strip"]),
                dwp = None,
                bolt_msdk = None,
            ),
            cxx_compiler_info = CxxCompilerInfo(
                compiler = RunInfo(args = ["clang++"]),
                preprocessor_flags = [],
                compiler_flags = [],
                compiler_type = "clang",  # one of CxxToolProviderType
            ),
            c_compiler_info = CCompilerInfo(
                compiler = RunInfo(args = ["clang"]),
                preprocessor_flags = [],
                compiler_flags = [],
                compiler_type = "clang",  # one of CxxToolProviderType
            ),
            header_mode = HeaderMode("symlink_tree_only"),
        ),
        CxxPlatformInfo(name = "x86_64"),
    ]

cxx_toolchain = rule(
    impl = _cxx_toolchain,
    attrs = {
        "link_style": attrs.string(default = "shared"),
        "make_comp_db": attrs.dep(providers = [RunInfo], default = DEFAULT_MAKE_COMP_DB),
        "make_shlib_intf": attrs.dep(providers = [RunInfo], default = DEFAULT_MAKE_COMP_DB),
    },
    is_toolchain_rule = True,
)

def _python_toolchain(ctx):
    """
    A very simple toolchain that is hardcoded to the current environment.
    """

    return [
        DefaultInfo(),
        PythonToolchainInfo(
            make_source_db = ctx.attrs.make_source_db[RunInfo],
            host_interpreter = RunInfo(args = ["python3"]),
            interpreter = RunInfo(args = ["python3"]),
            make_pex_modules = ctx.attrs.make_pex_modules[RunInfo],
            make_pex_inplace = ctx.attrs.make_pex_inplace[RunInfo],
            compile = RunInfo(args = ["echo", "COMPILEINFO"]),
            package_style = "inplace",
            native_link_strategy = "merged",
        ),
        PythonPlatformInfo(name = "x86_64"),
    ]

python_toolchain = rule(
    impl = _python_toolchain,
    attrs = {
        "make_pex_inplace": attrs.dep(providers = [RunInfo], default = DEFAULT_MAKE_PEX_INPLACE),
        "make_pex_modules": attrs.dep(providers = [RunInfo], default = DEFAULT_MAKE_PEX_MODULES),
        "make_source_db": attrs.dep(providers = [RunInfo], default = DEFAULT_MAKE_COMP_DB),
    },
    is_toolchain_rule = True,
)

def _ocaml_toolchain(_ctx):
    """
    A very simple toolchain that is hardcoded to the current environment.
    """

    return [
        DefaultInfo(
        ),
        OCamlToolchainInfo(
            ocaml_compiler = RunInfo(args = ["ocamlopt.opt"]),

            # "Partial linking" (via `ocamlopt.opt -output-obj`) emits calls to
            # `ld -r -o`. If not `None`, this is the `ld` that will be invoked;
            # the default is to use whatever `ld` is in the environment. See
            # [Note: What is `binutils_ld`?] in `providers.bzl`.
            binutils_ld = None,

            # This one was introduced in D37700753. The diff talks about
            # cross-compilation IIUC.
            binutils_as = None,
            dep_tool = RunInfo(args = ["ocamldep.opt"]),
            yacc_compiler = RunInfo(args = ["ocamlyacc"]),
            menhir_compiler = RunInfo(args = ["menir"]),
            lex_compiler = RunInfo(args = ["lex.compiler"]),

            # These are choices for CircleCI ubuntu. The right values vary from
            # platform to platform.
            interop_includes = "/usr/lib/ocaml",
            libasmrun = "/usr/lib/ocaml/libasmrun.a",
            ocaml_bytecode_compiler = RunInfo(args = ["ocamlc.opt"]),
            debug = RunInfo(args = ["ocamldebug"]),
            warnings_flags = "-4-29-35-41-42-44-45-48-50-58-70",
            ocaml_compiler_flags = [],  # e.g. "-opaque"
        ),
        OCamlPlatformInfo(name = "x86_64"),
    ]

ocaml_toolchain = rule(
    impl = _ocaml_toolchain,
    attrs = {
    },
    is_toolchain_rule = True,
)
