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
    "@prelude//python_bootstrap:python_bootstrap.bzl",
    "PythonBootstrapToolchainInfo",
)
load("@prelude//rust:rust_toolchain.bzl", "RustPlatformInfo", "RustToolchainInfo")

DEFAULT_MAKE_COMP_DB = "@prelude//cxx/tools:make_comp_db"
DEFAULT_MAKE_PEX_INPLACE = "@prelude//python/tools:make_pex_inplace"
DEFAULT_MAKE_PEX_MODULES = "@prelude//python/tools:make_pex_modules"

def _cxx_toolchain(ctx):
    """
    A very simple toolchain that is hardcoded to the current environment.
    """

    return [
        DefaultInfo(),
        CxxToolchainInfo(
            mk_comp_db = ctx.attrs.make_comp_db,
            linker_info = LinkerInfo(
                linker = RunInfo(args = ["gcc"]),
                linker_flags = [],
                archiver = RunInfo(args = ["ar", "rcs"]),
                type = "gnu",
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
            as_compiler_info = CCompilerInfo(
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

# Seems Windows disagrees with what to call python3
python = "python" if host_info().os.is_windows else "python3"

def _python_toolchain(ctx):
    """
    A very simple toolchain that is hardcoded to the current environment.
    """

    return [
        DefaultInfo(),
        PythonToolchainInfo(
            make_source_db = ctx.attrs.make_source_db,
            host_interpreter = RunInfo(args = [python]),
            interpreter = RunInfo(args = [python]),
            make_pex_modules = ctx.attrs.make_pex_modules,
            make_pex_inplace = ctx.attrs.make_pex_inplace,
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

def _python_bootstrap_toolchain(_ctx):
    return [
        DefaultInfo(),
        PythonBootstrapToolchainInfo(interpreter = python),
    ]

python_bootstrap_toolchain = rule(
    impl = _python_bootstrap_toolchain,
    attrs = {},
    is_toolchain_rule = True,
)

def _rust_toolchain(ctx):
    return [
        DefaultInfo(),
        RustToolchainInfo(
            allow_lints = [],
            clippy_driver = "clippy-driver",
            compiler = "rustc",
            deny_lints = [],
            extern_html_root_url_prefix = "",
            failure_filter = False,
            failure_filter_action = None,
            pipelined = False,
            report_unused_deps = False,
            rustc_action = ctx.attrs.rustc_action[RunInfo],
            rustc_binary_flags = [],
            rustc_check_flags = [],
            rustc_flags = [],
            default_edition = "2021",
            rustc_target_triple = "x86_64-pc-windows-msvc" if host_info().os.is_windows else "x86_64-unknown-linux-gnu",
            rustc_test_flags = "",
            rustdoc = "rustdoc",
            rustdoc_flags = [],
            warn_lints = [],
        ),
        RustPlatformInfo(
            name = "x86_64",
        ),
    ]

rust_toolchain = rule(
    impl = _rust_toolchain,
    attrs = {
        "rustc_action": attrs.dep(providers = [RunInfo], default = "prelude//rust/tools:rustc_action"),
    },
    is_toolchain_rule = True,
)
