prebuilt_cxx_library(
    name = "ocaml-dev",
    visibility = ["PUBLIC"],
    header_only = True,
    header_dirs = ["standard_library"],
) if not host_info().os.is_windows else None

prebuilt_ocaml_library(
    name = "ocaml-compiler-libs.shadow",
    visibility = ["PUBLIC"],
    bytecode_lib = "opam/lib/ocaml-compiler-libs/shadow/ocaml_shadow.cma",
    native_lib = "opam/lib/ocaml-compiler-libs/shadow/ocaml_shadow.cmxa",
    include_dir = "opam/lib/ocaml-compiler-libs/shadow",
) if not host_info().os.is_windows else None

prebuilt_ocaml_library(
    name = "ppx_derivers",
    visibility = ["PUBLIC"],
    bytecode_lib = "opam/lib/ppx_derivers/ppx_derivers.cma",
    native_lib = "opam/lib/ppx_derivers/ppx_derivers.cmxa",
    include_dir = "opam/lib/ppx_derivers",
) if not host_info().os.is_windows else None

prebuilt_ocaml_library(
    name = "ocaml.compiler-libs.common",
    visibility = ["PUBLIC"],
    bytecode_lib = "standard_library/compiler-libs/ocamlcommon.cma",
    native_lib = "standard_library/compiler-libs/ocamlcommon.cmxa",
    include_dir = "standard_library/compiler-libs",
    deps = []
) if not host_info().os.is_windows else None

prebuilt_ocaml_library(
    name = "ocaml-compiler-libs.common",
    visibility = ["PUBLIC"],
    bytecode_lib = "opam/lib/ocaml-compiler-libs/common/ocaml_common.cma",
    native_lib = "opam/lib/ocaml-compiler-libs/common/ocaml_common.cmxa",
    include_dir = "opam/lib/ocaml-compiler-libs/common",
    deps = []
) if not host_info().os.is_windows else None

prebuilt_ocaml_library(
    name = "ppxlib.traverse_builtins",
    visibility = ["PUBLIC"],
    bytecode_lib = "opam/lib/ppxlib/traverse_builtins/ppxlib_traverse_builtins.cma",
    native_lib = "opam/lib/ppxlib/traverse_builtins/ppxlib_traverse_builtins.cmxa",
    include_dir = "opam/lib/ppxlib/traverse_builtins",
) if not host_info().os.is_windows else None

prebuilt_ocaml_library(
    name = "ppxlib.astlib",
    visibility = ["PUBLIC"],
    bytecode_lib = "opam/lib/ppxlib/astlib/astlib.cma",
    native_lib = "opam/lib/ppxlib/astlib/astlib.cmxa",
    include_dir = "opam/lib/ppxlib/astlib",
    deps = [
        ":ocaml.compiler-libs.common",
        ":ocaml-compiler-libs.common",
    ]
) if not host_info().os.is_windows else None

prebuilt_ocaml_library(
    name = "ppxlib.print_diff",
    visibility = ["PUBLIC"],
    bytecode_lib = "opam/lib/ppxlib/print_diff/ppxlib_print_diff.cma",
    native_lib = "opam/lib/ppxlib/print_diff/ppxlib_print_diff.cmxa",
    include_dir = "opam/lib/ppxlib/print_diff",
) if not host_info().os.is_windows else None

prebuilt_ocaml_library(
    name = "ppxlib.ast",
    visibility = ["PUBLIC"],
    bytecode_lib = "opam/lib/ppxlib/ast/ppxlib_ast.cma",
    native_lib = "opam/lib/ppxlib/ast/ppxlib_ast.cmxa",
    include_dir = "opam/lib/ppxlib/ast",
    deps = [ ":ppxlib.astlib" ]
) if not host_info().os.is_windows else None

prebuilt_ocaml_library(
    name = "sexplib0",
    visibility = ["PUBLIC"],
    bytecode_lib = "opam/lib/sexplib0/sexplib0.cma",
    native_lib = "opam/lib/sexplib0/sexplib0.cmxa",
    include_dir = "opam/lib/sexplib0",
) if not host_info().os.is_windows else None

prebuilt_ocaml_library(
    name = "stdlib-shims",
    visibility = ["PUBLIC"],
    bytecode_lib = "opam/lib/stdlib-shims/stdlib_shims.cma",
    native_lib = "opam/lib/stdlib-shims/stdlib_shims.cmxa",
    include_dir = "opam/lib/stdlib-shims",
) if not host_info().os.is_windows else None

prebuilt_ocaml_library(
    name = "ppxlib.stdppx",
    visibility = ["PUBLIC"],
    bytecode_lib = "opam/lib/ppxlib/stdppx/stdppx.cma",
    native_lib = "opam/lib/ppxlib/stdppx/stdppx.cmxa",
    include_dir = "opam/lib/ppxlib/stdppx",
    deps = [
        ":sexplib0",
        ":stdlib-shims",
    ]
) if not host_info().os.is_windows else None

prebuilt_ocaml_library(
    name = "ppxlib",
    visibility = ["PUBLIC"],
    bytecode_lib = "opam/lib/ppxlib/ppxlib.cma",
    include_dir = "opam/lib/ppxlib",
    native_lib = "opam/lib/ppxlib/ppxlib.cmxa",
    deps = [
        ":ocaml-compiler-libs.shadow",
        ":ppx_derivers",
        ":ppxlib.ast",
        ":ppxlib.traverse_builtins",
        ":ppxlib.stdppx",
        ":ppxlib.print_diff",
    ],
) if not host_info().os.is_windows else None
