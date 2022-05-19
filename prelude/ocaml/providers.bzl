# Providers for OCaml build rules.

load("@fbcode//buck2/prelude/utils:utils.bzl", "flatten")

OCamlToolchainInfo = provider(fields = [
    "ocaml_compiler",
    # [Note: What is `binutils_ld`?]
    # ------------------------------
    # When the compiler is invoked in partial linking mode (`ocamlopt.opt
    # -output-obj ...`) it makes naked calls to `ld`. In such a case, if we
    # don't arrange to execute `ocamlopt.opt` in an environment where `ld`
    # resolves to an `ld` that is either the `ld` in scope when the compiler was
    # built (or at least compatible with it) it then a call to the system `ld`
    # will result and almost certainly be a mismatch.
    #
    # How the compiler invokes `ld` is established when it's built by its
    # configure script
    # (https://github.com/ocaml/ocaml/blob/f27d671b23f5246d37d91571eeccb802d5399a0b/configure.ac).
    #
    # So far I've found `fbcode//.../binutils:bin/ld` to be a choice that works.
    # It's in `_mk_ocaml_opt` in `ocaml.bzl` where we make use of this.
    "binutils_ld",
    "dep_tool",
    "yacc_compiler",
    "lex_compiler",
    "libasmrun",  # The location of 'libasmrun.a'.
    "ocaml_bytecode_compiler",
    "debug",
    "interop_includes",
    "warnings_flags",
])

# Stores "platform"/flavor name used to resolve *platform_* arguments
OCamlPlatformInfo = provider(fields = [
    "name",
])

# A list of `OCamlLibraryInfo`s.
OCamlLinkInfo = provider(
    # Contains a list of OCamlLibraryInfo records
    fields = ["info"],
)

# A record of an OCaml library.
OCamlLibraryInfo = record(
    # The library target name: e.g. "`foo`"
    name = str.type,
    # .a (C archives e.g. `libfoo_stubs.a`)
    c_libs = ["artifact"],
    # .o (Native compiler produced stubs)
    stbs_nat = ["artifact"],
    # .o (Bytecode compiler produced stubs)
    stbs_byt = ["artifact"],
    # .cma (Bytecode compiler module archives e.g. `libfoo.cma`)
    cmas = ["artifact"],
    # .cmxa (Native compiler module archives e.g. `libfoo.cmxa`)
    cmxas = ["artifact"],
    # .cmi (Native compiled module interfaces)
    cmis_nat = ["artifact"],
    # .cmi (Bytecode compiled module interfaces)
    cmis_byt = ["artifact"],
    # .cmo (Bytecode compiled modules - bytecode)
    cmos = ["artifact"],
    # .cmx (Compiled modules - native)
    cmxs = ["artifact"],
    # .cmt (Native compiler produced typed abstract syntax trees)
    cmts_nat = ["artifact"],
    # .cmt (Bytecode compiler produced typed abstract syntax trees)
    cmts_byt = ["artifact"],
    # .cmti (Native compiler produced typed abstract syntax trees)
    cmtis_nat = ["artifact"],
    # .cmti (Bytecode compiler produced typed abstract syntax trees)
    cmtis_byt = ["artifact"],
    # External dependency include paths
    include_dirs = ["cmd_args"],
    # Native C libs (like `libthreadsnat.a` in the compiler's `threads` package)
    native_c_libs = ["artifact"],
    # Bytecode C libs (like `libthreads.a` in the compiler's `threads` package)
    bytecode_c_libs = ["artifact"],
)

def merge_ocaml_link_infos(lis: ["OCamlLinkInfo"]) -> "OCamlLinkInfo":
    return OCamlLinkInfo(info = dedupe(flatten([li.info for li in lis])))
