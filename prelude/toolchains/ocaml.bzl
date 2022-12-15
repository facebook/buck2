# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//ocaml/providers.bzl",
    "OCamlPlatformInfo",
    "OCamlToolchainInfo",
)

def _system_ocaml_toolchain_impl(_ctx):
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
            interop_includes = None,
            libc = None,
            ocaml_bytecode_compiler = RunInfo(args = ["ocamlc.opt"]),
            debug = RunInfo(args = ["ocamldebug"]),
            warnings_flags = "-4-29-35-41-42-44-45-48-50-58-70",
            ocaml_compiler_flags = [],  # e.g. "-opaque"
        ),
        OCamlPlatformInfo(name = "x86_64"),
    ]

system_ocaml_toolchain = rule(
    impl = _system_ocaml_toolchain_impl,
    attrs = {},
    is_toolchain_rule = True,
)
