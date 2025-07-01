# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//rust:link_info.bzl", "RustProcMacroPlugin")
load(":toolchains_common.bzl", "toolchains_common")

def rust_target_dep(is_binary: bool) -> Attr:
    return attrs.dep(
        pulls_and_pushes_plugins = [] if is_binary else [RustProcMacroPlugin],
        pulls_plugins = [RustProcMacroPlugin] if is_binary else [],
    )

def _deps_arg(is_binary: bool):
    return {
        "deps": attrs.list(
            rust_target_dep(is_binary),
            default = [],
            doc = """
    The set of dependencies of this rule. Currently, this supports rust\\_library
     and prebuilt\\_rust\\_library rules.
""",
        ),
    }

def _srcs_arg():
    return {
        "srcs": attrs.list(attrs.source(), default = [], doc = """
    The set of Rust source files to be compiled by this rule.


     One of the source files is the root module of the crate. By default this is `lib.rs` for libraries, `main.rs` for executables, or
     the crate's name with `.rs` appended. This can be overridden with the `crate_root` rule parameter.
"""),
    }

def _features_arg():
    return {
        "features": attrs.list(attrs.string(), default = [], doc = """
    The set of features to be enabled for this rule.


     These are passed to `rustc` with `--cfg feature="{feature}"`, and can be used in the code with `#[cfg(feature = "{feature}")]`.
"""),
    }

def _edition_arg():
    return {
        "edition": attrs.option(attrs.string(), default = None, doc = """
    Set the language edition to be used for this rule. Can be set to any edition the
     compiler supports (`2018` right now). If unset it uses the compiler's default
     (`2015`).
"""),
    }

def _rustc_flags_arg():
    return {
        "rustc_flags": attrs.list(attrs.arg(), default = [], doc = """
    The set of additional compiler flags to pass to `rustc`.
"""),
    }

def _linker_flags_arg():
    return {
        "linker_flags": attrs.list(attrs.arg(anon_target_compatible = True), default = [], doc = """
    The set of additional flags to pass to the linker.
"""),
    }

def _exported_linker_flags_arg():
    return {
        "exported_linker_flags": attrs.list(attrs.arg(anon_target_compatible = True), default = [], doc = """
    A set of additional flag to pass before this item on the link line, even if
    this items is compiled to a DSO.
"""),
        "exported_post_linker_flags": attrs.list(attrs.arg(anon_target_compatible = True), default = [], doc = """
    A set of additional flag to pass after this item on the link line, even if
    this items is compiled to a DSO.
"""),
    }

def _crate(crate_type):
    return {
        "crate": crate_type,
    }

def _crate_root():
    return {
        "crate_root": attrs.option(attrs.string(), default = None, doc = """
    Set the name of the top-level source file for the crate, which can be used to override the
     default (see `srcs`).
"""),
    }

def _default_roots_arg():
    return {
        "default_roots": attrs.option(attrs.list(attrs.string()), default = None, doc = """
    Set the candidate source names to consider for crate root. Typically used to disambiguate between
     lib.rs or main.rs for rust_test, which may be declare a test suite for either library or binary
     rules. Has no effect if an explicit `crate_root` is provided.
"""),
    }

def _env_arg():
    return {
        "env": attrs.dict(key = attrs.string(), value = attrs.arg(), sorted = False, default = {}, doc = """
    Set environment variables for this rule's invocations of rustc. The environment variable
     values may include macros which are expanded.
"""),
    }

def _run_env_arg():
    return {
        "run_env": attrs.dict(key = attrs.string(), value = attrs.arg(), sorted = False, default = {}, doc = """
    Set environment variables during test execution. The environment variable values may
     include macros which are expanded.
"""),
    }

def _build_and_run_env_arg():
    # Same as env_arg(), but with different documentation.
    return {
        "env": attrs.dict(key = attrs.string(), value = attrs.arg(), sorted = False, default = {}, doc = """
    Set environment variables for this rule's invocations of rustc *and* during execution of
     the tests. The environment variable values may include macros which are expanded.
"""),
    }

def _mapped_srcs_arg():
    return {
        "mapped_srcs": attrs.dict(key = attrs.source(), value = attrs.string(), sorted = False, default = {}, doc = """
    Add source files along with a local path mapping. Rust is sensitive to the layout of
     source files, as the directory structure follows the module structure. However this is
     awkward if the source file is, for example, generated by another rule. In this case, you
     can set up a mapping from the actual source path to something that makes sense locally.
     For example `mapped_srcs = {":generate-module", "src/generated.rs" }`.
     These are added to the regular `srcs`, so a file should not be listed in
     both.
"""),
    }

def _named_deps_arg(is_binary: bool):
    return {
        "named_deps": attrs.one_of(attrs.dict(key = attrs.string(), value = rust_target_dep(is_binary), sorted = False), attrs.list(attrs.tuple(attrs.arg(), rust_target_dep(is_binary))), default = {}, doc = """
    Add crate dependencies and define a local name by which to use that dependency by. This
     allows a crate to have multiple dependencies with the same crate name. For example:
     `named_deps = {"local_name", ":some_rust_crate" }`.
     The dependencies may also be non-Rust, but the alias is ignored. It has no effect on the
     symbols provided by a C/C++ library.
"""),
    }

def _rust_toolchain_arg():
    return {
        "_rust_internal_tools_toolchain": attrs.default_only(
            attrs.toolchain_dep(default = "prelude//rust/tools:internal_tools_toolchain"),
        ),
        "_rust_toolchain": toolchains_common.rust(),
    }

def _cxx_toolchain_arg():
    return {
        "_cxx_toolchain": toolchains_common.cxx(),
    }

def _workspaces_arg():
    return {
        "_workspaces": attrs.list(attrs.label(), default = [], doc = """
    Internal implementation detail of Rust workspaces. This should not be set manually and will be
     replaced in favor of metadata in a future version of buck2.
"""),
    }

rust_common = struct(
    deps_arg = _deps_arg,
    srcs_arg = _srcs_arg,
    features_arg = _features_arg,
    edition_arg = _edition_arg,
    rustc_flags_arg = _rustc_flags_arg,
    linker_flags_arg = _linker_flags_arg,
    exported_linker_flags_arg = _exported_linker_flags_arg,
    crate = _crate,
    crate_root = _crate_root,
    default_roots_arg = _default_roots_arg,
    env_arg = _env_arg,
    run_env_arg = _run_env_arg,
    build_and_run_env_arg = _build_and_run_env_arg,
    mapped_srcs_arg = _mapped_srcs_arg,
    named_deps_arg = _named_deps_arg,
    rust_toolchain_arg = _rust_toolchain_arg,
    cxx_toolchain_arg = _cxx_toolchain_arg,
    workspaces_arg = _workspaces_arg,
)
