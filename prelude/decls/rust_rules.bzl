# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:link_groups_types.bzl", "LINK_GROUP_MAP_ATTR")
load("@prelude//decls:cxx_rules.bzl", "BUILD_INFO_ATTR")
load("@prelude//decls:test_common.bzl", "test_common")
load("@prelude//decls:third_party_common.bzl", "third_party_common")
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//rust:clippy_configuration.bzl", "ClippyConfiguration")
load("@prelude//rust:link_info.bzl", "RustProcMacroPlugin")
load("@prelude//rust:rust_binary.bzl", "rust_binary_impl", "rust_test_impl")
load("@prelude//rust:rust_library.bzl", "rust_library_impl")
load(":common.bzl", "buck", "prelude_rule")
load(":native_common.bzl", "native_common")
load(":re_test_common.bzl", "re_test_common")
load(":rust_common.bzl", "rust_common", "rust_target_dep")

def _rust_common_attributes(is_binary: bool):
    return buck.licenses_arg() | buck.labels_arg() | buck.contacts_arg() | {
        "clippy_configuration": attrs.option(attrs.dep(providers = [ClippyConfiguration]), default = None),
        "coverage": attrs.bool(default = False),
        "default_host_platform": attrs.option(attrs.configuration_label(), default = None),
        "default_platform": attrs.option(attrs.string(), default = None),
        "flagged_deps": attrs.list(attrs.tuple(rust_target_dep(is_binary), attrs.list(attrs.string())), default = []),
        "incremental_enabled": attrs.bool(default = False),
        "resources": attrs.named_set(attrs.one_of(attrs.dep(), attrs.source()), sorted = True, default = []),
        "rustdoc_flags": attrs.list(attrs.arg(), default = []),
        "separate_debug_info": attrs.bool(default = False),
        "use_content_based_paths": attrs.bool(default = True),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_target_os_type": buck.target_os_type_arg(),
    }

def _rust_binary_attrs_group(prefix: str) -> dict[str, Attr]:
    attrs = (rust_common.deps_arg(is_binary = True) |
             rust_common.named_deps_arg(is_binary = True) |
             rust_common.linker_flags_arg() |
             native_common.link_style())
    return {prefix + name: v for name, v in attrs.items()}

_RUST_EXECUTABLE_ATTRIBUTES = {
    "anonymous_link_groups": attrs.bool(default = True),
    # Unlike cxx which supports pre-defined link groups, we only support
    # auto_link_groups in rust
    "auto_link_groups": attrs.bool(default = True),
    # TODO: enable distributed thinlto
    "enable_distributed_thinlto": attrs.bool(default = False),
    "extra_dwp_flags": attrs.list(attrs.string(), default = []),
    # Required by the rules but not supported, since Rust is auto-link groups only
    "link_group": attrs.default_only(attrs.option(attrs.string(), default = None)),
    "link_group_map": LINK_GROUP_MAP_ATTR,
    "link_group_min_binary_node_count": attrs.option(attrs.int(), default = None),
    "rpath": attrs.bool(default = False, doc = """
              Set the "rpath" in the executable when using a shared link style.
          """),
    "_build_info": BUILD_INFO_ATTR,
}

rust_binary = prelude_rule(
    name = "rust_binary",
    impl = rust_binary_impl,
    docs = """
        A rust\\_binary() rule builds a native executable from the supplied set of Rust source files
        and dependencies.


        If you invoke a build with the `check` flavor, then Buck will invoke rustc
        to check the code (typecheck, produce warnings, etc), but won't generate an executable code.
        When applied to binaries it produces no output; for libraries it produces metadata for
        consumers of the library.


        Note: Buck is currently tested with (and therefore supports) version 1.32.0 of Rust.
    """,
    examples = """
        For more examples, check out our [integration tests](https://github.com/facebook/buck/tree/dev/test/com/facebook/buck/rust/testdata/).


        ```
        rust_binary(
          name='greet',
          srcs=[
            'greet.rs',
          ],
          deps=[
            ':greeting',
          ],
        )

        rust_library(
          name='greeting',
          srcs=[
            'greeting.rs',
          ],
          deps=[
            ':join',
          ],
        )

        rust_library(
          name='join',
          srcs=[
            'join.rs',
          ],
        )
        ```
    """,
    further = None,
    attrs = (
        # @unsorted-dict-items
        rust_common.srcs_arg() |
        rust_common.srcs_filegroup_arg() |
        rust_common.mapped_srcs_arg() |
        rust_common.edition_arg() |
        rust_common.features_arg() |
        rust_common.rustc_flags_arg() |
        rust_common.crate(crate_type = attrs.option(attrs.string(), default = None)) |
        rust_common.crate_root() |
        rust_common.env_arg() |
        _rust_binary_attrs_group(prefix = "") |
        _rust_common_attributes(is_binary = True) |
        _RUST_EXECUTABLE_ATTRIBUTES |
        rust_common.cxx_toolchain_arg() |
        rust_common.rust_toolchain_arg() |
        rust_common.workspaces_arg() |
        native_common.transformation_spec_arg() |
        buck.allow_cache_upload_arg()
    ),
    uses_plugins = [RustProcMacroPlugin],
    supports_incoming_transition = True,
)

rust_library = prelude_rule(
    name = "rust_library",
    impl = rust_library_impl,
    docs = """
        A rust\\_library() rule builds a native library from the supplied set of Rust source files
        and dependencies.


        If you invoke a build with the `check` flavor, then Buck will invoke rustc
        to check the code (typecheck, produce warnings, etc), but won't generate an executable code.
        When applied to binaries it produces no output; for libraries it produces metadata for
        consumers of the library.


        Note: Buck is currently tested with (and therefore supports) version 1.32.0 of Rust.
    """,
    examples = """
        For more examples, check out our [integration tests](https://github.com/facebook/buck/tree/dev/test/com/facebook/buck/rust/testdata/).


        ```
        rust_library(
          name='greeting',
          srcs=[
            'greeting.rs',
          ],
          deps=[
            ':join',
          ],
        )
        ```
    """,
    further = None,
    attrs = (
        # @unsorted-dict-items
        rust_common.srcs_arg() |
        rust_common.srcs_filegroup_arg() |
        rust_common.mapped_srcs_arg() |
        rust_common.deps_arg(is_binary = False) |
        rust_common.named_deps_arg(is_binary = False) |
        rust_common.edition_arg() |
        rust_common.features_arg() |
        rust_common.rustc_flags_arg() |
        # linker_flags weren't supported for rust_library in Buck v1 but the
        # fbcode macros pass them anyway. They're typically empty since the
        # config-level flags don't get injected, but it doesn't hurt to accept
        # them and it simplifies the implementation of Rust rules since they
        # don't have to know whether we're building a rust_binary or a
        # rust_library.
        rust_common.linker_flags_arg() |
        rust_common.exported_linker_flags_arg() |
        rust_common.env_arg() |
        rust_common.crate(crate_type = attrs.option(attrs.string(), default = None)) |
        rust_common.crate_root() |
        native_common.preferred_linkage(preferred_linkage_type = attrs.enum(Linkage.values(), default = "any")) |
        native_common.soname() |
        native_common.link_style() |
        native_common.link_whole(link_whole_type = attrs.option(attrs.bool(), default = None)) |
        _rust_common_attributes(is_binary = False) |
        {
            "crate_dynamic": attrs.option(attrs.dep(), default = None),
            "doc_env": rust_common.env_arg()["env"],
            "doctests": attrs.option(attrs.bool(), default = None),
            "proc_macro": attrs.bool(default = False),
            "supports_python_dlopen": attrs.option(attrs.bool(), default = None),
        } |
        _rust_binary_attrs_group(prefix = "doc_") |
        rust_common.cxx_toolchain_arg() |
        rust_common.rust_toolchain_arg() |
        rust_common.workspaces_arg() |
        third_party_common.create_third_party_build_root_attrs()
    ),
    uses_plugins = [RustProcMacroPlugin],
    supports_incoming_transition = True,
)

rust_test = prelude_rule(
    name = "rust_test",
    impl = rust_test_impl,
    docs = """
        A rust\\_test() rule builds a Rust test native executable from the supplied set of Rust source
        files and dependencies and runs this test.


        Note: Buck is currently tested with (and therefore supports) version 1.32.0 of Rust.
    """,
    examples = """
        For more examples, check out our [integration tests](https://github.com/facebook/buck/tree/dev/test/com/facebook/buck/rust/testdata/).


        ```
        rust_test(
          name='greet',
          srcs=[
            'greet.rs',
          ],
          deps=[
            ':greeting',
          ],
        )

        rust_library(
          name='greeting',
          srcs=[
            'greeting.rs',
          ],
          deps=[
            ':join',
          ],
        )

        rust_library(
          name='join',
          srcs=[
            'join.rs',
          ],
        )
        ```
    """,
    further = None,
    attrs = (
        # @unsorted-dict-items
        buck.inject_test_env_arg() |
        rust_common.srcs_arg() |
        rust_common.srcs_filegroup_arg() |
        rust_common.mapped_srcs_arg() |
        rust_common.edition_arg() |
        rust_common.features_arg() |
        rust_common.rustc_flags_arg() |
        rust_common.crate(crate_type = attrs.option(attrs.string(), default = None)) |
        rust_common.crate_root() |
        rust_common.default_roots_arg() |
        rust_common.run_env_arg() |
        rust_common.build_and_run_env_arg() |
        _rust_binary_attrs_group(prefix = "") |
        _rust_common_attributes(is_binary = True) |
        _RUST_EXECUTABLE_ATTRIBUTES |
        {
            "framework": attrs.bool(default = True, doc = """
                Use the standard test framework. If this is set to false, then the result is a normal
                executable which requires a `main()`, etc. It is still expected to accept the
                same command-line parameters and produce the same output as the test framework.
            """),
        } |
        re_test_common.test_args() |
        rust_common.cxx_toolchain_arg() |
        rust_common.rust_toolchain_arg() |
        rust_common.workspaces_arg() |
        native_common.transformation_spec_arg() |
        test_common.attributes()
    ),
    uses_plugins = [RustProcMacroPlugin],
    supports_incoming_transition = True,
)

rust_rules = struct(
    rust_binary = rust_binary,
    rust_library = rust_library,
    rust_test = rust_test,
)
