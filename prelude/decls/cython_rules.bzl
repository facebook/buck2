# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//decls:common.bzl", "buck", "prelude_rule")
load("@prelude//decls:cxx_rules.bzl", "cxx_rules")
load("@prelude//decls:python_common.bzl", "python_common")
load("@prelude//decls:toolchains_common.bzl", "toolchains_common")

cython_library = prelude_rule(
    name = "cython_library",
    docs = """
        A rule that compiles Cython (.pyx) sources into Python C extensions.

        This rule handles the full pipeline: Cython transpilation to C/C++,
        compilation to shared libraries, and assembly into a Python library.
    """,
    attrs = (
        # Inherit all cxx_library attrs as default_only so cxx_library_parameterized
        # can access required attributes like supported_platforms_regex, preferred_linkage, etc.
        # This mirrors how cxx_python_extension inherits these attrs.
        # @unsorted-dict-items
        {k: attrs.default_only(v) for k, v in cxx_rules.cxx_library.attrs.items()} |
        python_common.base_module_arg() |
        buck.labels_arg() |
        buck.contacts_arg() |
        {
            "allow_embedding": attrs.option(attrs.bool(), default = None, doc = """
                Whether to allow embedding this extension into the main binary.
            """),
            "api": attrs.list(attrs.string(), default = [], doc = """
                List of module names that should generate public API headers
                (_api.h files) for C++ interop.
            """),
            "code_comments": attrs.bool(default = False, doc = """
                Whether to include source code comments in the generated output.
            """),
            "cpp_compiler_flags": attrs.list(attrs.arg(), default = [], doc = """
                Additional flags to pass to the C++ compiler.
            """),
            "cpp_deps": attrs.list(attrs.dep(), default = [], doc = """
                C++ library dependencies for linking.
            """),
            "cpp_external_deps": attrs.list(attrs.string(), default = [], doc = """
                External C++ dependencies specified by name.
            """),
            "cpp_preprocessor_flags": attrs.list(attrs.arg(), default = [], doc = """
                Additional C++ preprocessor flags.
            """),
            "cpp_python_extension_srcs": attrs.list(attrs.source(), default = [], doc = """
                Additional C++ source files to compile into the Python extensions.
            """),
            "cython_annotate": attrs.option(attrs.enum(["off", "basic", "fullc"]), default = None, doc = """
                Level of Cython annotation to generate.
            """),
            "cython_binding": attrs.bool(default = False, doc = """
                Whether this is a Cython binding module.
            """),
            "cython_compiler": attrs.option(attrs.dep(providers = [RunInfo]), default = None, doc = """
                Override the default Cython compiler.
            """),
            "cython_so_package": attrs.option(attrs.string(), default = None, doc = """
                Override the package path for the generated .so file.
            """),
            "deps": attrs.list(attrs.dep(), default = [], doc = """
                Dependencies of this rule, including other cython_library
                and python_library targets.
            """),
            "flags": attrs.list(attrs.string(), default = [], doc = """
                Additional flags to pass to the Cython compiler.
            """),
            "generate_cpp": attrs.bool(default = True, doc = """
                Whether to generate C++ (.cpp) or C (.c) output from Cython.
            """),
            "header_namespace": attrs.option(attrs.string(), default = None, doc = """
                A namespace for the exported headers. If set, headers will be
                available under this namespace for dependent rules.
            """),
            "headers": attrs.named_set(attrs.source(), sorted = True, default = [], doc = """
                Cython header files (.pxd) to expose to dependent Cython rules.
            """),
            "includes": attrs.named_set(attrs.source(), sorted = True, default = [], doc = """
                Additional Cython include files (.pxd, .pxi) for this library.
            """),
            "legacy_noexcept": attrs.bool(default = True, doc = """
                Whether to use legacy implicit noexcept behavior.
            """),
            "need_static_lib": attrs.bool(default = False, doc = """
                Whether to also produce a static C++ library.
            """),
            "public_declarations": attrs.list(attrs.string(), default = [], doc = """
                List of public declaration module names.
            """),
            "python_deps": attrs.list(attrs.dep(), default = [], doc = """
                Python-only dependencies.
            """),
            "python_external_deps": attrs.list(attrs.string(), default = [], doc = """
                External Python dependencies specified by name.
            """),
            "srcs": attrs.named_set(attrs.source(), sorted = True, default = [], doc = """
                The Cython source files (.pyx) and header files (.pxd, .pxi)
                to include in this library.
            """),
            "suffix_all": attrs.option(attrs.bool(), default = None, doc = """
                Whether to suffix all symbols for static linking.
            """),
            "types": attrs.list(attrs.source(), default = [], doc = """
                Type stub files (.pyi) for the generated modules.
            """),
            "typing": attrs.bool(default = False, doc = """
                Whether to enable type checking support.
            """),
            # Override inherited cxx_library attrs that need concrete defaults
            "preferred_linkage": attrs.enum(["any", "static", "shared"], default = "any"),
            # Private/toolchain attrs
            "_cxx_toolchain": toolchains_common.cxx(),
            "_cython_toolchain": toolchains_common.cython(),
            "_exec_os_type": buck.exec_os_type_arg(),
            "_python_toolchain": toolchains_common.python(),
            "_target_os_type": buck.target_os_type_arg(),
        }
    ),
)

cython_static_extension = prelude_rule(
    name = "cython_static_extension",
    docs = """
        A rule that compiles a single Cython (.pyx) source into a statically-linkable
        Python C extension.

        Similar to cython_library but simplified for single-source use and
        produces extensions suitable for static linking into binaries.
    """,
    attrs = (
        # Inherit all cxx_library attrs as default_only so cxx_library_parameterized
        # can access required attributes like supported_platforms_regex, preferred_linkage, etc.
        # @unsorted-dict-items
        {k: attrs.default_only(v) for k, v in cxx_rules.cxx_library.attrs.items()} |
        python_common.base_module_arg() |
        {
            "compiler_flags": attrs.list(attrs.arg(), default = [], doc = """
                Additional C++ compiler flags.
            """),
            "cython_binding": attrs.bool(default = False, doc = """
                Whether this is a Cython binding module.
            """),
            "cython_code_comments": attrs.bool(default = False, doc = """
                Whether to include source code comments in the generated output.
            """),
            "cython_compiler": attrs.option(attrs.dep(providers = [RunInfo]), default = None, doc = """
                Override the default Cython compiler.
            """),
            "cython_deps": attrs.list(attrs.dep(), default = [], doc = """
                Cython-specific dependencies (other cython_library or
                cython_static_extension targets).
            """),
            "cython_fast_fail": attrs.bool(default = True, doc = """
                Whether to enable --fast-fail for Cython compilation.
            """),
            "cython_flags": attrs.list(attrs.string(), default = [], doc = """
                Additional flags to pass to the Cython compiler.
            """),
            "cython_generate_cpp": attrs.bool(default = True, doc = """
                Whether to generate C++ (.cpp) or C (.c) output.
            """),
            "cython_headers": attrs.one_of(
                attrs.list(attrs.source()),
                attrs.dict(attrs.string(), attrs.source()),
                doc = """
                    Cython header files (.pxd) for this extension.
                    Can be a list of sources or a dict mapping names to sources.
                """,
            ),
            "cython_includes": attrs.one_of(
                attrs.list(attrs.source()),
                attrs.dict(attrs.string(), attrs.source()),
                doc = """
                    Cython include files (.pxd, .pxi) for this extension.
                    Can be a list of sources or a dict mapping names to sources.
                """,
            ),
            "cython_pyx": attrs.source(doc = """
                The single Cython .pyx source file.
            """),
            "cython_pyx_on_disk": attrs.option(attrs.source(), default = None, doc = """
                Optional on-disk override for the .pyx source.
            """),
            "cython_version": attrs.enum(["2", "3", "3str"], default = "3", doc = """
                The Cython language version to target.
            """),
            "deps": attrs.list(attrs.dep(), default = [], doc = """
                C++ library dependencies.
            """),
            "legacy_noexcept": attrs.bool(default = True, doc = """
                Whether to use legacy implicit noexcept behavior.
            """),
            "preprocessor_flags": attrs.list(attrs.arg(), default = [], doc = """
                Additional C++ preprocessor flags.
            """),
            "public_deps": attrs.list(attrs.dep(), default = [], doc = """
                Public C++ dependencies that are re-exported.
            """),
            "public_include_directories": attrs.list(attrs.string(), default = [], doc = """
                Public include directories for C++ consumers.
            """),
            "python_deps": attrs.list(attrs.dep(), default = [], doc = """
                Python dependencies.
            """),
            "python_types": attrs.list(attrs.source(), default = [], doc = """
                Type stub files (.pyi) for the generated extension.
            """),
            "raw_headers": attrs.list(attrs.source(), default = [], doc = """
                Raw C/C++ header files.
            """),
            # cxx attrs
            "srcs": attrs.list(attrs.source(), default = [], doc = """
                Additional C/C++ source files to compile alongside the
                Cython-generated source.
            """),
            # Override inherited cxx_library attrs that need concrete defaults
            "preferred_linkage": attrs.enum(["any", "static", "shared"], default = "any"),
            # Toolchain attrs
            "_cxx_toolchain": toolchains_common.cxx(),
            "_cython_toolchain": toolchains_common.cython(),
            "_exec_os_type": buck.exec_os_type_arg(),
            "_python_toolchain": toolchains_common.python(),
            "_target_os_type": buck.target_os_type_arg(),
        }
    ),
)

cython_toolchain_rule = prelude_rule(
    name = "cython_toolchain",
    docs = """
        A toolchain rule that provides the Cython compiler configuration.

        Python version-based compiler selection is handled via select() +
        py_version_select() on the "compiler" attribute in the toolchain
        BUCK definition, mirroring the fbsource//third-party/pypi/cython:compiler
        alias pattern.
    """,
    is_toolchain_rule = True,
    attrs = {
        "compiler": attrs.dep(providers = [RunInfo], doc = """
            The Cython compiler binary target. Use select() + py_version_select()
            to vary the compiler by Python version.
        """),
        "default_flags": attrs.list(attrs.string(), default = [], doc = """
            Default flags to pass to the Cython compiler.
        """),
    },
)

cython_rules = struct(
    cython_library = cython_library,
    cython_static_extension = cython_static_extension,
    cython_toolchain = cython_toolchain_rule,
)
