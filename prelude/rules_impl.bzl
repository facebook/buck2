# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:alias.bzl", "alias_impl", "configured_alias_impl", "versioned_alias_impl")
load("@prelude//:command_alias.bzl", "command_alias_impl")
load("@prelude//:export_file.bzl", "export_file_impl")
load("@prelude//:filegroup.bzl", "filegroup_impl")
load("@prelude//:genrule.bzl", "genrule_attributes", "genrule_impl")
load("@prelude//:http_file.bzl", "http_file_impl")
load("@prelude//:remote_file.bzl", "remote_file_impl")
load("@prelude//:sh_binary.bzl", "sh_binary_impl")
load("@prelude//:sh_test.bzl", "sh_test_impl")
load("@prelude//:test_suite.bzl", "test_suite_impl")
load("@prelude//android:android.bzl", _android_extra_attributes = "extra_attributes", _android_implemented_rules = "implemented_rules")
load("@prelude//android:configuration.bzl", "is_building_android_binary_attr")
load("@prelude//apple:apple_common.bzl", "apple_common")
load("@prelude//apple:apple_rules_decls.bzl", "apple_rules")
load("@prelude//apple:apple_rules_impl.bzl", _apple_extra_attributes = "extra_attributes", _apple_implemented_rules = "implemented_rules")
load("@prelude//configurations:rules.bzl", _config_extra_attributes = "extra_attributes", _config_implemented_rules = "implemented_rules")
load("@prelude//csharp:csharp.bzl", "csharp_library_impl", "prebuilt_dotnet_library_impl")
load("@prelude//cxx:bitcode.bzl", "llvm_link_bitcode_impl")
load("@prelude//cxx:cuda.bzl", "CudaCompileStyle")
load("@prelude//cxx:cmake.bzl", "cmake_configure_file_impl", "cmake_type_size_substitution_impl", "cmake_substitution_impl", "cmake_immediate_substitution_impl")
load("@prelude//utils:value.bzl", "generic_simple_value_impl", "generic_file_value_impl", "generic_list_value_impl", "generic_value_mapping_impl", "generic_value_join_list_impl")
load("@prelude//cxx:cxx.bzl", "cxx_binary_impl", "cxx_library_impl", "cxx_precompiled_header_impl", "cxx_test_impl", "prebuilt_cxx_library_impl")
load("@prelude//cxx:cxx_toolchain.bzl", "cxx_toolchain_extra_attributes", "cxx_toolchain_impl")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo", "CxxToolchainInfo")
load("@prelude//cxx:headers.bzl", "CPrecompiledHeaderInfo", "HeaderMode")
load("@prelude//cxx:link_groups_types.bzl", "LINK_GROUP_MAP_ATTR")
load("@prelude//cxx:prebuilt_cxx_library_group.bzl", "prebuilt_cxx_library_group_impl")
load("@prelude//cxx:transformation_spec.bzl", "TransformationKind", "transformation_spec_impl")
load("@prelude//cxx:windows_resource.bzl", "windows_resource_impl")
load("@prelude//decls:android_rules.bzl", "android_rules")
load("@prelude//decls:common.bzl", "IncludeType", "buck")
load("@prelude//decls:core_rules.bzl", "core_rules")
load("@prelude//decls:cxx_rules.bzl", "BUILD_INFO_ATTR", "cxx_rules")
load("@prelude//decls:d_rules.bzl", "d_rules")
load("@prelude//decls:dotnet_rules.bzl", "dotnet_rules")
load("@prelude//decls:erlang_rules.bzl", "erlang_rules")
load("@prelude//decls:git_rules.bzl", "git_rules")
load("@prelude//decls:go_rules.bzl", "go_rules")
load("@prelude//decls:groovy_rules.bzl", "groovy_rules")
load("@prelude//decls:halide_rules.bzl", "halide_rules")
load("@prelude//decls:haskell_rules.bzl", "haskell_rules")
load("@prelude//decls:java_rules.bzl", "java_rules")
load("@prelude//decls:js_rules.bzl", "js_rules")
load("@prelude//decls:kotlin_rules.bzl", "kotlin_rules")
load("@prelude//decls:lua_rules.bzl", "lua_rules")
load("@prelude//decls:ocaml_rules.bzl", "ocaml_rules")
load("@prelude//decls:python_rules.bzl", "python_rules")
load("@prelude//decls:re_test_common.bzl", "re_test_common")
load("@prelude//decls:rust_rules.bzl", "rust_rules")
load("@prelude//decls:scala_rules.bzl", "scala_rules")
load("@prelude//decls:shell_rules.bzl", "shell_rules")
load("@prelude//decls:third_party_common.bzl", "third_party_common")
load("@prelude//decls:toolchains_common.bzl", "toolchains_common")
load("@prelude//decls:uncategorized_rules.bzl", "uncategorized_rules")
load("@prelude//erlang:erlang.bzl", _erlang_implemented_rules = "implemented_rules")
load("@prelude//git:git_fetch.bzl", "git_fetch_impl")
load("@prelude//go:coverage.bzl", "GoCoverageMode")
load("@prelude//go:go_binary.bzl", "go_binary_impl")
load("@prelude//go:go_exported_library.bzl", "go_exported_library_impl")
load("@prelude//go:go_library.bzl", "go_library_impl")
load("@prelude//go:go_stdlib.bzl", "go_stdlib_impl")
load("@prelude//go:go_test.bzl", "go_test_impl")
load("@prelude//go/transitions:defs.bzl", "build_tags_attr", "cgo_enabled_attr", "coverage_mode_attr")
load("@prelude//go_bootstrap:go_bootstrap.bzl", "go_bootstrap_binary_impl")
load("@prelude//haskell:haskell.bzl", "haskell_binary_impl", "haskell_library_impl", "haskell_prebuilt_library_impl")
load("@prelude//haskell:haskell_ghci.bzl", "haskell_ghci_impl")
load("@prelude//haskell:haskell_haddock.bzl", "haskell_haddock_impl")
load("@prelude//haskell:haskell_ide.bzl", "haskell_ide_impl")
load("@prelude//haskell:library_info.bzl", "HaskellLibraryProvider")
load("@prelude//http_archive:http_archive.bzl", "http_archive_impl")
load("@prelude//java:java.bzl", _java_extra_attributes = "extra_attributes", _java_implemented_rules = "implemented_rules")
load("@prelude//js:js.bzl", _js_extra_attributes = "extra_attributes", _js_implemented_rules = "implemented_rules")
load("@prelude//js:worker_tool.bzl", "worker_tool")
load("@prelude//julia:julia.bzl", _julia_extra_attributes = "extra_attributes", _julia_implemented_rules = "implemented_rules")
load("@prelude//kotlin:kotlin.bzl", _kotlin_extra_attributes = "extra_attributes", _kotlin_implemented_rules = "implemented_rules")
load("@prelude//linking:execution_preference.bzl", "link_execution_preference_attr")
load("@prelude//linking:link_info.bzl", "LinkOrdering")
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//lua:cxx_lua_extension.bzl", "cxx_lua_extension_impl")
load("@prelude//lua:lua_binary.bzl", "lua_binary_impl")
load("@prelude//lua:lua_library.bzl", "lua_library_impl")
load("@prelude//matlab:matlab.bzl", _matlab_extra_attributes = "extra_attributes", _matlab_implemented_rules = "implemented_rules")
load("@prelude//ocaml:attrs.bzl", _ocaml_extra_attributes = "ocaml_extra_attributes")
load("@prelude//ocaml:ocaml.bzl", "ocaml_binary_impl", "ocaml_library_impl", "ocaml_object_impl", "ocaml_shared_impl", "prebuilt_ocaml_library_impl")
load("@prelude//python:cxx_python_extension.bzl", "cxx_python_extension_impl")
load("@prelude//python:prebuilt_python_library.bzl", "prebuilt_python_library_impl")
load("@prelude//python:python_binary.bzl", "python_binary_impl")
load("@prelude//python:python_library.bzl", "python_library_impl")
load("@prelude//python:python_needed_coverage_test.bzl", "python_needed_coverage_test_impl")
load("@prelude//python:python_runtime_bundle.bzl", "python_runtime_bundle_impl")
load("@prelude//python:python_test.bzl", "python_test_impl")
load("@prelude//python_bootstrap:python_bootstrap.bzl", "PythonBootstrapSources", "python_bootstrap_binary_impl", "python_bootstrap_library_impl")
load("@prelude//third-party:providers.bzl", "ThirdPartyBuildInfo")
load("@prelude//transitions:constraint_overrides.bzl", "constraint_overrides")
load("@prelude//zip_file:zip_file.bzl", _zip_file_extra_attributes = "extra_attributes", _zip_file_implemented_rules = "implemented_rules")

_ANDROID_RULES_KEY = "android"
_CORE_RULES_KEY = "core"
_CXX_RULES_KEY = "cxx"
_D_RULES_KEY = "d"
_DOTNET_RULES_KEY = ".NET"
_ERLANG_RULES_KEY = "erlang"
_GIT_RULES_KEY = "git"
_GO_RULES_KEY = "go"
_GROOVY_RULES_KEY = "groovy"
_HALIDE_RULES_KEY = "halide"
_HASKELL_RULES_KEY = "haskell"
_APPLE_RULES_KEY = "apple"
_JAVA_RULES_KEY = "java"
_JS_RULES_KEY = "js"
_KOTLIN_RULES_KEY = "kotlin"
_LUA_RULES_KEY = "lua"
_OCAML_RULES_KEY = "ocaml"
_PYTHON_RULES_KEY = "python"
_RUST_RULES_KEY = "rust"
_SCALA_RULES_KEY = "scala"
_SHELL_RULES_KEY = "shell"
_UNCATEGORIZED_RULES_KEY = "uncategorized"

_JULIA_RULES_KEY = "julia"
_MATLAB_RULES_KEY = "matlab"

categorized_rule_decl_records = {
    _ANDROID_RULES_KEY: android_rules,
    _CORE_RULES_KEY: core_rules,
    _CXX_RULES_KEY: cxx_rules,
    _D_RULES_KEY: d_rules,
    _DOTNET_RULES_KEY: dotnet_rules,
    _ERLANG_RULES_KEY: erlang_rules,
    _GIT_RULES_KEY: git_rules,
    _GO_RULES_KEY: go_rules,
    _GROOVY_RULES_KEY: groovy_rules,
    _HALIDE_RULES_KEY: halide_rules,
    _HASKELL_RULES_KEY: haskell_rules,
    _APPLE_RULES_KEY: apple_rules,
    _JAVA_RULES_KEY: java_rules,
    _JS_RULES_KEY: js_rules,
    _KOTLIN_RULES_KEY: kotlin_rules,
    _LUA_RULES_KEY: lua_rules,
    _OCAML_RULES_KEY: ocaml_rules,
    _PYTHON_RULES_KEY: python_rules,
    _RUST_RULES_KEY: rust_rules,
    _SCALA_RULES_KEY: scala_rules,
    _SHELL_RULES_KEY: shell_rules,
    _UNCATEGORIZED_RULES_KEY: uncategorized_rules,
}

def _merge_dictionaries(dicts):
    result = {}
    for d in dicts:
        for key, value in d.items():
            if key in result:
                fail("Duplicate key: '{}' while merging dictionaries".format(key))
            result[key] = value

    return result

extra_implemented_rules = struct(
    #common rules
    alias = alias_impl,
    command_alias = command_alias_impl,
    configured_alias = configured_alias_impl,
    export_file = export_file_impl,
    filegroup = filegroup_impl,
    genrule = genrule_impl,
    http_archive = http_archive_impl,
    http_file = http_file_impl,
    remote_file = remote_file_impl,
    sh_binary = sh_binary_impl,
    sh_test = sh_test_impl,
    test_suite = test_suite_impl,
    toolchain_alias = alias_impl,
    versioned_alias = versioned_alias_impl,
    worker_tool = worker_tool,
    generic_simple_value = generic_simple_value_impl,
    generic_file_value = generic_file_value_impl,
    generic_value_mapping = generic_value_mapping_impl,
    generic_value_join_list = generic_value_join_list_impl,

    #c#
    csharp_library = csharp_library_impl,
    prebuilt_dotnet_library = prebuilt_dotnet_library_impl,

    #c++
    cmake_configure_file = cmake_configure_file_impl,
    cmake_type_size_substitution = cmake_type_size_substitution_impl,
    cmake_substitution = cmake_substitution_impl,
    cmake_immediate_substitution = cmake_immediate_substitution_impl,
    cxx_binary = cxx_binary_impl,
    cxx_test = cxx_test_impl,
    cxx_toolchain = cxx_toolchain_impl,
    cxx_genrule = genrule_impl,
    cxx_library = cxx_library_impl,
    cxx_precompiled_header = cxx_precompiled_header_impl,
    cxx_python_extension = cxx_python_extension_impl,
    prebuilt_cxx_library = prebuilt_cxx_library_impl,
    prebuilt_cxx_library_group = prebuilt_cxx_library_group_impl,
    windows_resource = windows_resource_impl,
    transformation_spec = transformation_spec_impl,

    # C++ / LLVM
    llvm_link_bitcode = llvm_link_bitcode_impl,

    #git
    git_fetch = git_fetch_impl,

    #go
    go_binary = go_binary_impl,
    go_bootstrap_binary = go_bootstrap_binary_impl,
    go_exported_library = go_exported_library_impl,
    go_library = go_library_impl,
    go_test = go_test_impl,
    go_stdlib = go_stdlib_impl,

    #haskell
    haskell_library = haskell_library_impl,
    haskell_binary = haskell_binary_impl,
    haskell_ghci = haskell_ghci_impl,
    haskell_haddock = haskell_haddock_impl,
    haskell_ide = haskell_ide_impl,
    haskell_prebuilt_library = haskell_prebuilt_library_impl,

    #lua
    cxx_lua_extension = cxx_lua_extension_impl,
    lua_binary = lua_binary_impl,
    lua_library = lua_library_impl,

    #ocaml
    ocaml_binary = ocaml_binary_impl,
    ocaml_object = ocaml_object_impl,
    ocaml_shared = ocaml_shared_impl,
    ocaml_library = ocaml_library_impl,
    prebuilt_ocaml_library = prebuilt_ocaml_library_impl,

    #python
    prebuilt_python_library = prebuilt_python_library_impl,
    python_binary = python_binary_impl,
    python_library = python_library_impl,
    python_runtime_bundle = python_runtime_bundle_impl,
    python_test = python_test_impl,
    python_needed_coverage_test = python_needed_coverage_test_impl,

    #python bootstrap
    python_bootstrap_binary = python_bootstrap_binary_impl,
    python_bootstrap_library = python_bootstrap_library_impl,

    #merged **kwargs
    **_merge_dictionaries([
        _android_implemented_rules,
        _apple_implemented_rules,
        _config_implemented_rules,
        _erlang_implemented_rules,
        _java_implemented_rules,
        _js_implemented_rules,
        _julia_implemented_rules,
        _kotlin_implemented_rules,
        _matlab_implemented_rules,
        _zip_file_implemented_rules,
    ])
)

def _python_runtime_bundle_attrs():
    return {
        "include": attrs.string(doc = "Header files required for linking python extensions"),
        "install_root": attrs.dep(doc = "The filegroup containing the runtime artifacts, all the paths are relative to this location"),
        "libpython": attrs.option(
            attrs.string(doc = "libpyhon.so required at runtime for the python executable and native extensions."),
            default = None,
        ),
        "py_bin": attrs.string(doc = "The runtime executable"),
        "py_version": attrs.string(doc = "The version of python this represents"),
        "shared_libs": attrs.list(attrs.dep(), default = [], doc = "Additional shared libraries required by this runtime"),
        "stdlib": attrs.string(doc = "The python standard library"),
    }

_dotnet_extra_attributes = {
    "csharp_library": {
        "_csharp_toolchain": toolchains_common.csharp(),
    },
}

_cxx_extra_library_attrs = (
    {
        "auto_link_groups": attrs.bool(default = False),
        # These flags will only be used to instrument a target
        # when coverage for that target is enabled by `exported_needs_coverage_instrumentation`
        # or by any of the target's dependencies.
        "coverage_instrumentation_compiler_flags": attrs.list(attrs.string(), default = []),
        "cuda_compile_style": attrs.enum(CudaCompileStyle.values(), default = "mono"),
        "deps_query": attrs.option(attrs.query(), default = None),
        "exported_needs_coverage_instrumentation": attrs.bool(default = False),
        "extra_dwp_flags": attrs.list(attrs.string(), default = []),
        "header_mode": attrs.option(attrs.enum(HeaderMode.values()), default = None),
        "link_deps_query_whole": attrs.bool(default = False),
        "link_execution_preference": link_execution_preference_attr(),
        "link_group_map": LINK_GROUP_MAP_ATTR,
        "link_ordering": attrs.option(attrs.enum(LinkOrdering.values()), default = None),
        "precompiled_header": attrs.option(attrs.dep(providers = [CPrecompiledHeaderInfo]), default = None),
        "prefer_stripped_objects": attrs.bool(default = False),
        "preferred_linkage": attrs.enum(
            Linkage.values(),
            default = "any",
            doc = """
Determines what linkage is used when the library is depended on by another target. To
control how the dependencies of this library are linked, use `link_style` instead.
""",
        ),
        "resources": attrs.named_set(attrs.one_of(attrs.dep(), attrs.source(allow_directory = True)), sorted = True, default = []),
        "separate_debug_info": attrs.bool(default = False),
        "stub": attrs.bool(default = False),
        "supports_header_symlink_subtarget": attrs.bool(default = False),
        "supports_python_dlopen": attrs.option(attrs.bool(), default = None),
        "supports_shlib_interfaces": attrs.bool(default = True),
        "third_party_project": attrs.option(attrs.string(), default = None),
        "_cxx_hacks": attrs.default_only(attrs.dep(default = "prelude//cxx/tools:cxx_hacks")),
        "_cxx_toolchain": toolchains_common.cxx(),
        "_is_building_android_binary": is_building_android_binary_attr(),
    } |
    apple_common.extra_xcode_sources() |
    third_party_common.create_third_party_build_root_attrs()
)

cxx_extra_attributes = {
    "cxx_genrule": genrule_attributes() | {
        "_cxx_toolchain": toolchains_common.cxx(),
        "_exec_os_type": buck.exec_os_type_arg(),
    },
    "cxx_library": _cxx_extra_library_attrs,
    "cxx_precompiled_header": _cxx_extra_library_attrs,
    "cxx_test": re_test_common.test_args(),
    "cxx_toolchain": cxx_toolchain_extra_attributes(is_toolchain_rule = False),
    "llvm_link_bitcode": {
        "_cxx_toolchain": toolchains_common.cxx(),
    },
    "prebuilt_cxx_library": (
        {
            "exported_header_style": attrs.enum(IncludeType, default = "system"),
            "header_dirs": attrs.option(attrs.list(attrs.source(allow_directory = True)), default = None),
            "linker_flags": attrs.list(attrs.arg(anon_target_compatible = True), default = []),
            "platform_header_dirs": attrs.option(attrs.list(attrs.tuple(attrs.regex(), attrs.list(attrs.source(allow_directory = True)))), default = None),
            "post_linker_flags": attrs.list(attrs.arg(anon_target_compatible = True), default = []),
            "preferred_linkage": attrs.enum(
                Linkage.values(),
                default = "any",
                doc = """
Determines what linkage is used when the library is depended on by another target. To
control how the dependencies of this library are linked, use `link_style` instead.
""",
            ),
            "public_include_directories": attrs.set(attrs.string(), sorted = True, default = []),
            "public_system_include_directories": attrs.set(attrs.string(), sorted = True, default = []),
            "raw_headers": attrs.set(attrs.source(), sorted = True, default = []),
            "stub": attrs.bool(default = False),
            "supports_lto": attrs.bool(default = False),
            "supports_python_dlopen": attrs.bool(default = True),
            "third_party_build": attrs.option(attrs.dep(providers = [ThirdPartyBuildInfo]), default = None),
            "versioned_header_dirs": attrs.option(attrs.versioned(attrs.list(attrs.source(allow_directory = True))), default = None),
            "_cxx_toolchain": toolchains_common.cxx(),
            "_target_os_type": buck.target_os_type_arg(),
        } |
        third_party_common.create_third_party_build_root_attrs()
    ),
    "prebuilt_cxx_library_group": {
        "_create_third_party_build_root": attrs.default_only(attrs.exec_dep(default = "prelude//third-party/tools:create_build")),
        "_cxx_toolchain": toolchains_common.cxx(),
    },
    "transformation_spec": {
        "transformations": attrs.list(
            attrs.tuple(
                attrs.one_of(attrs.dep(), attrs.string()),
                attrs.enum(TransformationKind.values()),
            ),
            default = [],
        ),
    },
    "windows_resource": {
        "_cxx_toolchain": toolchains_common.cxx(),
    },
}

_go_extra_attributes = {
    "go_binary": {
        "embedcfg": attrs.option(attrs.source(allow_directory = False), default = None),
        "resources": attrs.list(attrs.one_of(attrs.dep(), attrs.source(allow_directory = True)), default = []),
        "_build_info": BUILD_INFO_ATTR,
        "_build_tags": build_tags_attr,
        "_cxx_toolchain": toolchains_common.cxx(),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_go_stdlib": attrs.default_only(attrs.dep(default = "prelude//go/tools:stdlib")),
        "_go_toolchain": toolchains_common.go(),
    },
    "go_bootstrap_binary": {
        "_exec_os_type": buck.exec_os_type_arg(),
        "_go_bootstrap_toolchain": toolchains_common.go_bootstrap(),
    },
    "go_exported_library": {
        "embedcfg": attrs.option(attrs.source(allow_directory = False), default = None),
        "_build_info": BUILD_INFO_ATTR,
        "_build_tags": build_tags_attr,
        "_cxx_toolchain": toolchains_common.cxx(),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_go_stdlib": attrs.default_only(attrs.dep(default = "prelude//go/tools:stdlib")),
        "_go_toolchain": toolchains_common.go(),
    },
    "go_library": {
        "embedcfg": attrs.option(attrs.source(allow_directory = False), default = None),
        "_build_tags": build_tags_attr,
        "_cgo_enabled": cgo_enabled_attr,
        "_coverage_mode": coverage_mode_attr,
        "_cxx_toolchain": toolchains_common.cxx(),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_go_stdlib": attrs.default_only(attrs.dep(default = "prelude//go/tools:stdlib")),
        "_go_toolchain": toolchains_common.go(),
    },
    "go_stdlib": {
        "_build_tags": build_tags_attr,
        "_cgo_enabled": cgo_enabled_attr,
        "_cxx_toolchain": toolchains_common.cxx(),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_go_toolchain": toolchains_common.go(),
    },
    "go_test": {
        "coverage_mode": attrs.option(attrs.enum(GoCoverageMode.values()), default = None),
        "embedcfg": attrs.option(attrs.source(allow_directory = False), default = None),
        "resources": attrs.list(attrs.source(allow_directory = True), default = []),
        "_build_info": BUILD_INFO_ATTR,
        "_build_tags": build_tags_attr,
        "_coverage_mode": coverage_mode_attr,
        "_cxx_toolchain": toolchains_common.cxx(),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_go_stdlib": attrs.default_only(attrs.dep(default = "prelude//go/tools:stdlib")),
        "_go_toolchain": toolchains_common.go(),
        "_testmaingen": attrs.default_only(attrs.exec_dep(providers = [RunInfo], default = "prelude//go/tools:testmaingen")),
    },
}

_groovy_extra_attributes = {
    "groovy_library": {
        "resources_root": attrs.option(attrs.string(), default = None),
    },
    "groovy_test": {
        "resources_root": attrs.option(attrs.string(), default = None),
    },
}

_haskell_extra_attributes = {
    "haskell_binary": {
        "auto_link_groups": attrs.bool(default = False),
        "link_group_map": LINK_GROUP_MAP_ATTR,
        "template_deps": attrs.list(attrs.exec_dep(providers = [HaskellLibraryProvider]), default = []),
        "_cxx_toolchain": toolchains_common.cxx(),
        "_haskell_toolchain": toolchains_common.haskell(),
    },
    "haskell_ghci": {
        "template_deps": attrs.list(attrs.exec_dep(providers = [HaskellLibraryProvider]), default = []),
        "_cxx_toolchain": toolchains_common.cxx(),
        "_haskell_toolchain": toolchains_common.haskell(),
    },
    "haskell_haddock": {
        "_cxx_toolchain": toolchains_common.cxx(),
        "_haskell_toolchain": toolchains_common.haskell(),
    },
    "haskell_ide": {
        "include_projects": attrs.list(attrs.dep(), default = []),
        "_haskell_toolchain": toolchains_common.haskell(),
    },
    "haskell_library": {
        "preferred_linkage": attrs.enum(Linkage.values(), default = "any"),
        "template_deps": attrs.list(attrs.exec_dep(providers = [HaskellLibraryProvider]), default = []),
        "_cxx_toolchain": toolchains_common.cxx(),
        "_haskell_toolchain": toolchains_common.haskell(),
    },
}

_python_extra_attributes = {
    #python bootstrap
    "python_bootstrap_binary": {
        "copy_deps": attrs.bool(default = True),
        "deps": attrs.list(attrs.dep(providers = [PythonBootstrapSources]), default = []),
        "has_content_based_path": attrs.bool(default = False),
        "main": attrs.source(),
        "_python_bootstrap_toolchain": toolchains_common.python_bootstrap(),
    },
    "python_bootstrap_library": {
        "deps": attrs.list(attrs.dep(providers = [PythonBootstrapSources]), default = []),
        "has_content_based_path": attrs.bool(default = False),
        "srcs": attrs.list(attrs.source()),
    },
    "python_needed_coverage_test": dict(
        contacts = attrs.list(attrs.string(), default = []),
        env = attrs.dict(key = attrs.string(), value = attrs.arg(), sorted = False, default = {}),
        labels = attrs.list(attrs.string(), default = []),
        needed_coverage = attrs.list(attrs.tuple(attrs.int(), attrs.dep(), attrs.option(attrs.string())), default = []),
        test = attrs.dep(providers = [ExternalRunnerTestInfo]),
        **(re_test_common.test_args() | buck.inject_test_env_arg())
    ),
    "python_runtime_bundle": _python_runtime_bundle_attrs(),
}

_rust_extra_attributes = {
    "rust_test": {},
}

_core_extra_attributes = {
    "export_file": constraint_overrides.attributes,
    "filegroup": constraint_overrides.attributes,
    "genrule": genrule_attributes() | constraint_overrides.attributes,
    "remote_file": {
        "sha1": attrs.option(attrs.string(), default = None),
        "sha256": attrs.option(attrs.string(), default = None),
        "_unzip_tool": attrs.default_only(attrs.exec_dep(providers = [RunInfo], default = "prelude//zip_file/tools:unzip")),
    },
} | _zip_file_extra_attributes | _config_extra_attributes

_shell_extra_attributes = {
    "sh_test": constraint_overrides.attributes,
}

_uncategorized_extra_attributes = {
    "ndk_toolchain": {
        "cxx_toolchain": attrs.toolchain_dep(providers = [CxxToolchainInfo, CxxPlatformInfo]),
    },
}

categorized_extra_attributes = {
    _ANDROID_RULES_KEY: _android_extra_attributes,
    _CORE_RULES_KEY: _core_extra_attributes,
    _CXX_RULES_KEY: cxx_extra_attributes,
    _DOTNET_RULES_KEY: _dotnet_extra_attributes,
    _GO_RULES_KEY: _go_extra_attributes,
    _GROOVY_RULES_KEY: _groovy_extra_attributes,
    _HASKELL_RULES_KEY: _haskell_extra_attributes,
    _APPLE_RULES_KEY: _apple_extra_attributes,
    _JAVA_RULES_KEY: _java_extra_attributes,
    _JS_RULES_KEY: _js_extra_attributes,
    _JULIA_RULES_KEY: _julia_extra_attributes,
    _KOTLIN_RULES_KEY: _kotlin_extra_attributes,
    _MATLAB_RULES_KEY: _matlab_extra_attributes,
    _OCAML_RULES_KEY: _ocaml_extra_attributes,
    _PYTHON_RULES_KEY: _python_extra_attributes,
    _RUST_RULES_KEY: _rust_extra_attributes,
    _SHELL_RULES_KEY: _shell_extra_attributes,
    _UNCATEGORIZED_RULES_KEY: _uncategorized_extra_attributes,
}

toolchain_rule_names = [
    "apple_toolchain",
    "swift_macro_toolchain",
    "swift_toolchain",
    "toolchain_alias",
]