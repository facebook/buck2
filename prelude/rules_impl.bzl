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
load("@prelude//:sh_binary.bzl", "sh_binary")
load("@prelude//:sh_test.bzl", "sh_test")
load("@prelude//:test_suite.bzl", "test_suite_impl")
load("@prelude//android:android.bzl", _android_implemented_rules = "implemented_rules")
load("@prelude//android:configuration.bzl", "is_building_android_binary_attr")
load("@prelude//android:native_build_commands.bzl", "EMIT_NATIVE_BUILD_COMMANDS")
load("@prelude//apple:apple_common.bzl", "apple_common")
load("@prelude//apple:apple_rules_decls.bzl", "apple_rules")
load("@prelude//apple:apple_rules_impl.bzl", _apple_extra_attributes = "extra_attributes", _apple_implemented_rules = "implemented_rules")
load("@prelude//configurations:rules.bzl", _config_extra_attributes = "extra_attributes", _config_implemented_rules = "implemented_rules")
load("@prelude//csharp:rules.bzl", _dotnet_rules = "rules")
load("@prelude//cxx:bitcode.bzl", "llvm_link_bitcode_impl")
load("@prelude//cxx:cuda.bzl", "CudaCompileStyle")
load("@prelude//cxx:cxx.bzl", "cxx_binary_impl", "cxx_library_impl", "cxx_precompiled_header_impl", "cxx_test_impl", "prebuilt_cxx_library_impl")
load("@prelude//cxx:cxx_flags.bzl", "cxx_flags_impl")
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
load("@prelude//decls:cython_rules.bzl", "cython_rules")
load("@prelude//decls:erlang_rules.bzl", "erlang_rules")
load("@prelude//decls:java_rules.bzl", "java_rules")
load("@prelude//decls:kotlin_rules.bzl", "kotlin_rules")
load("@prelude//decls:python_rules.bzl", "python_rules")
load("@prelude//decls:re_test_common.bzl", "re_test_common")
load("@prelude//decls:rust_rules.bzl", "rust_rules")
load("@prelude//decls:third_party_common.bzl", "third_party_common")
load("@prelude//decls:toolchains_common.bzl", "toolchains_common")
load("@prelude//decls:uncategorized_rules.bzl", "uncategorized_rules")
load("@prelude//erlang:erlang.bzl", _erlang_implemented_rules = "implemented_rules")
load("@prelude//git:rules.bzl", _git_rules = "rules")
load("@prelude//go:rules.bzl", _go_rules = "rules")
load("@prelude//haskell:rules.bzl", _haskell_rules = "rules")
load("@prelude//http_archive:http_archive.bzl", "http_archive_impl")
load("@prelude//java:java.bzl", _java_implemented_rules = "implemented_rules")
load("@prelude//js:rules.bzl", _js_rules = "rules")
load("@prelude//js:worker_tool.bzl", "worker_tool")
load("@prelude//julia:julia.bzl", _julia_extra_attributes = "extra_attributes", _julia_implemented_rules = "implemented_rules")
load("@prelude//kotlin:kotlin.bzl", _kotlin_implemented_rules = "implemented_rules")
load("@prelude//linking:execution_preference.bzl", "link_execution_preference_attr")
load("@prelude//linking:link_info.bzl", "LinkOrdering")
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//lua:rules.bzl", _lua_rules = "rules")
load("@prelude//matlab:matlab.bzl", _matlab_extra_attributes = "extra_attributes", _matlab_implemented_rules = "implemented_rules")
load("@prelude//nodejs:rules.bzl", _nodejs_rules = "rules")
load("@prelude//ocaml:rules.bzl", _ocaml_rules = "rules")
load("@prelude//python:cxx_python_extension.bzl", "cxx_python_extension_impl")
load("@prelude//python:prebuilt_python_library.bzl", "prebuilt_python_library_impl")
load("@prelude//python:python_binary.bzl", "python_binary_impl")
load("@prelude//python:python_library.bzl", "python_library_impl")
load("@prelude//python:python_needed_coverage_test.bzl", "python_needed_coverage_test_impl")
load("@prelude//python:python_runtime_bundle.bzl", "python_runtime_bundle_impl")
load("@prelude//python:python_test.bzl", "python_test_impl")
load("@prelude//python/cython:cython_library.bzl", "cython_library_impl")
load("@prelude//python/cython:cython_static_extension.bzl", "cython_static_extension_impl")
load("@prelude//python/cython:cython_toolchain.bzl", "cython_toolchain_impl")
load("@prelude//python_bootstrap:python_bootstrap.bzl", "PythonBootstrapSources", "python_bootstrap_binary_impl", "python_bootstrap_library_impl")
load("@prelude//third-party:providers.bzl", "ThirdPartyBuildInfo")
load("@prelude//transitions:constraint_overrides.bzl", "constraint_overrides")
load("@prelude//zip_file:zip_file.bzl", _zip_file_extra_attributes = "extra_attributes", _zip_file_implemented_rules = "implemented_rules")

_ANDROID_RULES_KEY = "android"
_CORE_RULES_KEY = "core"
_CXX_RULES_KEY = "cxx"
_CYTHON_RULES_KEY = "cython"
_DOTNET_RULES_KEY = ".NET"
_ERLANG_RULES_KEY = "erlang"
_GIT_RULES_KEY = "git"
_GO_RULES_KEY = "go"
_HASKELL_RULES_KEY = "haskell"
_APPLE_RULES_KEY = "apple"
_JAVA_RULES_KEY = "java"
_JS_RULES_KEY = "js"
_KOTLIN_RULES_KEY = "kotlin"
_LUA_RULES_KEY = "lua"
_NODEJS_RULES_KEY = "nodejs"
_OCAML_RULES_KEY = "ocaml"
_PYTHON_RULES_KEY = "python"
_RUST_RULES_KEY = "rust"
_SHELL_RULES_KEY = "shell"
_UNCATEGORIZED_RULES_KEY = "uncategorized"

_JULIA_RULES_KEY = "julia"
_MATLAB_RULES_KEY = "matlab"

categorized_rule_decl_records = {
    _ANDROID_RULES_KEY: android_rules,
    _CORE_RULES_KEY: core_rules,
    _CXX_RULES_KEY: cxx_rules,
    _CYTHON_RULES_KEY: cython_rules,
    _DOTNET_RULES_KEY: _dotnet_rules,
    _ERLANG_RULES_KEY: erlang_rules,
    _GIT_RULES_KEY: _git_rules,
    _GO_RULES_KEY: _go_rules,
    _HASKELL_RULES_KEY: _haskell_rules,
    _APPLE_RULES_KEY: apple_rules,
    _JAVA_RULES_KEY: java_rules,
    _JS_RULES_KEY: _js_rules,
    _KOTLIN_RULES_KEY: kotlin_rules,
    _LUA_RULES_KEY: _lua_rules,
    _NODEJS_RULES_KEY: _nodejs_rules,
    _OCAML_RULES_KEY: _ocaml_rules,
    _PYTHON_RULES_KEY: python_rules,
    _RUST_RULES_KEY: rust_rules,
    _SHELL_RULES_KEY: {
        "sh_binary": sh_binary,
        "sh_test": sh_test,
    },
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
    # common rules
    alias = alias_impl,
    command_alias = command_alias_impl,
    configured_alias = configured_alias_impl,
    export_file = export_file_impl,
    filegroup = filegroup_impl,
    genrule = genrule_impl,
    http_archive = http_archive_impl,
    http_file = http_file_impl,
    remote_file = remote_file_impl,
    test_suite = test_suite_impl,
    toolchain_alias = alias_impl,
    versioned_alias = versioned_alias_impl,
    worker_tool = worker_tool,
    # c++
    cxx_binary = cxx_binary_impl,
    cxx_flags = cxx_flags_impl,
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
    # cython
    cython_library = cython_library_impl,
    cython_static_extension = cython_static_extension_impl,
    cython_toolchain = cython_toolchain_impl,
    # C++ / LLVM
    llvm_link_bitcode = llvm_link_bitcode_impl,
    # python
    prebuilt_python_library = prebuilt_python_library_impl,
    python_binary = python_binary_impl,
    python_library = python_library_impl,
    python_runtime_bundle = python_runtime_bundle_impl,
    python_test = python_test_impl,
    python_needed_coverage_test = python_needed_coverage_test_impl,
    # python bootstrap
    python_bootstrap_binary = python_bootstrap_binary_impl,
    python_bootstrap_library = python_bootstrap_library_impl,
    # merged **kwargs
    **_merge_dictionaries([
        _android_implemented_rules,
        _apple_implemented_rules,
        _config_implemented_rules,
        _erlang_implemented_rules,
        _java_implemented_rules,
        _julia_implemented_rules,
        _kotlin_implemented_rules,
        _matlab_implemented_rules,
        _zip_file_implemented_rules,
    ]),
)

def _python_runtime_bundle_attrs():
    return (
        {
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
        | buck.labels_arg()
        | buck.contacts_arg()
    )

_cxx_extra_library_attrs = (
    {
        "auto_link_groups": attrs.bool(default = False),
        # These flags will only be used to instrument a target
        # when coverage for that target is enabled by `exported_needs_coverage_instrumentation`
        # or by any of the target's dependencies.
        "coverage_instrumentation_compiler_flags": attrs.list(attrs.string(), default = []),
        # Optional clang_profile_list target for selective coverage instrumentation via -fprofile-list.
        "coverage_profile_list": attrs.option(attrs.dep(), default = None),
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
        "_emit_native_build_commands": attrs.default_only(attrs.bool(default = EMIT_NATIVE_BUILD_COMMANDS)),
        "_is_building_android_binary": is_building_android_binary_attr(),
    }
    | apple_common.extra_xcode_sources()
    | third_party_common.create_third_party_build_root_attrs()
)

cxx_extra_attributes = {
    "cxx_binary": {
        "_gen_build_info": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "_generated_build_info_data": attrs.option(attrs.source(), default = None),
        "_generated_build_info_enabled": attrs.bool(default = False),
        "_generated_build_info_mode": attrs.enum(["full", "stable"], default = "stable"),
        "_generated_build_info_spec": BUILD_INFO_ATTR,
    },
    "cxx_genrule": genrule_attributes()
    | {
        "_cxx_toolchain": toolchains_common.cxx(),
        "_exec_os_type": buck.exec_os_type_arg(),
    },
    "cxx_library": _cxx_extra_library_attrs,
    "cxx_precompiled_header": _cxx_extra_library_attrs,
    "cxx_test": re_test_common.test_args()
    | {
        "_gen_build_info": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "_generated_build_info_data": attrs.option(attrs.source(), default = None),
        "_generated_build_info_enabled": attrs.bool(default = False),
        "_generated_build_info_mode": attrs.enum(["full", "stable"], default = "stable"),
        "_generated_build_info_spec": BUILD_INFO_ATTR,
    },
    "cxx_toolchain": cxx_toolchain_extra_attributes(is_toolchain_rule = False),
    "llvm_link_bitcode": {
        "_cxx_toolchain": toolchains_common.cxx(),
    },
    "prebuilt_cxx_library": (
        {
            "exported_header_style": attrs.enum(IncludeType, default = "system"),
            "header_dirs": attrs.option(attrs.list(attrs.source(allow_directory = True)), default = None),
            "linker_flags": attrs.list(attrs.arg(anon_target_compatible = True), default = []),
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
        }
        | third_party_common.create_third_party_build_root_attrs()
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

_python_extra_attributes = {
    # python bootstrap
    "python_bootstrap_binary": {
        "copy_deps": attrs.bool(default = True),
        "deps": attrs.list(attrs.dep(providers = [PythonBootstrapSources]), default = []),
        "has_content_based_path": attrs.bool(default = False),
        "main": attrs.source(),
        "_python_bootstrap_toolchain": toolchains_common.python_bootstrap(),
    }
    | buck.labels_arg()
    | buck.contacts_arg(),
    "python_bootstrap_library": {
        "deps": attrs.list(attrs.dep(providers = [PythonBootstrapSources]), default = []),
        "has_content_based_path": attrs.bool(default = False),
        "srcs": attrs.list(attrs.source()),
    }
    | buck.labels_arg()
    | buck.contacts_arg(),
    "python_needed_coverage_test": dict(
        contacts = attrs.list(attrs.string(), default = []),
        env = attrs.dict(key = attrs.string(), value = attrs.arg(), sorted = False, default = {}),
        labels = attrs.list(attrs.string(), default = []),
        needed_coverage = attrs.list(attrs.tuple(attrs.int(), attrs.dep(), attrs.option(attrs.string())), default = []),
        supports_test_execution_caching = attrs.bool(default = False),
        test = attrs.dep(providers = [ExternalRunnerTestInfo]),
        **(re_test_common.test_args() | buck.inject_test_env_arg()),
    ),
    "python_runtime_bundle": _python_runtime_bundle_attrs(),
}

_rust_extra_attributes = {
    "rust_binary": {
        "_gen_build_info": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "_generated_build_info_args": attrs.list(attrs.arg(), default = []),
        "_generated_build_info_data": attrs.option(attrs.source(), default = None),
        "_generated_build_info_enabled": attrs.bool(default = False),
        "_generated_build_info_mode": attrs.enum(["full", "stable"], default = "stable"),
        "_generated_build_info_spec": BUILD_INFO_ATTR,
    },
    "rust_test": {
        "_gen_build_info": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "_generated_build_info_args": attrs.list(attrs.arg(), default = []),
        "_generated_build_info_data": attrs.option(attrs.source(), default = None),
        "_generated_build_info_enabled": attrs.bool(default = False),
        "_generated_build_info_mode": attrs.enum(["full", "stable"], default = "stable"),
        "_generated_build_info_spec": BUILD_INFO_ATTR,
    },
}

_core_extra_attributes = (
    {
        "export_file": constraint_overrides.attributes,
        "filegroup": constraint_overrides.attributes,
        "genrule": genrule_attributes() | constraint_overrides.attributes,
        "remote_file": {
            "sha1": attrs.option(attrs.string(), default = None),
            "sha256": attrs.option(attrs.string(), default = None),
            "_unzip_tool": attrs.default_only(attrs.exec_dep(providers = [RunInfo], default = "prelude//zip_file/tools:unzip")),
        },
    }
    | _zip_file_extra_attributes
    | _config_extra_attributes
)

_uncategorized_extra_attributes = {
    "ndk_toolchain": {
        "cxx_toolchain": attrs.toolchain_dep(providers = [CxxToolchainInfo, CxxPlatformInfo]),
    },
}

_cython_extra_attributes = {
    "cython_library": _cxx_extra_library_attrs,
    "cython_static_extension": _cxx_extra_library_attrs,
}

categorized_extra_attributes = {
    _ANDROID_RULES_KEY: {},
    _CORE_RULES_KEY: _core_extra_attributes,
    _CXX_RULES_KEY: cxx_extra_attributes,
    _CYTHON_RULES_KEY: _cython_extra_attributes,
    _APPLE_RULES_KEY: _apple_extra_attributes,
    _JAVA_RULES_KEY: {},
    _JULIA_RULES_KEY: _julia_extra_attributes,
    _KOTLIN_RULES_KEY: {},
    _MATLAB_RULES_KEY: _matlab_extra_attributes,
    _NODEJS_RULES_KEY: {},
    _PYTHON_RULES_KEY: _python_extra_attributes,
    _RUST_RULES_KEY: _rust_extra_attributes,
    _UNCATEGORIZED_RULES_KEY: _uncategorized_extra_attributes,
}

toolchain_rule_names = [
    "apple_toolchain",
    "swift_macro_toolchain",
    "swift_toolchain",
    "toolchain_alias",
]
