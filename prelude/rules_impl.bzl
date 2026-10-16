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
load("@prelude//android:rules.bzl", _android_rules = "rules")
load("@prelude//apple:apple_rules_decls.bzl", "apple_rules")
load("@prelude//apple:apple_rules_impl.bzl", _apple_extra_attributes = "extra_attributes", _apple_implemented_rules = "implemented_rules")
load("@prelude//configurations:rules.bzl", _config_extra_attributes = "extra_attributes", _config_implemented_rules = "implemented_rules")
load("@prelude//csharp:rules.bzl", _dotnet_rules = "rules")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo", "CxxToolchainInfo")
load("@prelude//cxx:rules.bzl", _cxx_rules = "rules")
load("@prelude//decls:core_rules.bzl", "core_rules")
load("@prelude//decls:uncategorized_rules.bzl", "uncategorized_rules")
load("@prelude//erlang:rules.bzl", _erlang_rules = "rules")
load("@prelude//git:rules.bzl", _git_rules = "rules")
load("@prelude//go:rules.bzl", _go_rules = "rules")
load("@prelude//haskell:rules.bzl", _haskell_rules = "rules")
load("@prelude//http_archive:http_archive.bzl", "http_archive_impl")
load("@prelude//java:rules.bzl", _java_rules = "rules")
load("@prelude//js:rules.bzl", _js_rules = "rules")
load("@prelude//js:worker_tool.bzl", "worker_tool")
load("@prelude//julia:julia.bzl", _julia_extra_attributes = "extra_attributes", _julia_implemented_rules = "implemented_rules")
load("@prelude//kotlin:rules.bzl", _kotlin_rules = "rules")
load("@prelude//lua:rules.bzl", _lua_rules = "rules")
load("@prelude//matlab:matlab.bzl", _matlab_extra_attributes = "extra_attributes", _matlab_implemented_rules = "implemented_rules")
load("@prelude//nodejs:rules.bzl", _nodejs_rules = "rules")
load("@prelude//ocaml:rules.bzl", _ocaml_rules = "rules")
load("@prelude//python:rules.bzl", _python_rules = "rules")
load("@prelude//python/cython:rules.bzl", _cython_rules = "rules")
load("@prelude//rust:rules.bzl", _rust_rules = "rules")
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
    _ANDROID_RULES_KEY: _android_rules,
    _CORE_RULES_KEY: core_rules,
    _CXX_RULES_KEY: _cxx_rules,
    _CYTHON_RULES_KEY: _cython_rules,
    _DOTNET_RULES_KEY: _dotnet_rules,
    _ERLANG_RULES_KEY: _erlang_rules,
    _GIT_RULES_KEY: _git_rules,
    _GO_RULES_KEY: _go_rules,
    _HASKELL_RULES_KEY: _haskell_rules,
    _APPLE_RULES_KEY: apple_rules,
    _JAVA_RULES_KEY: _java_rules,
    _JS_RULES_KEY: _js_rules,
    _KOTLIN_RULES_KEY: _kotlin_rules,
    _LUA_RULES_KEY: _lua_rules,
    _NODEJS_RULES_KEY: _nodejs_rules,
    _OCAML_RULES_KEY: _ocaml_rules,
    _PYTHON_RULES_KEY: _python_rules,
    _RUST_RULES_KEY: _rust_rules,
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
    # merged **kwargs
    **_merge_dictionaries([
        _apple_implemented_rules,
        _config_implemented_rules,
        _julia_implemented_rules,
        _matlab_implemented_rules,
        _zip_file_implemented_rules,
    ]),
)

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

categorized_extra_attributes = {
    _CORE_RULES_KEY: _core_extra_attributes,
    _APPLE_RULES_KEY: _apple_extra_attributes,
    _JAVA_RULES_KEY: {},
    _JULIA_RULES_KEY: _julia_extra_attributes,
    _KOTLIN_RULES_KEY: {},
    _MATLAB_RULES_KEY: _matlab_extra_attributes,
    _NODEJS_RULES_KEY: {},
    _UNCATEGORIZED_RULES_KEY: _uncategorized_extra_attributes,
}

toolchain_rule_names = [
    "apple_toolchain",
    "swift_macro_toolchain",
    "swift_toolchain",
    "toolchain_alias",
]
