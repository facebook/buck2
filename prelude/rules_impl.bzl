# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:alias.bzl", "alias", "configured_alias", "toolchain_alias", "versioned_alias")
load("@prelude//:command_alias.bzl", "command_alias_rule")
load("@prelude//:export_file.bzl", "export_file")
load("@prelude//:filegroup.bzl", "filegroup")
load("@prelude//:genrule.bzl", "genrule")
load("@prelude//:http_file.bzl", "http_file")
load("@prelude//:remote_file.bzl", "remote_file")
load("@prelude//:sh_binary.bzl", "sh_binary")
load("@prelude//:sh_test.bzl", "sh_test")
load("@prelude//:test_suite.bzl", "test_suite")
load("@prelude//android:ndk_toolchain.bzl", "ndk_toolchain")
load("@prelude//android:rules.bzl", _android_rules = "rules")
load("@prelude//apple:rules.bzl", _apple_rules = "rules")
load("@prelude//configurations:rules.bzl", _configuration_rules = "rules")
load("@prelude//csharp:rules.bzl", _dotnet_rules = "rules")
load("@prelude//cxx:rules.bzl", _cxx_rules = "rules")
load("@prelude//erlang:rules.bzl", _erlang_rules = "rules")
load("@prelude//git:rules.bzl", _git_rules = "rules")
load("@prelude//go:rules.bzl", _go_rules = "rules")
load("@prelude//haskell:rules.bzl", _haskell_rules = "rules")
load("@prelude//http_archive:http_archive.bzl", "http_archive")
load("@prelude//java:rules.bzl", _java_rules = "rules")
load("@prelude//js:rules.bzl", _js_rules = "rules")
load("@prelude//js:worker_tool.bzl", "worker_tool")
load("@prelude//julia:rules.bzl", _julia_rules = "rules")
load("@prelude//kotlin:rules.bzl", _kotlin_rules = "rules")
load("@prelude//lua:rules.bzl", _lua_rules = "rules")
load("@prelude//matlab:rules.bzl", _matlab_rules = "rules")
load("@prelude//nodejs:rules.bzl", _nodejs_rules = "rules")
load("@prelude//ocaml:rules.bzl", _ocaml_rules = "rules")
load("@prelude//python:rules.bzl", _python_rules = "rules")
load("@prelude//python/cython:rules.bzl", _cython_rules = "rules")
load("@prelude//rust:rules.bzl", _rust_rules = "rules")
load("@prelude//zip_file:zip_file.bzl", "zip_file")

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
    _CORE_RULES_KEY: {
        "alias": alias,
        "command_alias": command_alias_rule,
        "configured_alias": configured_alias,
        "export_file": export_file,
        "filegroup": filegroup,
        "genrule": genrule,
        "http_archive": http_archive,
        "http_file": http_file,
        "remote_file": remote_file,
        "test_suite": test_suite,
        "toolchain_alias": toolchain_alias,
        "versioned_alias": versioned_alias,
        "worker_tool": worker_tool,
        "zip_file": zip_file,
    }
    | _configuration_rules,
    _CXX_RULES_KEY: _cxx_rules,
    _CYTHON_RULES_KEY: _cython_rules,
    _DOTNET_RULES_KEY: _dotnet_rules,
    _ERLANG_RULES_KEY: _erlang_rules,
    _GIT_RULES_KEY: _git_rules,
    _GO_RULES_KEY: _go_rules,
    _HASKELL_RULES_KEY: _haskell_rules,
    _APPLE_RULES_KEY: _apple_rules,
    _JAVA_RULES_KEY: _java_rules,
    _JS_RULES_KEY: _js_rules,
    _JULIA_RULES_KEY: _julia_rules,
    _KOTLIN_RULES_KEY: _kotlin_rules,
    _LUA_RULES_KEY: _lua_rules,
    _MATLAB_RULES_KEY: _matlab_rules,
    _NODEJS_RULES_KEY: _nodejs_rules,
    _OCAML_RULES_KEY: _ocaml_rules,
    _PYTHON_RULES_KEY: _python_rules,
    _RUST_RULES_KEY: _rust_rules,
    _SHELL_RULES_KEY: {
        "sh_binary": sh_binary,
        "sh_test": sh_test,
    },
    _UNCATEGORIZED_RULES_KEY: {
        "ndk_toolchain": ndk_toolchain,
    },
}

extra_implemented_rules = struct()

categorized_extra_attributes = {
    _JAVA_RULES_KEY: {},
    _KOTLIN_RULES_KEY: {},
    _NODEJS_RULES_KEY: {},
}
