# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# The rules the prelude provides, as `rule` objects. `native.bzl` exposes them as
# `native.<name>`, and they can also be loaded from this module by name.
#
# See `prelude_rule.bzl` for how rules are declared. This file only collects the
# per-language registries and turns the declarations into rules.

load("@prelude//:alias.bzl", "alias", "configured_alias", "toolchain_alias", "versioned_alias")
load("@prelude//:command_alias.bzl", "command_alias_rule")
load("@prelude//:export_file.bzl", "export_file")
load("@prelude//:filegroup.bzl", "filegroup")
load("@prelude//:genrule.bzl", "genrule")
load("@prelude//:http_file.bzl", "http_file")
load("@prelude//:is_full_meta_repo.bzl", "is_full_meta_repo")
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

# Rule declarations grouped by the category they are documented under.
_categorized_rule_specs = {
    ".NET": _dotnet_rules,
    "android": _android_rules,
    "apple": _apple_rules,
    "core": {
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
    "cxx": _cxx_rules,
    "cython": _cython_rules,
    "erlang": _erlang_rules,
    "git": _git_rules,
    "go": _go_rules,
    "haskell": _haskell_rules,
    "java": _java_rules,
    "js": _js_rules,
    "julia": _julia_rules,
    "kotlin": _kotlin_rules,
    "lua": _lua_rules,
    "matlab": _matlab_rules,
    "nodejs": _nodejs_rules,
    "ocaml": _ocaml_rules,
    "python": _python_rules,
    "rust": _rust_rules,
    "shell": {
        "sh_binary": sh_binary,
        "sh_test": sh_test,
    },
    "uncategorized": {
        "ndk_toolchain": ndk_toolchain,
    },
}

def _flatten(categorized: dict[str, dict[str, typing.Any]]) -> dict[str, typing.Any]:
    flat = {}
    for category, by_name in categorized.items():
        for name, value in by_name.items():
            if name in flat:
                fail("Rule `{}` is declared in more than one category (`{}`)".format(name, category))
            flat[name] = value
    return flat

_rule_specs = _flatten(_categorized_rule_specs)

for _name, _spec in _rule_specs.items():
    if _spec.name != _name:
        fail("Rule `{}` is registered under the name `{}`".format(_spec.name, _name))

def _unimplemented(name, ctx):
    fail("Unimplemented rule type `{}` for target `{}`.".format(name, ctx.label))

def _unimplemented_impl(name):
    # We could use a lambda here, but then it means every single parse evaluates a lambda.
    # Lambda's have tricky semantics, so using partial lets us test Starlark prototypes with
    # some features disabled.
    return partial(_unimplemented, name)

def _mk_rule(rule_spec: typing.Any, extra_attrs: dict[str, typing.Any] = dict(), impl_override: [typing.Callable, None] = None, **kwargs):
    name = rule_spec.name
    attributes = rule_spec.attrs

    # We want native code-containing rules to be marked incompatible with fat
    # platforms. Getting the ones that use cxx/apple toolchains is a little
    # overly broad as it includes things like python that don't themselves have
    # native code but need the toolchains if they depend on native code and in
    # that case incompatibility is transitive and they'll get it.
    fat_platform_compatible = True
    if name not in ("python_library", "python_binary", "python_test"):
        for toolchain_attr in ("_apple_toolchain", "_cxx_toolchain", "_go_toolchain"):
            if toolchain_attr in attributes:
                fat_platform_compatible = False

    # Fat platforms is an idea specific to our toolchains, so doesn't apply to
    # open source. Ideally this restriction would be done at the toolchain level.
    if not is_full_meta_repo():
        fat_platform_compatible = True

    attributes = dict(attributes)
    attributes.update(extra_attrs)
    if not fat_platform_compatible:
        # copy so we don't try change the passed in object
        attributes["_cxx_toolchain_target_configuration"] = attrs.dep(default = "prelude//platforms:fat_platform_incompatible")

    cfg = rule_spec.cfg

    if cfg != None:
        # Allow clone_rule callers to set their own cfg, if it exists
        kwargs.setdefault("cfg", cfg)

    if rule_spec.docs:
        doc = rule_spec.docs

        # This is awkward. When generating documentation, we'll strip leading whitespace
        # like it's a python docstring. For that to work here, we need the "Examples:" line
        # to match the other lines for leading whitespace. We've just hardcoded this to
        # be what its expected to be in prelude.
        # TODO(cjhopman): Figure out something better here.
        if rule_spec.examples:
            # NOTE: DocString needs 'Examples:' to render the sections properly, that code should be updated along with this
            doc += "\n{}Examples:\n{}".format(" " * 8, rule_spec.examples)
        if rule_spec.further:
            doc += "\n{}Additional notes:\n{}".format(" " * 8, rule_spec.further)

        kwargs["doc"] = doc

    impl = rule_spec.impl
    if not impl:
        impl = _unimplemented_impl(name)
    if impl_override != None:
        impl = impl_override
    if rule_spec.uses_plugins != None:
        kwargs["uses_plugins"] = rule_spec.uses_plugins
    if rule_spec.supports_incoming_transition != None:
        kwargs["supports_incoming_transition"] = rule_spec.supports_incoming_transition

    kwargs.setdefault("is_configuration_rule", rule_spec.is_configuration_rule)
    kwargs.setdefault("is_toolchain_rule", rule_spec.is_toolchain_rule)
    return rule(impl = impl, attrs = attributes, **kwargs)

categorized_rules = {category: {name: _mk_rule(spec) for name, spec in specs.items()} for category, specs in _categorized_rule_specs.items()}
rules = _flatten(categorized_rules)

# The rules are accessed by doing module.name, so we have to put them on the correct module.
load_symbols(rules)

# TODO(akrieger): Remove this and instead refactor to allow impl bzl files to export attrs.
def clone_rule(rule: str, extra_attrs: dict[str, typing.Any] = dict(), impl_override = None, **kwargs):
    if not rule in _rule_specs:
        fail("Tried clone rule {} which does not exist".format(rule))
    return _mk_rule(_rule_specs[rule], extra_attrs, impl_override, **kwargs)
