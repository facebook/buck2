# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":erlang_application.bzl", "StartTypeValues", "erlang_application_impl")
load(":erlang_application_includes.bzl", "erlang_application_includes_impl")
load(":erlang_escript.bzl", "erlang_escript_impl")
load(":erlang_otp_application.bzl", "normalize_application")
load(":erlang_rebar3_interface.bzl", "erlang_rebar3_interface_impl")
load(":erlang_release.bzl", "erlang_release_impl")
load(":erlang_tests.bzl", "erlang_test_impl", "erlang_tests_macro")
load(":erlang_toolchain.bzl", "erlang_otp_binaries_impl")

# all rules have an optional contacts attribute
common_attributes = {
    "contacts": attrs.list(attrs.string(), default = []),
    "labels": attrs.list(attrs.string(), default = []),
}

common_shell_attributes = {
    "shell_configs": attrs.set(attrs.dep(), default = read_config("erlang", "shell_configs", "").split()),
    "shell_libs": attrs.set(attrs.dep(), default = ["prelude//erlang/shell:buck2_shell_utils"]),
}

common_application_attributes = dict({
    "applications": attrs.list(attrs.dep(), default = []),
    "included_applications": attrs.list(attrs.dep(), default = []),
    "version": attrs.string(default = "1.0.0"),
    "_toolchain": attrs.toolchain_dep(default = "toolchains//:erlang-default"),
}, **common_shell_attributes)

# target attributes for public erlang targets
rules_attributes = {
    "erlang_app": dict({
        "app_src": attrs.option(attrs.source(), default = None),
        "build_edoc_chunks": attrs.bool(default = True),
        "erl_opts": attrs.option(attrs.list(attrs.string()), default = None),
        "extra_includes": attrs.list(attrs.dep(), default = []),
        "includes": attrs.list(attrs.source(), default = []),
        "mod": attrs.option(attrs.tuple(attrs.string(), attrs.list(attrs.string())), default = None),
        "resources": attrs.list(attrs.dep(), default = []),
        "srcs": attrs.list(attrs.source(), default = []),
        "use_global_parse_transforms": attrs.bool(default = True),
    }, **common_application_attributes),
    "erlang_app_includes": {
        "application_name": attrs.string(),
        "includes": attrs.list(attrs.source(), default = []),
        "_toolchain": attrs.toolchain_dep(default = "toolchains//:erlang-default"),
    },
    "erlang_escript": {
        "deps": attrs.list(attrs.dep()),
        "emu_args": attrs.list(attrs.string(), default = []),
        "include_priv": attrs.bool(default = False),
        "main_module": attrs.option(attrs.string(), default = None),
        "resources": attrs.list(attrs.dep(), default = []),
        "script_name": attrs.option(attrs.string(), default = None),
        "_toolchain": attrs.toolchain_dep(default = "toolchains//:erlang-default"),
    },
    "erlang_otp_binaries": {
        "erl": attrs.source(),
        "erlc": attrs.source(),
        "escript": attrs.source(),
    },
    "erlang_rebar3_interface": {
        "deps": attrs.list(attrs.dep(), default = []),
    },
    "erlang_release": {
        "applications": attrs.list(attrs.one_of(attrs.dep(), attrs.tuple(attrs.dep(), attrs.enum(StartTypeValues)))),
        "include_erts": attrs.bool(default = False),
        "multi_toolchain": attrs.option(attrs.list(attrs.dep()), default = None),
        "overlays": attrs.dict(key = attrs.string(), value = attrs.list(attrs.dep()), default = {}),
        "release_name": attrs.option(attrs.string(), default = None),
        "version": attrs.string(default = "1.0.0"),
        "_toolchain": attrs.toolchain_dep(default = "toolchains//:erlang-default"),
    },
    "erlang_test": dict({
        "config_files": attrs.list(attrs.dep(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "env": attrs.dict(key = attrs.string(), value = attrs.string(), default = {}),
        "extra_ct_hooks": attrs.list(attrs.string(), default = []),
        "property_tests": attrs.list(attrs.dep(), default = []),
        "resources": attrs.list(attrs.dep(), default = []),
        "suite": attrs.source(),
        "_cli_lib": attrs.dep(default = "prelude//erlang/common_test/test_cli_lib:test_cli_lib"),
        "_ct_opts": attrs.string(default = read_config("erlang", "erlang_test_ct_opts", "")),
        "_providers": attrs.string(),
        "_test_binary": attrs.dep(default = "prelude//erlang/common_test/test_binary:escript"),
        "_test_binary_lib": attrs.dep(default = "prelude//erlang/common_test/test_binary:test_binary"),
        "_toolchain": attrs.toolchain_dep(default = "toolchains//:erlang-default"),
        "_trampoline": attrs.option(attrs.dep(), default = None),
    }, **common_shell_attributes),
}

# target rules

implemented_rules = {
    "erlang_app": erlang_application_impl,
    "erlang_app_includes": erlang_application_includes_impl,
    "erlang_escript": erlang_escript_impl,
    "erlang_otp_binaries": erlang_otp_binaries_impl,
    "erlang_rebar3_interface": erlang_rebar3_interface_impl,
    "erlang_release": erlang_release_impl,
    "erlang_test": erlang_test_impl,
}

attributes = {
    name: dict(rules_attributes[name], **common_attributes)
    for name in rules_attributes
}

# Macros

# Wrapper to generate the erlang_app and erlang_app_include target from a single
# specification. It also redirects the target from the regular appliction target
# to the include-only target for extra_include deps
def erlang_application(
        erlang_app_rule,
        erlang_app_includes_rule,
        name,
        applications = [],
        included_applications = [],
        extra_includes = [],
        labels = [],
        **kwargs):
    if read_config("erlang", "application_only_dependencies"):
        kwargs["shell_libs"] = []
        kwargs["resources"] = []

    normalized_applications = [
        normalize_application(app)
        for app in applications
    ]

    normalized_included_applications = [
        normalize_application(app)
        for app in included_applications
    ]

    return [
        erlang_app_rule(
            name = name,
            applications = normalized_applications,
            included_applications = normalized_included_applications,
            extra_includes = [
                _extra_include_name(dep)
                for dep in extra_includes
            ],
            labels = labels,
            **kwargs
        ),
        erlang_app_includes_rule(
            name = _extra_include_name(name),
            application_name = name,
            includes = kwargs.get("includes", []),
            visibility = kwargs.get("visibility", []),
            labels = ["generated", "app_includes"],
        ),
    ]

# convenience macro to specify the includes-only target based on the base-application
# target name
def _extra_include_name(name: "string") -> "string":
    return name + "_includes_only"

def erlang_tests(
        erlang_app_rule,
        erlang_test_rule,
        suites: ["string"] = [],
        deps: ["string"] = [],
        resources: ["string"] = [],
        srcs: ["string"] = [],
        property_tests: ["string"] = [],
        config_files: ["string"] = [],
        use_default_configs: "bool" = True,
        use_default_deps: "bool" = True,
        **common_attributes):
    """
    Generate multiple erlang_test targets based on the `suites` field.
    """
    erlang_tests_macro(
        erlang_app_rule = erlang_app_rule,
        erlang_test_rule = erlang_test_rule,
        suites = suites,
        deps = deps,
        resources = resources,
        srcs = srcs,
        property_tests = property_tests,
        config_files = config_files,
        use_default_configs = use_default_configs,
        use_default_deps = use_default_deps,
        **common_attributes
    )
