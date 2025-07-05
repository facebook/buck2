# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# This file exports utilities for use with with reindeer.
# These are not used anywhere else in prelude and are not exported as prelude globals.

load("@prelude//:prelude.bzl", "native")
load("@prelude//utils:selects.bzl", "selects")
load("@prelude//utils:type_defs.bzl", "is_dict", "is_list")

DEFAULT_PLATFORM_TEMPLATES = select({
    "DEFAULT": None,
    "config//os:linux": select({
        "DEFAULT": None,
        "config//cpu:arm64": "linux-arm64",
        "config//cpu:x86_64": "linux-x86_64",
        "config//cpu:riscv64": "linux-riscv64",
    }),
    "config//os:macos": select({
        "DEFAULT": None,
        "config//cpu:arm64": "macos-arm64",
        "config//cpu:x86_64": "macos-x86_64",
    }),
    "config//os:none": select({
        "DEFAULT": None,
        "config//cpu:wasm32": "wasm32",
    }),
    "config//os:wasi": select({
        "DEFAULT": None,
        "config//cpu:wasm32": "wasi",
    }),
    "config//os:windows": select({
        "DEFAULT": "windows-msvc",
        "config//abi:gnu": "windows-gnu",
        "config//abi:msvc": "windows-msvc",
    }),
})

def apply_platform_attrs(
        platform_attrs,
        universal_attrs = {},
        templates = DEFAULT_PLATFORM_TEMPLATES):
    combined_attrs = dict(universal_attrs)

    if is_dict(templates):
        # Deprecated format: {
        #     "linux-arm64": select({
        #         "DEFAULT": False,
        #         "config//os:linux": select({
        #             "DEFAULT": False,
        #             "config//cpu:arm64": True,
        #         }),
        #     }),
        #     ...
        # }
        for platform, attrs in platform_attrs.items():
            template = templates.get(platform, None)
            if template:
                for attr, value in attrs.items():
                    default_value = {} if type(value) == type({}) else [] if type(value) == type([]) else None
                    conditional_value = selects.apply(template, lambda cond: value if cond else default_value)
                    if attr in combined_attrs:
                        combined_attrs[attr] = combined_attrs[attr] + conditional_value
                    else:
                        combined_attrs[attr] = conditional_value
    else:
        # Preferred format: select({
        #     "config//os:linux": select({
        #         "config//cpu:arm64": "linux-arm64",
        #         ...
        #     }),
        #     ...
        # })
        platform_attr_defaults = {}
        for attrs in platform_attrs.values():
            for attr, value in attrs.items():
                if native.select_test(value, is_list):
                    platform_attr_defaults[attr] = []
                elif native.select_test(value, is_dict):
                    platform_attr_defaults[attr] = {}
                else:
                    platform_attr_defaults[attr] = None
        for attr, default_value in platform_attr_defaults.items():
            conditional_value = selects.apply(
                templates,
                lambda platform: platform_attrs.get(platform, {}).get(attr, default_value),
            )
            if attr in combined_attrs:
                combined_attrs[attr] = combined_attrs[attr] + conditional_value
            else:
                combined_attrs[attr] = conditional_value

    return combined_attrs

def _cargo_rust_binary(name, platform = {}, **kwargs):
    kwargs = apply_platform_attrs(platform, kwargs)

    rustc_flags = kwargs.get("rustc_flags", [])
    kwargs["rustc_flags"] = ["--cap-lints=allow"] + rustc_flags

    native.rust_binary(name = name, **kwargs)

def _cargo_rust_library(name, platform = {}, **kwargs):
    kwargs = apply_platform_attrs(platform, kwargs)

    rustc_flags = kwargs.get("rustc_flags", [])
    kwargs["rustc_flags"] = ["--cap-lints=allow"] + rustc_flags

    kwargs.setdefault("doctests", False)

    # Support for reindeer's `python_ext` fixup is not implemented yet.
    kwargs.pop("dlopen_enable", None)
    kwargs.pop("linkable_alias", None)

    native.rust_library(name = name, **kwargs)

cargo = struct(
    rust_binary = _cargo_rust_binary,
    rust_library = _cargo_rust_library,
)
