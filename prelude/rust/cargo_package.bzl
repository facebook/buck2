# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# This file exports utilities for use with with reindeer.
# These are not used anywhere else in prelude and are not exported as prelude globals.

load("@prelude//:prelude.bzl", "native")
load("@prelude//utils:selects.bzl", "selects")

DEFAULT_PLATFORM_TEMPLATES = {
    "linux-arm64": select({
        "DEFAULT": False,
        "config//os:linux": select({
            "DEFAULT": False,
            "config//cpu:arm64": True,
        }),
    }),
    "linux-x86_64": select({
        "DEFAULT": False,
        "config//os:linux": select({
            "DEFAULT": False,
            "config//cpu:x86_64": True,
        }),
    }),
    "macos-arm64": select({
        "DEFAULT": False,
        "config//os:macos": select({
            "DEFAULT": False,
            "config//cpu:arm64": True,
        }),
    }),
    "macos-x86_64": select({
        "DEFAULT": False,
        "config//os:macos": select({
            "DEFAULT": False,
            "config//cpu:x86_64": True,
        }),
    }),
    "wasi": select({
        "DEFAULT": False,
        "config//os:wasi": select({
            "DEFAULT": False,
            "config//cpu:wasm32": True,
        }),
    }),
    "wasm32": select({
        "DEFAULT": False,
        "config//os:none": select({
            "DEFAULT": False,
            "config//cpu:wasm32": True,
        }),
    }),
    "windows-gnu": select({
        "DEFAULT": False,
        "config//os:windows": select({
            "DEFAULT": False,
            "config//abi:gnu": True,
        }),
    }),
    "windows-msvc": select({
        "DEFAULT": False,
        "config//os:windows": select({
            "DEFAULT": True,
            "config//abi:gnu": False,
            "config//abi:msvc": True,
        }),
    }),
}

def apply_platform_attrs(
        platform_attrs,
        universal_attrs = {},
        templates = DEFAULT_PLATFORM_TEMPLATES):
    combined_attrs = dict(universal_attrs)

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

    return combined_attrs

def fix_missing_filegroup_for_buildscript_run(filegroup_name, manifest_dir, srcs):
    if len(srcs) == 1 and srcs[0].startswith(":"):
        # In cases such as `srcs = [":thiserror-a2b02c8016a45f62.git"]`, this creates
        # an alias like "thiserror-2.0.12.crate".
        native.alias(
            name = filegroup_name,
            actual = srcs[0],
            visibility = [],
        )
    elif not any([path.startswith(":") for path in srcs]):
        # Otherwise `srcs` is a simple list of local source files, this creates a filegroup
        # with source mapping like `srcs = { "src/lib.rs": "path/to/manifest/dir/src/lib.rs", ... }`,
        # removing the "path/to/manifest/dir/" prefix.
        manifest_dir_with_trailing_slash = "{}/".format(manifest_dir)
        native.filegroup(
            name = filegroup_name,
            srcs = {
                path.removeprefix(manifest_dir_with_trailing_slash): path
                for path in srcs
            },
            copy = False,
            visibility = [],
        )
    # The last case of filegroup missing is when using Reindeer in `vendor = true` mode,
    # which is handled in @prelude//rust:cargo_buildscript.bzl by the `buildscript_run` macro.

def _cargo_rust_binary(name, platform = {}, **kwargs):
    kwargs = apply_platform_attrs(platform, kwargs)

    rustc_flags = kwargs.get("rustc_flags", [])
    kwargs["rustc_flags"] = ["--cap-lints=allow"] + rustc_flags

    env = kwargs.get("env", {})
    manifest_dir = env.get("CARGO_MANIFEST_DIR", None)
    package_name = env.get("CARGO_PKG_NAME", None)
    version = env.get("CARGO_PKG_VERSION", None)

    is_buildscript_build = kwargs.get("crate", None) == "build_script_build" and name.endswith("-build-script-build")
    has_required_envs = manifest_dir != None and package_name != None and version != None

    if is_buildscript_build and has_required_envs:
        filegroup_name = "{}-{}.crate".format(package_name, version)
        if not rule_exists(filegroup_name):
            fix_missing_filegroup_for_buildscript_run(filegroup_name, manifest_dir, srcs = kwargs["srcs"])

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
