# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//rust:cargo_package.bzl", "apply_platform_attrs", "cargo", "get_reindeer_platforms")
load("@prelude//utils:selects.bzl", "selects")

def _third_party_rust_prebuilt_cxx_library(name, **kwargs):
    # FIXME: This should probably be a fixup.toml, but it currently can't be expressed.
    # The windows-sys crate does -lwindows to find windows. We pass libwindows.a on the command line,
    # which resolves the symbols, but the linker still needs to "find" windows, so we also put its
    # directory on the link options.
    if name.endswith("libwindows.a"):
        kwargs["exported_linker_flags"] = ["-Lshim/third-party/rust/" + kwargs["static_lib"].rpartition("/")[0]]

    # @lint-ignore BUCKLINT
    native.prebuilt_cxx_library(
        name = name,
        **kwargs
    )

def third_party_rust_alias(name, **kwargs):
    # slightly silly thing reindeer makes us do by giving us a list
    platforms = kwargs.pop("platforms", {})
    platform = {k: {} for k in platforms}

    # @lint-ignore BUCKLINT: avoid "Direct usage of native rules is not allowed."
    _target_restricted(native.alias, name = name, platform = platform, **kwargs)

def _target_restricted(f, name, target_compatible_with = None, **kwargs):
    platform = kwargs.pop("platform", {})
    kwargs = apply_platform_attrs(platform, kwargs)

    target_compatible_with = target_compatible_with or _target_constraints(platform, kwargs)
    return f(
        name = name,
        target_compatible_with = target_compatible_with,
        **kwargs
    )

def _target_constraints(platforms, kwargs):
    target_compatible_with = kwargs.pop("target_compatible_with", [])

    if kwargs.get("proc_macro", False) or platforms == {}:
        return target_compatible_with
    else:
        return selects.apply(
            get_reindeer_platforms(),
            lambda p: target_compatible_with if p in platforms else ["prelude//:none"],
        )

third_party_rust_prebuilt_cxx_library = partial(_target_restricted, _third_party_rust_prebuilt_cxx_library)

# @lint-ignore BUCKLINT: avoid "Direct usage of native rules is not allowed."
third_party_rust_cxx_library = partial(_target_restricted, native.cxx_library)

# @lint-ignore BUCKLINT: avoid "Direct usage of native rules is not allowed."
third_party_rust_rust_library = partial(_target_restricted, cargo.rust_library)

# @lint-ignore BUCKLINT: avoid "Direct usage of native rules is not allowed."
third_party_rust_rust_binary = partial(_target_restricted, cargo.rust_binary)
