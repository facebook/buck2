# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
Wrappers for native rules that use a minimal "bootstrap" toolchain instead of the full toolchain.

Use these to break cycles when a target used in a toolchain needs that toolchain to compile. Such targets
provide functionality that is not strictly required in order to build, and thus can be omitted from the
bootstrap toolchain. For example, an ABI generator for Java can be written in Java and compiled with a
bootstrap toolchain that does not include an ABI generator. While using that bootstrap toolchain, buck2
will just use the full JARs as ABI jars, which will be slower and result in some unnecessary recompilation,
but still produce valid results.
"""

load("@prelude//:prelude.bzl", "native")

def java_bootstrap_binary(**kwargs):
    kwargs = _set_bootstrap_java_toolchain(**kwargs)
    native.java_binary(**kwargs)

def java_bootstrap_library(**kwargs):
    kwargs = _set_bootstrap_java_toolchain(**kwargs)
    kwargs = _set_dex_toolchain(**kwargs)
    native.java_library(**kwargs)

def kotlin_bootstrap_library(**kwargs):
    kwargs = _set_bootstrap_java_toolchain(**kwargs)
    kwargs = _set_bootstrap_kotlin_toolchain(**kwargs)
    kwargs = _set_dex_toolchain(**kwargs)
    native.kotlin_library(**kwargs)

def prebuilt_jar_bootstrap(**kwargs):
    kwargs["_prebuilt_jar_toolchain"] = "toolchains//:prebuilt_jar_bootstrap"
    native.prebuilt_jar(**kwargs)

def _set_bootstrap_java_toolchain(**kwargs):
    kwargs["_java_toolchain"] = "toolchains//:java_bootstrap"
    return kwargs

def _set_bootstrap_kotlin_toolchain(**kwargs):
    kwargs["_kotlin_toolchain"] = "toolchains//:kotlin_bootstrap"
    return kwargs

def _set_dex_toolchain(**kwargs):
    # Override dex toolchain to avoid dependency cycles in unconfigured graph
    dex_toolchain = kwargs.pop("_dex_toolchain", None)
    kwargs["_dex_toolchain"] = dex_toolchain or select({
        "DEFAULT": "toolchains//:empty_dex",
        "config//os/constraints:android": "toolchains//:dex",
    })
    return kwargs
