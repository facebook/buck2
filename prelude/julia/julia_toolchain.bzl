# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":julia_info.bzl", "JuliaToolchainInfo")

def _toolchain(lang: str, providers: list[typing.Any]) -> Attr:
    return attrs.default_only(attrs.toolchain_dep(default = "toolchains//:" + lang, providers = providers))

def julia_toolchain():
    return _toolchain("julia", [JuliaToolchainInfo])
