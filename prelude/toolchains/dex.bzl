# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//java:dex_toolchain.bzl", "DexToolchainInfo")

def system_noop_dex_toolchain(
        name,
        **kwargs):
    _dex_toolchain_rule(
        name = name,
        **kwargs
    )

def _dex_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        DexToolchainInfo(
            android_jar = ctx.attrs.android_jar,
            d8_command = ctx.attrs.d8_command_binary,
        ),
    ]

_dex_toolchain_rule = rule(
    attrs = {
        "android_jar": attrs.option(attrs.source(), default = None),
        "d8_command_binary": attrs.option(attrs.dep(providers = [RunInfo]), default = None),
    },
    impl = _dex_toolchain_rule_impl,
    is_toolchain_rule = True,
)
