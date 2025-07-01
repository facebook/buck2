# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:genrule_toolchain.bzl", "GenruleToolchainInfo")

def _system_genrule_toolchain_impl(_ctx):
    return [
        DefaultInfo(),
        GenruleToolchainInfo(
            zip_scrubber = None,
        ),
    ]

system_genrule_toolchain = rule(
    impl = _system_genrule_toolchain_impl,
    attrs = {},
    is_toolchain_rule = True,
)
