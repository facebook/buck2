# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(_ctx):
    fail("cquery only test")

needs_toolchain = rule(
    impl = _impl,
    attrs = {
        "toolchain": attrs.toolchain_dep(),
    },
)

toolchain = rule(
    impl = _impl,
    attrs = {
        "exec_dep": attrs.exec_dep(),
    },
    is_toolchain_rule = True,
)

optional_dep = rule(
    impl = _impl,
    attrs = {
        "dep": attrs.option(attrs.dep()),
    },
)
