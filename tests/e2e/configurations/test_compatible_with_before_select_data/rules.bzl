# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(_ctx):
    fail("cquery only test")

two_dep_rule = rule(
    impl = _impl,
    attrs = {
        "a": attrs.dep(),
        "b": attrs.dep(),
    },
)

two_exec_dep_rule = rule(
    impl = _impl,
    attrs = {
        "a": attrs.exec_dep(),
        "b": attrs.exec_dep(),
    },
)
