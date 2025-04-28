# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(_ctx):
    return [DefaultInfo()]

simple_rule = rule(
    impl = _impl,
    attrs = {
        "deps": attrs.list(attrs.one_of(attrs.dep(), attrs.configured_dep()), default = []),
        "toolchain_deps": attrs.list(attrs.toolchain_dep(), default = []),
    },
)

simple_toolchain_rule = rule(
    impl = _impl,
    is_toolchain_rule = True,
    attrs = {
        "deps": attrs.list(attrs.toolchain_dep(), default = []),
        "exec_deps": attrs.list(attrs.exec_dep(), default = []),
    },
)
