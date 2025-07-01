# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

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
