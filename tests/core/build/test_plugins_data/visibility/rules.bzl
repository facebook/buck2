# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _nop(_ctx):
    return [DefaultInfo()]

nop = rule(
    impl = _nop,
    attrs = {},
)

MyKind = plugins.kind()

plugin_dep = rule(
    impl = _nop,
    attrs = {
        "actual": attrs.plugin_dep(kind = MyKind),
    },
)

plugin_user = rule(
    impl = _nop,
    attrs = {
        "actual": attrs.dep(pulls_plugins = [MyKind]),
    },
    uses_plugins = [MyKind],
)
