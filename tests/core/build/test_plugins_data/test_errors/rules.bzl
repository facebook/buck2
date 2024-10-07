# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _nop(_ctx):
    return [DefaultInfo()]

MyKind = plugins.kind()

plugin_dep_rule = rule(
    impl = _nop,
    attrs = {
        "dep": attrs.plugin_dep(kind = MyKind),
    },
)

multi_dep_rule = rule(
    impl = _nop,
    attrs = {
        "dep": attrs.dep(),
        "plugin_dep": attrs.plugin_dep(kind = MyKind),
    },
)

regular_rule = rule(
    impl = _nop,
    attrs = {},
)

toolchain_rule = rule(
    impl = _nop,
    attrs = {},
    is_toolchain_rule = True,
)

A = plugins.kind()
B = plugins.kind()

def _wrong_plugin_kind_rule_impl(ctx):
    # Need to write it this way to make the linter happy
    ctx.plugins[A]  # buildifier: disable=no-effect
    fail("unreachable")

wrong_plugin_kind_rule = rule(
    impl = _wrong_plugin_kind_rule_impl,
    attrs = {
        "dep": attrs.dep(pulls_and_pushes_plugins = [A]),
    },
    uses_plugins = [B],
)
