# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _nop_impl(_ctx):
    return [DefaultInfo()]

Plugin = plugins.kind()

plugin = rule(
    impl = _nop_impl,
    attrs = {},
)

bad_anon_rule = anon_rule(
    impl = _nop_impl,
    attrs = {
        "bad_attr": attrs.plugin_dep(kind = Plugin),
    },
    artifact_promise_mappings = {},
)
