# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _transition_impl(platform, refs):
    _platform = platform  # @unused
    _refs = refs  # @unused
    pass

_t = transition(
    impl = _transition_impl,
    refs = {
        "someref": "root//:cat",
    },
)

def _impl(_ctx):
    pass

outgoing_transition_alias = rule(
    impl = _impl,
    attrs = {
        "actual": attrs.transition_dep(cfg = _t),
    },
)

incoming_transition_alias = rule(
    impl = _impl,
    attrs = {},
    cfg = _t,
)

stub_configuration = rule(
    impl = _impl,
    attrs = {},
    is_configuration_rule = True,
)

outgoing_transition_alias_vnew = rule(
    impl = _impl,
    attrs = {
        "actual": attrs.transition_dep(cfg = "root//:transition"),
    },
)

incoming_transition_alias_vnew = rule(
    impl = _impl,
    attrs = {},
    supports_incoming_transition = True,
)
