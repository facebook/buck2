# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _tr_impl(platform, refs):
    _platform = platform  # @unused
    _refs = refs  # @unused
    pass

_tr = transition(
    impl = _tr_impl,
    refs = {},
)

def _impl(_ctx):
    pass

incoming_edge_transition_stub = rule(
    impl = _impl,
    attrs = {
        "configured_deps": attrs.list(attrs.configured_dep(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
    },
    cfg = _tr,
)
