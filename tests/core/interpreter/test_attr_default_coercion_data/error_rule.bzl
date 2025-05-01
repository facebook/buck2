# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _transition_impl(**_kwargs):
    pass

_transition = transition(
    impl = _transition_impl,
    refs = {},
)

def _impl():
    pass

error_rule = rule(
    impl = _impl,
    attrs = {
        "someattr": attrs.transition_dep(default = "notaproperlabel", cfg = _transition),
    },
)
