# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(_ctx):
    return [
        DefaultInfo(),
        TransitionInfo(
            impl = lambda platform: platform,
        ),
    ]

nop_transition = rule(
    impl = _impl,
    attrs = {},
    is_configuration_rule = True,
)

def _impl2(_ctx):
    pass

toolchain_with_incoming_transition = rule(
    impl = _impl2,
    attrs = {},
    is_toolchain_rule = True,
    supports_incoming_transition = True,
)
