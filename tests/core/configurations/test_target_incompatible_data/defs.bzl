# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

_transition = transition(
    refs = {},
    impl = lambda platform, refs: PlatformInfo(configuration = platform.configuration, label = "<transitioned>"),
)

transitioned_stub = rule(
    attrs = {
        "stub": attrs.transition_dep(cfg = _transition),
    },
    impl = lambda ctx: ctx.attrs.stub,
)
