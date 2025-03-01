# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _constraint_override_impl(ctx):
    c = ctx.attrs.constraint_to_add[ConstraintValueInfo]

    def _transition(platform):
        constraints = dict(platform.configuration.constraints)
        constraints[c.setting.label] = c
        new_cfg = ConfigurationInfo(
            constraints = constraints,
            values = platform.configuration.values,
        )
        return PlatformInfo(
            label = "transitioned",
            configuration = new_cfg,
        )

    return [
        DefaultInfo(),
        TransitionInfo(
            impl = _transition,
        ),
    ]

constraint_override_transition = rule(
    impl = _constraint_override_impl,
    attrs = {
        "constraint_to_add": attrs.dep(),
    },
    is_configuration_rule = True,
)

def _impl(_ctx):
    pass

stub_transition = rule(
    impl = _impl,
    attrs = {
        "dep": attrs.transition_dep(cfg = "//:transition"),
    },
)

stub_with_incoming_transition = rule(
    impl = _impl,
    attrs = {
        "dep": attrs.dep(),
    },
    supports_incoming_transition = True,
)
