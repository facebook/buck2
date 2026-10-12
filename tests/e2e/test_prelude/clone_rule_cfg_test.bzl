# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:rules.bzl", "clone_rule")

def _caller_cfg_transition_impl(platform, refs):
    cfg_value = refs.cfg_value[ConstraintValueInfo]
    constraints = dict(platform.configuration.constraints)
    constraints[cfg_value.setting.label] = cfg_value
    return PlatformInfo(
        label = "clone_rule_caller_cfg",
        configuration = ConfigurationInfo(
            constraints = constraints,
            values = {},
        ),
    )

_caller_cfg_transition = transition(
    impl = _caller_cfg_transition_impl,
    refs = {
        "cfg_value": "fbcode//buck2/tests/e2e/test_prelude:clone_rule_cfg_value",
    },
)

clone_rule_cfg_test_rule = clone_rule(
    "export_file",
    cfg = _caller_cfg_transition,
)
