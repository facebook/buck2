# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _cgo_enabled_transition(platform, refs, attrs):
    if attrs.cgo_enabled == None:
        cgo_enabled_ref = refs.cgo_enabled_auto
    elif attrs.cgo_enabled == True:
        cgo_enabled_ref = refs.cgo_enabled_true
    else:
        cgo_enabled_ref = refs.cgo_enabled_false

    cgo_enabled_value = cgo_enabled_ref[ConstraintValueInfo]
    constraints = platform.configuration.constraints
    constraints[cgo_enabled_value.setting.label] = cgo_enabled_value

    new_cfg = ConfigurationInfo(
        constraints = constraints,
        values = platform.configuration.values,
    )

    return PlatformInfo(
        label = platform.label,
        configuration = new_cfg,
    )

cgo_enabled_transition = transition(
    impl = _cgo_enabled_transition,
    refs = {
        "cgo_enabled": "prelude//go/constraints:cgo_enabled",
        "cgo_enabled_auto": "prelude//go/constraints:cgo_enabled_auto",
        "cgo_enabled_false": "prelude//go/constraints:cgo_enabled_false",
        "cgo_enabled_true": "prelude//go/constraints:cgo_enabled_true",
    },
    attrs = ["cgo_enabled"],
)
