# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# List of constraint settings that can be selected on via `modifiers_match`.
# Modifiers for these constraints are resolved first before other modifiers,
# if they exist. `modifiers_match` keying on other constraint settings will
# fail
# TODO(scottcao): Find a better place to set this so that OSS users can add
# their own constraints.
CONSTRAINT_SETTING_ORDER = [
    "ovr_config//build_mode/constraints:build_mode",
    "ovr_config//build_mode/constraints:core_build_mode",
    "ovr_config//os/constraints:os",
    "ovr_config//cpu/constraints:cpu",
    "ovr_config//build_mode/constraints:lto",
    "prelude//go/constraints:race",
]

def get_constraint_setting_order(refs: dict[str, ProviderCollection]) -> list[TargetLabel]:
    return [refs[constraint_setting][ConstraintSettingInfo].label for constraint_setting in CONSTRAINT_SETTING_ORDER]
