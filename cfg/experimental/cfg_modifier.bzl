# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":common.bzl", "cfg_modifier_common_impl")
load(
    ":types.bzl",
    "CfgModifier",
    "CfgModifierTargetLocation",
    "CfgModifierWithLocation",  # @unused This is used in type annotation
)

def cfg_modifier(constraint_setting: str, modifier: CfgModifier) -> dict[str, CfgModifierWithLocation]:
    key, modifier_with_loc = cfg_modifier_common_impl(constraint_setting, modifier, CfgModifierTargetLocation())
    return {key: modifier_with_loc}
