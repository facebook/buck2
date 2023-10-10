# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:asserts.bzl", "asserts")
load(":types.bzl", "CfgModifierCliLocation", "CfgModifierLocation", "CfgModifierPackageLocation", "CfgModifierTargetLocation")

def constraint_setting_to_modifier_key(constraint_setting: str) -> str:
    # Keys in PACKAGE values/metadata can only contain a single dot.
    # This means that if the constraint setting contains a dot, we must replace it.
    # Forward slash isn't allowed in filenames or target names, so a triple slash is
    # the best replacement for dots.
    constraint_setting_key = constraint_setting.replace(".", "///")
    return "cfg_modifiers." + constraint_setting_key

def modifier_key_to_constraint_setting(modifier_key: str) -> str:
    # Reverse transformation of `constraint_setting_to_modifier_key`
    asserts.true(modifier_key.startswith("cfg_modifiers."), "Internal error: modifier key must start with 'cfg_modifier.'; found '{}'".format(modifier_key))
    return modifier_key.replace("cfg_modifiers.", "").replace("///", ".")

_TARGET_LOCATION_STR = "`metadata` attribute of target"
_CLI_LOCATION_STR = "command line"

def location_to_string(location: CfgModifierLocation) -> str:
    if isinstance(location, CfgModifierPackageLocation):
        return location.package_path
    if isinstance(location, CfgModifierTargetLocation):
        return _TARGET_LOCATION_STR
    if isinstance(location, CfgModifierCliLocation):
        return _CLI_LOCATION_STR
    fail("Internal error. Unrecognized location type `{}` for location `{}`".format(type(location), location))
