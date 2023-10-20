# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:asserts.bzl", "asserts")
load(
    ":types.bzl",
    "CfgModifier",
    "CfgModifierCliLocation",
    "CfgModifierInfo",
    "CfgModifierLocation",
    "CfgModifierPackageLocation",
    "CfgModifierTargetLocation",
    "CfgModifierWithLocation",
    "ModifierSelect",
    "ModifierSelectInfo",
)

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

def verify_normalized_target(target: str, param_context: str, location: CfgModifierLocation):
    if isinstance(location, CfgModifierCliLocation):
        fail("Internal error: location should not be CfgModifierCliLocation")
    function_context = "set_cfg_modifier" if isinstance(location, CfgModifierPackageLocation) else "cfg_modifier"

    # Do some basic checks that target looks reasonably valid and normalized
    # Targets should always be fully qualified to improve readability.
    if "//" not in target or target.startswith("//") or ":" not in target:
        fail(
            "Must specify fully qualified target (ex. `cell//foo:bar`) for `{}` of `{}`. Found `{}`".format(
                param_context,
                function_context,
                target,
            ),
        )

_CONSTRAINT_SETTING_PARAM = "constraint_setting"
_MODIFIER_PARAM = "modifier"

def cfg_modifier_common_impl(
        constraint_setting: str,
        modifier: CfgModifier,
        location: CfgModifierLocation) -> (str, CfgModifierWithLocation):
    verify_normalized_target(constraint_setting, _CONSTRAINT_SETTING_PARAM, location)
    verify_normalized_target(modifier, _MODIFIER_PARAM, location)

    modifier_key = constraint_setting_to_modifier_key(constraint_setting)
    modifier_with_loc = CfgModifierWithLocation(
        modifier = modifier,
        location = location,
    )
    return modifier_key, modifier_with_loc

def get_modifier_info(
        refs: dict[str, ProviderCollection],
        modifier: CfgModifier,
        constraint_setting: str,
        location: CfgModifierLocation) -> CfgModifierInfo:
    # Gets a modifier info from a modifier based on providers from `refs`.
    if isinstance(modifier, ModifierSelect):
        default = None
        modifier_selector_info = []
        for key, sub_modifier in modifier.selector.items():
            if key == "DEFAULT":
                default = get_modifier_info(refs, sub_modifier, constraint_setting, location)
            else:
                configuration_info = refs[key][ConfigurationInfo]
                sub_modifier_info = get_modifier_info(refs, sub_modifier, constraint_setting, location)
                modifier_selector_info.append((configuration_info, sub_modifier_info))
        return ModifierSelectInfo(
            default = default,
            selector = modifier_selector_info,
        )
    if isinstance(modifier, str):
        constraint_value_info = refs[modifier][ConstraintValueInfo]
        constraint_setting_info = refs[constraint_setting][ConstraintSettingInfo]
        if constraint_setting_info.label != constraint_value_info.setting.label:
            fail(
                (
                    "Mismatched constraint setting and modifier: modifier `{}` from `{}` modifies constraint setting `{}`, but the provided constraint setting is `{}`"
                ).format(
                    modifier,
                    location_to_string(location),
                    constraint_value_info.setting.label,
                    constraint_setting,
                ),
            )
        return refs[modifier][ConstraintValueInfo]
    fail("Internal error: Found unexpected modifier `{}` type `{}`".format(modifier, type(modifier)))

def _is_subset(a: ConfigurationInfo, b: ConfigurationInfo) -> bool:
    for (constraint_setting, a_constraint_value) in a.constraints.items():
        b_constraint_value = b.constraints.get(constraint_setting)
        if a_constraint_value != b_constraint_value:
            return False
    return True

def resolve_modifier(cfg: ConfigurationInfo, modifier: CfgModifierInfo) -> ConstraintValueInfo | None:
    # Resolve the modifier and return the constraint value to add to the configuration, if there is one
    if isinstance(modifier, ModifierSelectInfo):
        for key, sub_modifier in modifier.selector:
            if _is_subset(key, cfg):
                # If constraints in key of the select are a subset of the constraints in the
                # current configuration, then it's a match.
                return resolve_modifier(cfg, sub_modifier)
        if modifier.default:
            return resolve_modifier(cfg, modifier.default)
        return None
    if isinstance(modifier, ConstraintValueInfo):
        return modifier
    fail("Internal error: Found unexpected modifier `{}` type `{}`".format(modifier, type(modifier)))
