# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:asserts.bzl", "asserts")
load(
    ":types.bzl",
    "Modifier",
    "ModifierCliLocation",
    "ModifierInfo",
    "ModifierLegacyPlatformLocation",
    "ModifierLocation",
    "ModifierPackageLocation",
    "ModifierSelect",
    "ModifierSelectInfo",
    "ModifierTargetLocation",
    "TaggedModifier",
    "TaggedModifierInfo",
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
_LEGACY_PLATFORM_LOCATION_STR = "legacy target platform"

def location_to_string(location: ModifierLocation) -> str:
    if isinstance(location, ModifierPackageLocation):
        return location.package_path
    if isinstance(location, ModifierTargetLocation):
        return _TARGET_LOCATION_STR
    if isinstance(location, ModifierCliLocation):
        return _CLI_LOCATION_STR
    if isinstance(location, ModifierLegacyPlatformLocation):
        return _LEGACY_PLATFORM_LOCATION_STR
    fail("Internal error. Unrecognized location type `{}` for location `{}`".format(type(location), location))

def verify_normalized_target(target: str, param_context: str, location: ModifierLocation):
    if isinstance(location, ModifierCliLocation):
        fail("Internal error: location should not be ModifierCliLocation")
    if isinstance(location, ModifierLegacyPlatformLocation):
        fail("Internal error: location should not be ModifierLegacyPlatformLocation")
    function_context = "set_cfg_modifiers" if isinstance(location, ModifierPackageLocation) else "cfg_modifiers"

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

def verify_normalized_modifier(modifier: Modifier, location: ModifierLocation):
    if isinstance(modifier, ModifierSelect):
        for key, sub_modifier in modifier.selector.items():
            if key != "DEFAULT":
                verify_normalized_modifier(sub_modifier, location)
    elif isinstance(modifier, str):
        verify_normalized_target(modifier, _MODIFIER_PARAM, location)
    else:
        fail("Found unexpected modifier `{}` type `{}`".format(modifier, type(modifier)))

def cfg_modifier_common_impl(
        constraint_setting: str,
        modifier: Modifier,
        location: ModifierLocation) -> (str, TaggedModifier):
    verify_normalized_target(constraint_setting, _CONSTRAINT_SETTING_PARAM, location)
    verify_normalized_modifier(modifier, location)

    modifier_key = constraint_setting_to_modifier_key(constraint_setting)
    tagged_modifier = TaggedModifier(
        modifier = modifier,
        location = location,
    )
    return modifier_key, tagged_modifier

def merge_modifiers(tagged_modifiers: list[TaggedModifier] | None, tagged_modifier: TaggedModifier) -> list[TaggedModifier]:
    if isinstance(tagged_modifier.modifier, str):
        return [tagged_modifier]

    # `read_parent_package_value` returns an immutable value,
    # so if `tagged_modifiers` is already a list, then we need to copy it to make it mutable.
    tagged_modifiers = list(tagged_modifiers) if tagged_modifiers else []
    tagged_modifiers.append(tagged_modifier)
    return tagged_modifiers

def get_modifier_info(
        refs: dict[str, ProviderCollection],
        modifier: Modifier,
        constraint_setting: str,
        location: ModifierLocation,
        constraint_setting_order: list[TargetLabel]) -> ModifierInfo:
    # Gets a modifier info from a modifier based on providers from `refs`.
    if isinstance(modifier, ModifierSelect):
        default = None
        modifier_selector_info = []
        for key, sub_modifier in modifier.selector.items():
            if key == "DEFAULT":
                default = get_modifier_info(refs, sub_modifier, constraint_setting, location, constraint_setting_order)
            else:
                cfg_info = refs[key][ConfigurationInfo]
                for cfg_constraint_setting, cfg_constraint_value_info in cfg_info.constraints.items():
                    asserts.true(
                        cfg_constraint_setting in constraint_setting_order,
                        (
                            "modifier_select `{}` from `{}` selects on `{}` of constraint_setting `{}`, which is now allowed. " +
                            "To select on this constraint, this constraint setting needs to be added to `buck2/cfg/experimental/cfg_constructor.bzl`"
                        ).format(modifier, location_to_string(location), cfg_constraint_value_info.label, constraint_setting),
                    )
                sub_modifier_info = get_modifier_info(refs, sub_modifier, constraint_setting, location, constraint_setting_order)
                modifier_selector_info.append((cfg_info, sub_modifier_info))
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

def get_modifier_info_with_loc(
        refs: dict[str, ProviderCollection],
        constraint_setting: str,
        tagged_modifier: TaggedModifier) -> (TargetLabel, TaggedModifierInfo):
    modifier_info = get_modifier_info(refs, tagged_modifier.modifier)
    constraint_setting = refs[constraint_setting][ConstraintSettingInfo]
    return constraint_setting.label, TaggedModifierInfo(
        modifier_info = modifier_info,
        location = tagged_modifier.location,
    )

def _is_subset(a: ConfigurationInfo, b: ConfigurationInfo) -> bool:
    for (constraint_setting, a_constraint_value) in a.constraints.items():
        b_constraint_value = b.constraints.get(constraint_setting)
        if a_constraint_value != b_constraint_value:
            return False
    return True

def resolve_modifier(cfg: ConfigurationInfo, modifier: ModifierInfo) -> ConstraintValueInfo | None:
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

def modifier_to_refs(modifier: Modifier, constraint_setting: str, location: ModifierLocation) -> list[str]:
    # Obtain a list of targets to analyze from a modifier.
    refs = []
    if isinstance(modifier, ModifierSelect):
        for key, sub_modifier in modifier.selector.items():
            if key != "DEFAULT":
                refs.append(key)
            refs.extend(modifier_to_refs(sub_modifier, constraint_setting, location))
    elif isinstance(modifier, str):
        refs.append(modifier)
    else:
        fail("Internal error: Found unexpected modifier `{}` type `{}`".format(modifier, type(modifier)))
    return refs
