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

    # Do some basic checks that target looks reasonably valid and normalized
    # Targets should always be fully qualified to improve readability.
    if "//" not in target or target.startswith("//") or ":" not in target:
        fail(
            "Must specify fully qualified target (ex. `cell//foo:bar`) for `{}`. Found `{}`".format(
                param_context,
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

def get_tagged_modifier(
        constraint_setting: str,
        modifier: Modifier,
        location: ModifierLocation) -> TaggedModifier:
    verify_normalized_target(constraint_setting, _CONSTRAINT_SETTING_PARAM, location)
    verify_normalized_modifier(modifier, location)

    tagged_modifier = TaggedModifier(
        modifier = modifier,
        location = location,
    )
    return tagged_modifier

def merge_modifiers(tagged_modifiers: list[typing.Any] | None, tagged_modifier: TaggedModifier, to_json: bool) -> list[typing.Any]:
    def transform(tagged_modifier: TaggedModifier):
        return tagged_modifier_to_json(tagged_modifier) if to_json else tagged_modifier

    if isinstance(tagged_modifier.modifier, str):
        return [transform(tagged_modifier)]

    # `read_parent_package_value` returns an immutable value,
    # so if `tagged_modifiers` is already a list, then we need to copy it to make it mutable.
    tagged_modifiers = list(tagged_modifiers) if tagged_modifiers else []
    tagged_modifiers.append(transform(tagged_modifier))
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

def tagged_modifier_to_json(tagged_modifier: TaggedModifier) -> dict[str, typing.Any]:
    return {
        "location": _location_to_json(tagged_modifier.location),
        "modifier": _modifier_to_json(tagged_modifier.modifier),
        "_type": "TaggedModifier",
    }

def _modifier_to_json(modifier: Modifier) -> dict[str, typing.Any] | str:
    if isinstance(modifier, ModifierSelect):
        return {"_type": "ModifierSelect"} | {
            k: _modifier_to_json(v)
            for k, v in modifier.selector.items()
        }
    if isinstance(modifier, str):
        return modifier
    fail("Internal error: Found unexpected modifier `{}` type `{}`".format(modifier, type(modifier)))

def _location_to_json(location: ModifierLocation) -> dict[str, str]:
    if isinstance(location, ModifierPackageLocation):
        return {"package_path": location.package_path, "_type": "ModifierPackageLocation"}
    if isinstance(location, ModifierTargetLocation):
        return {"_type": "ModifierTargetLocation"}
    if isinstance(location, _LEGACY_PLATFORM_LOCATION_STR) or isinstance(location, _CLI_LOCATION_STR):
        fail("Internal error: location shouldn't be specified as `{}`".format(type(location)))
    fail("Internal error: unknown location `{}` with type `{}`".format(location, type(location)))

def json_to_tagged_modifier(j: dict[str, typing.Any]) -> TaggedModifier:
    if j["_type"] != "TaggedModifier":
        fail("Internal error: `{}` is not a `TaggedModifier`".format(j))
    return TaggedModifier(
        location = _json_to_location(j["location"]),
        modifier = _json_to_modifier(j["modifier"]),
    )

def _json_to_modifier(j: dict[str, typing.Any] | str) -> Modifier:
    if isinstance(j, str):
        return j
    asserts.true(j.pop("_type") == "ModifierSelect", "Internal error: cannot deserialize modifier `{}`".format(j))
    return ModifierSelect(
        selector = {k: _json_to_modifier(v) for k, v in j.items()},
    )

def _json_to_location(j: dict[str, str]) -> ModifierLocation:
    modifier_type = j.pop("_type")
    if modifier_type == "ModifierPackageLocation":
        return ModifierPackageLocation(package_path = j["package_path"])
    if modifier_type == "ModifierTargetLocation":
        return ModifierTargetLocation()
    fail("Internal error: cannot deserialize location `{}`".format(j))
