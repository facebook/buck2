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
    "ModifierLocation",
    "ModifierPackageLocation",
    "ModifierSelectInfo",
    "ModifierTargetLocation",
    "TaggedModifier",
    "TaggedModifiers",
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
    fail("Internal error. Unrecognized location type `{}` for location `{}`".format(type(location), location))

def verify_normalized_target(target: str):
    # Do some basic checks that target looks reasonably valid and normalized
    # Targets should always be fully qualified to improve readability.
    if "//" not in target or target.startswith("//") or ":" not in target:
        fail(
            "Must specify fully qualified target (ex. `cell//foo:bar`). Found `{}`".format(
                target,
            ),
        )

def is_modifier_select(modifier: Modifier) -> bool:
    if isinstance(modifier, str):
        return False
    if isinstance(modifier, dict):
        if modifier["_type"] != "ModifierSelect":
            fail("Found unknown dictionary `{}` for modifier".format(modifier))
        return True
    fail("Modifier should either be a string or dict. Found `{}`".format(modifier))

def verify_normalized_modifier(modifier: Modifier):
    if is_modifier_select(modifier):
        # TODO(scottcao): Add a test case for this once `bxl_test` supports testing failures
        for key, sub_modifier in modifier.items():
            if key != "_type":
                verify_normalized_modifier(sub_modifier)
    elif isinstance(modifier, str):
        verify_normalized_target(modifier)
    else:
        fail("Found unexpected modifier `{}` type `{}`".format(modifier, type(modifier)))

def get_tagged_modifier(
        modifier: Modifier,
        location: ModifierLocation) -> TaggedModifier:
    verify_normalized_modifier(modifier)

    return TaggedModifier(
        modifier = modifier,
        location = location,
    )

def get_tagged_modifiers(
        modifiers: list[Modifier],
        location: ModifierLocation) -> TaggedModifiers:
    for modifier in modifiers:
        verify_normalized_modifier(modifier)

    return TaggedModifiers(
        modifiers = modifiers,
        location = location,
    )

def get_constraint_setting(constraint_settings: dict[TargetLabel, None], modifier: Modifier, location: ModifierLocation) -> TargetLabel:
    if len(constraint_settings) == 0:
        fail("`modifier_select` cannot be empty. Found empty `modifier_select` at `{}`".format(location_to_string(location)))
    if len(constraint_settings) > 1:
        fail(
            "A single modifier can only modify a single constraint setting.\n" +
            "Modifier `{}` from `{}` is found to modify the following constraint settings:\n".format(
                modifier,
                location_to_string(location),
            ) + "\n".join(constraint_settings.keys()),
        )
    return list(constraint_settings.keys())[0]

def get_modifier_info(
        refs: dict[str, ProviderCollection],
        modifier: Modifier,
        location: ModifierLocation,
        constraint_setting_order: list[TargetLabel]) -> (TargetLabel, ModifierInfo):
    # Gets a modifier info from a modifier based on providers from `refs`.
    if is_modifier_select(modifier):
        default = None
        modifier_selector_info = []
        constraint_settings = {}  # Used like a set
        for key, sub_modifier in modifier.items():
            if key == "DEFAULT":
                default_constraint_setting, default = get_modifier_info(refs, sub_modifier, location, constraint_setting_order)
                constraint_settings[default_constraint_setting] = None
            elif key != "_type":
                cfg_info = refs[key][ConfigurationInfo]
                for cfg_constraint_setting, cfg_constraint_value_info in cfg_info.constraints.items():
                    asserts.true(
                        cfg_constraint_setting in constraint_setting_order,
                        (
                            "modifier_select `{}` from `{}` selects on `{}` of constraint_setting `{}`, which is now allowed. " +
                            "To select on this constraint, this constraint setting needs to be added to `buck2/cfg/experimental/cfg_constructor.bzl`"
                        ).format(modifier, location_to_string(location), cfg_constraint_value_info.label, cfg_constraint_setting),
                    )
                sub_constraint_setting, sub_modifier_info = get_modifier_info(refs, sub_modifier, location, constraint_setting_order)
                constraint_settings[sub_constraint_setting] = None
                modifier_selector_info.append((cfg_info, sub_modifier_info))

        constraint_setting = get_constraint_setting(constraint_settings, modifier, location)

        return constraint_setting, ModifierSelectInfo(
            default = default,
            selector = modifier_selector_info,
        )
    if isinstance(modifier, str):
        constraint_value_info = refs[modifier][ConstraintValueInfo]
        return constraint_value_info.setting.label, constraint_value_info
    fail("Internal error: Found unexpected modifier `{}` type `{}`".format(modifier, type(modifier)))

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

def modifier_to_refs(modifier: Modifier, location: ModifierLocation) -> list[str]:
    # Obtain a list of targets to analyze from a modifier.
    refs = []
    if is_modifier_select(modifier):
        for key, sub_modifier in modifier.items():
            if key != "_type":
                if key != "DEFAULT":
                    refs.append(key)
                refs.extend(modifier_to_refs(sub_modifier, location))
    elif isinstance(modifier, str):
        refs.append(modifier)
    else:
        fail("Internal error: Found unexpected modifier `{}` type `{}`".format(modifier, type(modifier)))
    return refs

def tagged_modifier_to_json(tagged_modifier: TaggedModifier) -> dict[str, typing.Any]:
    return {
        "location": _location_to_json(tagged_modifier.location),
        "modifier": tagged_modifier.modifier,
        "_type": "TaggedModifier",
    }

def tagged_modifiers_to_json(tagged_modifiers: TaggedModifiers) -> dict[str, typing.Any]:
    return {
        "location": _location_to_json(tagged_modifiers.location),
        "modifiers": tagged_modifiers.modifiers,
        "_type": "TaggedModifiers",
    }

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
        modifier = j["modifier"],
    )

def _json_to_location(j: dict[str, str]) -> ModifierLocation:
    modifier_type = j.pop("_type")
    if modifier_type == "ModifierPackageLocation":
        return ModifierPackageLocation(package_path = j["package_path"])
    if modifier_type == "ModifierTargetLocation":
        return ModifierTargetLocation()
    fail("Internal error: cannot deserialize location `{}`".format(j))
