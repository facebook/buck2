# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":common.bzl", "get_tagged_modifier", "tagged_modifier_to_json")
load(":set_cfg_constructor.bzl", "MODIFIER_METADATA_KEY")
load(
    ":types.bzl",
    "Modifier",  # @unused This is used in type annotation
    "ModifierTargetLocation",
    "TaggedModifier",  # @unused This is used in type annotation
)

# TODO(scottcao): This function is just here so we don't have to migrate implementation for `cfg_constructor_pre_constraint_analysis`
# in the same diff. Remove this once we update `cfg_constructor_pre_constraint_analysis` to also accept JSONs for target modifiers
# instead of records.
def cfg_modifiers_deprecated(modifiers: dict[str, Modifier]) -> dict[str, dict[str, TaggedModifier]]:
    tagged_modifiers = {}
    for constraint_setting, modifier in modifiers.items():
        tagged_modifiers[constraint_setting] = get_tagged_modifier(constraint_setting, modifier, ModifierTargetLocation())
    return {MODIFIER_METADATA_KEY: tagged_modifiers}

def cfg_modifiers(modifiers: dict[str, Modifier]) -> dict[str, dict[str, dict[str, typing.Any]]]:
    tagged_modifier_jsons = {}
    for constraint_setting, modifier in modifiers.items():
        tagged_modifier_jsons[constraint_setting] = tagged_modifier_to_json(
            get_tagged_modifier(constraint_setting, modifier, ModifierTargetLocation()),
        )
    return {MODIFIER_METADATA_KEY: tagged_modifier_jsons}
