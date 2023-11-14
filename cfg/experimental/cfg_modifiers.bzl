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
    "Modifier",
    "ModifierTargetLocation",
)

def cfg_modifiers(modifiers: list[Modifier]) -> dict[str, list[dict[str, typing.Any]]]:
    tagged_modifier_jsons = [tagged_modifier_to_json(
        get_tagged_modifier(modifier, ModifierTargetLocation()),
    ) for modifier in modifiers]
    return {MODIFIER_METADATA_KEY: tagged_modifier_jsons}
