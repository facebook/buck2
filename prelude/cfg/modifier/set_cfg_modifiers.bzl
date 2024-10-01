# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:prelude.bzl", "native")
load(":common.bzl", "MODIFIER_METADATA_KEY", "get_tagged_modifiers", "tagged_modifiers_to_json")
load(":types.bzl", "Modifier", "ModifierPackageLocation")  # @unused Used in type annotation

def set_cfg_modifiers(
        cfg_modifiers: list[Modifier] | None = None,
        extra_cfg_modifiers_per_rule: dict[str, list[Modifier]] | None = None):
    """
    Sets a configuration modifier for all targets under this PACKAGE file. This can only be called from a PACKAGE file context
    (e.g. a PACKAGE file or a bzl file transitively loaded by a PACKAGE file).

    Args:
        cfg_modifiers:
            A list of modifiers to set. The simplest modifier is a constraint value target.
            For example, to change the OS to linux in fbsource, this can be specified as `["ovr_config//os/constraints:linux"]`.
        extra_cfg_modifiers_per_rule:
            A dictionary of rule name to a list of modifiers to set. This is applied on top of modifiers from `cfg_modifiers` parameter
            if a target's rule name matches the key, so it can override any modifier from `cfg_modifiers` parameter in the same PACKAGE.
            For example, if this dictionary is `{"python_binary": ["ovr_config//os/constraints:macos"]}`,
            then all python_binary targets covered will have the macos constraint added to their configurations.
    """

    # Make this buck1-proof
    call_stack_frame = getattr(native, "call_stack_frame", None)

    # To ensure that modifiers set in PACKAGE files are easily codemoddable
    # We want to enforce that `set_cfg_modifiers` is only invokable from a PACKAGE file and not a bzl file
    module_path = call_stack_frame(1).module_path
    if not module_path.endswith(("/PACKAGE", "/BUCK_TREE")) and module_path not in ("PACKAGE", "BUCK_TREE"):
        fail("set_cfg_modifiers is only allowed to be used from PACKAGE files, not a bzl file")

    cfg_modifiers = cfg_modifiers or []
    extra_cfg_modifiers_per_rule = extra_cfg_modifiers_per_rule or {}

    # Make this buck1-proof
    write_package_value = getattr(native, "write_package_value", None)
    read_parent_package_value = getattr(native, "read_parent_package_value", None)

    merged_modifier_jsons = read_parent_package_value(MODIFIER_METADATA_KEY)

    # `read_parent_package_value` returns immutable values. `list()` makes it mutable.
    merged_modifier_jsons = list(merged_modifier_jsons) if merged_modifier_jsons else []

    tagged_modifiers_list = get_tagged_modifiers(
        cfg_modifiers,
        extra_cfg_modifiers_per_rule,
        ModifierPackageLocation(package_path = _get_package_path()),
    )
    merged_modifier_jsons += [tagged_modifiers_to_json(tagged_modifiers) for tagged_modifiers in tagged_modifiers_list]

    write_package_value(
        MODIFIER_METADATA_KEY,
        merged_modifier_jsons,
        overwrite = True,
    )

def _get_package_path() -> str:
    """
    Returns the cell-relative path of the current PACKAGE file.
    Ex. `foo//bar/PACKAGE`
    """

    # Make this buck1-proof
    get_cell_name = getattr(native, "get_cell_name", None)
    get_base_path = getattr(native, "get_base_path", None)
    return "{}//{}/PACKAGE".format(get_cell_name(), get_base_path())
