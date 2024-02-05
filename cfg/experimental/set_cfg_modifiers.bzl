# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbsource//tools/build_defs/buck2:is_buck2.bzl", "is_buck2")
load(":common.bzl?v2_only", "get_tagged_modifiers", "tagged_modifiers_to_json")
load(":set_cfg_constructor.bzl?v2_only", "MODIFIER_METADATA_KEY")
load(":types.bzl?v2_only", "Modifier", "ModifierPackageLocation")

def set_cfg_modifiers(modifiers: list[Modifier]):
    """
    Sets a configuration modifier for all targets under this PACKAGE file. This can only be called from a PACKAGE file context
    (e.g. a PACKAGE file or a bzl file transitively loaded by a PACKAGE file).

    Args:
        constraint_setting:
            The constraint key to modify. This must be a `constraint_setting` target.
            For example, to change the OS constraint setting in fbsource, this would be "ovr_config//os/constraints:os".
        modifier:
            The modifier to change this constraint setting. This can be a constraint value target.
            For example, to change the OS to linux in fbsource, this can be specified as "ovr_config//os/constraints:linux".
            TODO(scottcao): Add support for modifier select types.
    """
    if not is_buck2():
        return

    # Make this buck1-proof
    call_stack_frame = getattr(native, "call_stack_frame", None)

    # To ensure that modifiers set in PACKAGE files are easily codemoddable
    # We want to enforce that `set_cfg_modifiers` is only invokable from a PACKAGE file and not a bzl file
    if not call_stack_frame(1).module_path.endswith("/PACKAGE"):
        fail("set_cfg_modifiers is only allowed to be used from PACKAGE files, not a bzl file")

    # Make this buck1-proof
    write_package_value = getattr(native, "write_package_value", None)
    read_parent_package_value = getattr(native, "read_parent_package_value", None)

    merged_modifier_jsons = read_parent_package_value(MODIFIER_METADATA_KEY)

    # `read_parent_package_value` returns immutable values. `list()` makes it mutable.
    merged_modifier_jsons = list(merged_modifier_jsons) if merged_modifier_jsons else []

    tagged_modifiers = get_tagged_modifiers(
        modifiers,
        ModifierPackageLocation(package_path = _get_package_path()),
    )
    merged_modifier_jsons.append(tagged_modifiers_to_json(tagged_modifiers))

    write_package_value(
        MODIFIER_METADATA_KEY,
        merged_modifier_jsons,
        overwrite = True,
    )

_BUCK1_COMPAT_STR = "Internal error: Modifiers are for buck2 only"

def _get_package_path() -> str:
    """
    Returns the cell-relative path of the current PACKAGE file.
    Ex. `foo//bar/PACKAGE`
    """
    if not is_buck2():
        return _BUCK1_COMPAT_STR

    # Make this buck1-proof
    get_cell_name = getattr(native, "get_cell_name", None)
    get_base_path = getattr(native, "get_base_path", None)
    return "{}//{}/PACKAGE".format(get_cell_name(), get_base_path())
