# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":asserts.bzl", "verify_normalized_target")
load(":common.bzl", "constraint_setting_to_modifier_key")
load(":types.bzl", "CfgModifier", "CfgModifierPackageLocation", "CfgModifierWithLocation")

def set_cfg_modifier(constraint_setting: str, modifier: CfgModifier):
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
    verify_normalized_target(constraint_setting, _CONSTRAINT_SETTING_ERROR_CONTEXT)
    verify_normalized_target(modifier, _MODIFIER_ERROR_CONTEXT)

    package_value_key = constraint_setting_to_modifier_key(constraint_setting)
    write_package_value(
        package_value_key,
        CfgModifierWithLocation(
            # We need to track where this modifier comes from because some user errors
            # can only be identified in the post-constraint analysis cfg constructor.
            # When these errors are identified, we need to be able to provide an
            # error message that clearly points to where the error occurred, including
            # which PACKAGE the modifier was specified.
            # The downside is that this however does make cfg constructor less likely to
            # cache. For example, if
            location = CfgModifierPackageLocation(package_path = _get_package_path()),
            modifier = modifier,
        ),
        overwrite = True,
    )

def _get_package_path() -> str:
    """
    Returns the cell-relative path of the current PACKAGE file.
    Ex. `foo//bar/PACKAGE`
    """
    return "{}//{}/PACKAGE".format(get_cell_name(), get_base_path())

_CONSTRAINT_SETTING_ERROR_CONTEXT = "`constraint_setting` of `set_cfg_modifier`"
_MODIFIER_ERROR_CONTEXT = "`modifier` of `set_cfg_modifier`"
