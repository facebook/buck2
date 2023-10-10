# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# TODO(scottcao): Annotate these types with comments once implementation is complete

# Metadata types for where cfg modifier is defined. We need to track this to give users error messages
# that include where the bad modifier comes from.

# Modifier defined in a PACKAGE file. We track path of that PACKAGE file.
CfgModifierPackageLocation = record(package_path = str)

# Modifier defined on the target in buildfile.
CfgModifierTargetLocation = record()

# Modifier specified via command line from the user
CfgModifierCliLocation = record()

# This is a handy way of specifying a rust-style enum in Starlark.
CfgModifierLocation = CfgModifierPackageLocation | CfgModifierTargetLocation | CfgModifierCliLocation

# Modifier types as how they appear to the user via `set_cfg_modifier` or `cfg_modifier` function.

CfgModifier = str

CfgModifierWithLocation = record(
    modifier = CfgModifier,
    location = CfgModifierLocation,
)

# Modifier types after analysis of configuration rules.
# There is an equivalent post-constraint-analysis type for every modifier type listed above.
# An "Info" is added to the type name to denote post-constraint-analysis version of the
# modifier type.

CfgModifierInfo = record(
    modifier = ConstraintValueInfo,
    setting = ConstraintSettingInfo,
)

CfgModifierInfoWithLocation = record(
    modifier_info = CfgModifierInfo,
    location = CfgModifierLocation,
)
