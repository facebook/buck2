# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
Apply python specific constraints such as opt-by-default
"""

load("@prelude//cfg/modifier:name.bzl", "cfg_name")

# This is used in prelude//transitions:constraints_override.bzl
def _transition_opt_by_default_impl(platform: PlatformInfo, refs: struct, attrs: struct) -> PlatformInfo:
    constraints = platform.configuration.constraints
    linux_key = refs._opt_by_default__linux[ConstraintValueInfo].setting.label

    # if not linux cancel the transition
    if linux_key not in constraints or constraints[linux_key].label != refs._opt_by_default__linux[ConstraintValueInfo].label:
        return platform

    # if opt-by-default is not enabled then cancel the transition
    if not attrs.opt_by_default_enabled:
        return platform

    opt_by_default_constraints = [
        refs._opt_by_default__fbcode_build_info_mode_full[ConstraintValueInfo],
        refs._opt_by_default__static[ConstraintValueInfo],
        refs._opt_by_default__split_dwarf_single[ConstraintValueInfo],
        refs._opt_by_default__opt_cxx_enabled[ConstraintValueInfo],
        refs._opt_by_default__no_san[ConstraintValueInfo],
        refs._opt_by_default__opt[ConstraintValueInfo],
        refs._opt_by_default__enabled[ConstraintValueInfo],
    ]
    for constraint in opt_by_default_constraints:
        constraints[constraint.setting.label] = constraint

    new_cfg = ConfigurationInfo(
        constraints = constraints,
        values = platform.configuration.values,
    )

    return PlatformInfo(
        label = cfg_name(new_cfg),
        configuration = new_cfg,
    )

def _refs():
    return {
        "_opt_by_default__enabled": "@config//toolchain/python/constraints:python-opt-by-default-enabled",
        "_opt_by_default__fbcode_build_info_mode_full": "@config//build_mode/constraints:fbcode-build-info-mode-full",
        "_opt_by_default__linux": "@config//os/constraints:linux",
        "_opt_by_default__no_san": "@config//build_mode/constraints:no-san",
        "_opt_by_default__opt": "@config//build_mode/constraints:opt",
        "_opt_by_default__opt_cxx_enabled": "@config//build_mode/default_opt_cxx:enabled",
        "_opt_by_default__split_dwarf_single": "@config//build_mode/constraints:split-dwarf-single",
        "_opt_by_default__static": "@config//build_mode/constraints:static",
    }

def _attrs():
    return {
        "opt_by_default_enabled": attrs.bool(default = False),
    }

python_transitions = struct(
    transition_opt_by_default_impl = _transition_opt_by_default_impl,
    attrs = _attrs,
    refs = _refs,
)
