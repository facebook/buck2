# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

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

    if refs._opt_by_default__opt[ConstraintValueInfo].setting.label not in constraints:
        return platform

    # check if native debug is enabled, if so cancel the transition
    maybe_native_debug_constraints = getattr(constraints.get(refs._opt_by_default_native_debug_enabled[ConstraintValueInfo].setting.label), "label", None)
    if maybe_native_debug_constraints == refs._opt_by_default_native_debug_enabled[ConstraintValueInfo].label:
        return platform

    mode_constraint = constraints[refs._opt_by_default__opt[ConstraintValueInfo].setting.label].label
    is_dev = mode_constraint == refs._opt_by_default__dev[ConstraintValueInfo].label
    is_opt = mode_constraint == refs._opt_by_default__opt[ConstraintValueInfo].label

    # Check if the build mode is either dev or opt. If not, cancel the transition
    if not is_dev and not is_opt:
        return platform

    sanitizer_constraint = constraints[refs._opt_by_default__no_san[ConstraintValueInfo].setting.label].label
    is_default_dev_sanitizer = sanitizer_constraint == refs._opt_by_default__dev_san[ConstraintValueInfo].label  # this bad boy only shows up in default dev mode 🙏
    is_no_san = sanitizer_constraint == refs._opt_by_default__no_san[ConstraintValueInfo].label

    if is_dev and not (is_default_dev_sanitizer or is_no_san):
        # dev-tsan/dev-asan etc modes still appear as dev, here we check that the user has not specifically requested sanitizers
        # returning here preserves the original behaviour of opt-by-default, but we can likely give a opt + sanitizer config here later
        return platform

    # opt mode comes with nosan by default, if its not default opt then cancel the transition
    if is_opt and not is_no_san:
        return platform

    maybe_lto_constraint = getattr(constraints.get(refs._opt_by_default__lto_none[ConstraintValueInfo].setting.label), "label", None)
    is_lto_none = not maybe_lto_constraint or maybe_lto_constraint == refs._opt_by_default__lto_none[ConstraintValueInfo].label

    # if opt-lto is then cancel the transition
    if is_opt and not is_lto_none:
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
        "_opt_by_default__dev": "@config//build_mode/constraints:dev",
        "_opt_by_default__dev_san": "@config//build_mode/constraints:asan-ubsan-dev",
        "_opt_by_default__fbcode_build_info_mode_full": "@config//build_mode/constraints:fbcode-build-info-mode-full",
        "_opt_by_default__linux": "@config//os/constraints:linux",
        "_opt_by_default__lto_none": "@config//build_mode/constraints:lto-none",
        "_opt_by_default__no_san": "@config//build_mode/constraints:no-san",
        "_opt_by_default__opt": "@config//build_mode/constraints:opt",
        "_opt_by_default__opt_cxx_enabled": "@config//build_mode/default_opt_cxx:enabled",
        "_opt_by_default__split_dwarf_single": "@config//build_mode/constraints:split-dwarf-single",
        "_opt_by_default__static": "@config//build_mode/constraints:static",
        "_opt_by_default_native_debug_enabled": "@config//build_mode/constraints:native-debugging-supported",
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
