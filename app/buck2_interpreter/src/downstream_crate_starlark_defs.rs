/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_util::late_binding::LateBinding;
use starlark::environment::GlobalsBuilder;

/// Globals defined in `buck2_build_api`.
pub static REGISTER_BUCK2_BUILD_API_GLOBALS: LateBinding<fn(&mut GlobalsBuilder)> =
    LateBinding::new("REGISTER_BUCK2_BUILD_API_GLOBALS");

/// `__internal__`s defined in `buck2_build_api`.
pub static REGISTER_BUCK2_BUILD_API_INTERNALS: LateBinding<fn(&mut GlobalsBuilder)> =
    LateBinding::new("REGISTER_BUCK2_BUILD_API_INTERNALS");

/// Globals defined in `buck2_transitions` crate.
pub static REGISTER_BUCK2_TRANSITION_GLOBALS: LateBinding<fn(&mut GlobalsBuilder)> =
    LateBinding::new("REGISTER_BUCK2_TRANSITION_GLOBALS");

/// Globals defined in `buck2_action_impl` crate.
pub static REGISTER_BUCK2_ACTION_IMPL_GLOBALS: LateBinding<fn(&mut GlobalsBuilder)> =
    LateBinding::new("REGISTER_BUCK2_ACTION_IMPL_GLOBALS");

/// Globals defined in `buck2_anon_targets` crate.
pub static REGISTER_BUCK2_ANON_TARGETS_GLOBALS: LateBinding<fn(&mut GlobalsBuilder)> =
    LateBinding::new("REGISTER_BUCK2_ANON_TARGETS_GLOBALS");

/// Globals defined in `buck2_bxl` crate,
/// which are used to create the context for `.bxl` evaluation.
pub static REGISTER_BUCK2_BXL_GLOBALS: LateBinding<fn(&mut GlobalsBuilder)> =
    LateBinding::new("REGISTER_BUCK2_BXL_GLOBALS");

/// Globals defined in `buck2_cfg_constructor` crate.
pub static REGISTER_BUCK2_CFG_CONSTRUCTOR_GLOBALS: LateBinding<fn(&mut GlobalsBuilder)> =
    LateBinding::new("REGISTER_BUCK2_CFG_CONSTRUCTOR_GLOBALS");
