/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)]

pub(crate) mod calculation;

use buck2_configured::configuration::cfg_constructor::CfgConstructorImpl;
use buck2_configured::configuration::cfg_constructor::CFG_CONSTRUCTOR_CALCULATION_IMPL;
use calculation::CfgConstructorCalculationInstance;
use starlark::values::OwnedFrozenValue;
struct CfgConstructor {
    cfg_constructor_pre_constraint_analysis: OwnedFrozenValue,
    cfg_constructor_post_constraint_analysis: OwnedFrozenValue,
}

impl CfgConstructorImpl for CfgConstructor {}

pub fn init_late_bindings() {
    CFG_CONSTRUCTOR_CALCULATION_IMPL.init(&CfgConstructorCalculationInstance);
}
