/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

/// Trait for configuration constructor functions.
/// Functions must be specified via buckconfigs
/// `buck2.cfg_constructor_pre_constraint_analysis` and
/// `buck2.cfg_constructor_post_constraint_analysis`.
/// The output of invoking these functions is a PlatformInfo
pub trait CfgConstructorImpl: Send + Sync + Allocative {}

pub static CFG_CONSTRUCTOR_CALCULATION_IMPL: LateBinding<
    &'static dyn CfgConstructorCalculationImpl,
> = LateBinding::new("CFG_CONSTRUCTOR_CALCULATION_IMPL");

#[async_trait]
pub trait CfgConstructorCalculationImpl: Send + Sync {
    /// Loads and returns cfg constructor functions from buckconfigs
    /// `buck2.cfg_constructor_pre_constraint_analysis` and
    /// `buck2.cfg_constructor_post_constraint_analysis`
    async fn get_cfg_constructor(
        &self,
        ctx: &DiceComputations,
    ) -> anyhow::Result<Option<Arc<dyn CfgConstructorImpl>>>;
}
