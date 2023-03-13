/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! DICE calculations for bxl

use std::fmt::Debug;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

use crate::bxl::result::BxlResult;
use crate::bxl::types::BxlKey;

#[async_trait]
pub trait BxlCalculationDyn: Debug + Send + Sync + 'static {
    async fn eval_bxl(&self, ctx: &DiceComputations, bxl: BxlKey)
    -> anyhow::Result<Arc<BxlResult>>;
}

/// Dependency injection for BXL.
///
/// BXL implementation lives in downstream crate.
/// This field is initialized at program start, so this crate can call BXL calculation.
pub static BXL_CALCULATION_IMPL: LateBinding<&'static dyn BxlCalculationDyn> =
    LateBinding::new("BXL_CALCULATION_IMPL");

#[async_trait]
pub trait BxlCalculation {
    async fn eval_bxl<'a>(&self, bxl: BxlKey) -> anyhow::Result<Arc<BxlResult>>;
}

#[async_trait]
impl BxlCalculation for DiceComputations {
    async fn eval_bxl<'a>(&self, bxl: BxlKey) -> anyhow::Result<Arc<BxlResult>> {
        BXL_CALCULATION_IMPL.get()?.eval_bxl(self, bxl).await
    }
}
