/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::deferred::key::DeferredHolderKey;
use dice::DiceComputations;

use crate::build::detailed_aggregated_metrics::types::ActionExecutionMetrics;
use crate::deferred::calculation::DeferredHolder;
pub trait HasDetailedAggregatedMetrics {
    fn action_executed(&self, ev: ActionExecutionMetrics) -> buck2_error::Result<()>;
    fn analysis_started(&self, key: &DeferredHolderKey) -> buck2_error::Result<()>;
    fn analysis_complete(
        &self,
        key: &DeferredHolderKey,
        result: &DeferredHolder,
    ) -> buck2_error::Result<()>;
}
impl HasDetailedAggregatedMetrics for DiceComputations<'_> {
    fn action_executed(&self, _ev: ActionExecutionMetrics) -> buck2_error::Result<()> {
        Ok(())
    }

    fn analysis_started(&self, _key: &DeferredHolderKey) -> buck2_error::Result<()> {
        Ok(())
    }

    fn analysis_complete(
        &self,
        _key: &DeferredHolderKey,
        _result: &DeferredHolder,
    ) -> buck2_error::Result<()> {
        Ok(())
    }
}
