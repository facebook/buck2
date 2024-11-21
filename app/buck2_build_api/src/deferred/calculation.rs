/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Dice calculations relating to deferreds

use std::pin::Pin;
use std::sync::Arc;

use allocative::Allocative;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::deferred::base_deferred_key::BaseDeferredKeyDyn;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use dupe::Dupe;
use futures::Future;
use starlark::values::OwnedFrozenValueTyped;

use crate::actions::RegisteredAction;
use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::analysis::registry::RecordedAnalysisValues;
use crate::analysis::AnalysisResult;
use crate::artifact_groups::deferred::TransitiveSetKey;
use crate::artifact_groups::promise::PromiseArtifact;
use crate::bxl::calculation::BXL_CALCULATION_IMPL;
use crate::bxl::result::BxlResult;
use crate::dynamic::calculation::dynamic_lambda_result;
use crate::dynamic::calculation::DynamicLambdaResult;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;

pub static EVAL_ANON_TARGET: LateBinding<
    for<'c> fn(
        &'c mut DiceComputations,
        Arc<dyn BaseDeferredKeyDyn>,
    )
        -> Pin<Box<dyn Future<Output = buck2_error::Result<AnalysisResult>> + Send + 'c>>,
> = LateBinding::new("EVAL_ANON_TARGET");

pub static GET_PROMISED_ARTIFACT: LateBinding<
    for<'c> fn(
        &'c PromiseArtifact,
        &'c mut DiceComputations,
    ) -> Pin<Box<dyn Future<Output = buck2_error::Result<Artifact>> + Send + 'c>>,
> = LateBinding::new("GET_PROMISED_ARTIFACT");

async fn lookup_deferred_inner(
    key: &BaseDeferredKey,
    dice: &mut DiceComputations<'_>,
) -> buck2_error::Result<DeferredHolder> {
    match key {
        BaseDeferredKey::TargetLabel(target) => {
            let analysis = dice
                .get_analysis_result(target)
                .await?
                .require_compatible()?;

            Ok(DeferredHolder::Analysis(analysis))
        }
        BaseDeferredKey::BxlLabel(bxl) => {
            let bxl_result = BXL_CALCULATION_IMPL
                .get()?
                .eval_bxl(dice, bxl.dupe())
                .await?
                .0;

            Ok(DeferredHolder::Bxl(bxl_result))
        }
        BaseDeferredKey::AnonTarget(target) => Ok(DeferredHolder::Analysis(
            (EVAL_ANON_TARGET.get()?)(dice, target.dupe()).await?,
        )),
    }
}

pub async fn lookup_deferred_holder(
    dice: &mut DiceComputations<'_>,
    key: &DeferredHolderKey,
) -> buck2_error::Result<DeferredHolder> {
    Ok(match key {
        DeferredHolderKey::Base(key) => lookup_deferred_inner(key, dice).await?,
        DeferredHolderKey::DynamicLambda(lambda) => {
            DeferredHolder::DynamicLambda(dynamic_lambda_result(dice, lambda).await?)
        }
    })
}

/// Represents an Analysis or Deferred result. Technically, we can treat analysis as a 'Deferred'
/// and get rid of this enum
pub enum DeferredHolder {
    Analysis(AnalysisResult),
    Bxl(Arc<BxlResult>),
    DynamicLambda(Arc<DynamicLambdaResult>),
}

impl DeferredHolder {
    pub(crate) fn lookup_transitive_set(
        &self,
        key: &TransitiveSetKey,
    ) -> buck2_error::Result<OwnedFrozenValueTyped<FrozenTransitiveSet>> {
        self.analysis_values().lookup_transitive_set(key)
    }

    pub(crate) fn lookup_action(&self, key: &ActionKey) -> buck2_error::Result<ActionLookup> {
        self.analysis_values().lookup_action(key)
    }

    pub fn analysis_values(&self) -> &RecordedAnalysisValues {
        match self {
            DeferredHolder::Analysis(result) => result.analysis_values(),
            DeferredHolder::Bxl(result) => result.analysis_values(),
            DeferredHolder::DynamicLambda(result) => result.analysis_values(),
        }
    }
}

#[derive(Debug, Allocative, Clone, Dupe)]
pub enum ActionLookup {
    Action(Arc<RegisteredAction>),
    Deferred(ActionKey),
}
