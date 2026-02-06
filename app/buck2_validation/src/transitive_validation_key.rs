/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;
use std::collections::BTreeSet;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::validation::transitive_validations::TransitiveValidations;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use derivative::Derivative;
use derive_more::Display;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dice_error::DiceError;
use dupe::Dupe;
use dupe::IterDupedExt;
use either::Either;
use futures::future::FutureExt;

use crate::cached_validation_result::CachedValidationResult;
use crate::cached_validation_result::CachedValidationResultData;
use crate::cached_validation_result::ValidationFailedUserFacingError;
use crate::enabled_optional_validations_key::EnabledOptionalValidationsKey;
use crate::single_validation_key::SingleValidationKey;

/// DICE key that corresponds to a validation of a whole target subgraph rooted at the given node.
#[derive(
    Clone, Display, Dupe, Allocative, Derivative, Hash, Eq, PartialEq, Debug
)]
#[repr(transparent)]
pub(crate) struct TransitiveValidationKey(pub ConfiguredTargetLabel);

impl TransitiveValidationKey {
    /// Only performs validations that are described in `ValidationInfo` for the current node
    async fn validate_current_node(
        &self,
        ctx: &mut DiceComputations<'_>,
        transitive_validations: TransitiveValidations,
    ) -> Result<(), TreatValidationFailureAsError> {
        let info = match &transitive_validations.0.info {
            Some(info) => info,
            None => return Ok(()),
        };

        let enabled_optional_validations = if info.validations().any(|spec| spec.optional()) {
            Either::Left(ctx.compute(&EnabledOptionalValidationsKey).await?)
        } else {
            Either::Right(Cow::Owned(BTreeSet::new()))
        };

        let enabled_optional_validations: &BTreeSet<String> =
            AsRef::as_ref(&enabled_optional_validations);

        let artifacts = info
            .validations()
            .filter(|spec| !spec.optional() || enabled_optional_validations.contains(spec.name()))
            .map(|spec| spec.validation_result().get_bound_artifact())
            .map(|r| r.map_err(buck2_error::Error::from))
            .collect::<buck2_error::Result<Vec<Artifact>>>()?;
        ctx.try_compute_join(artifacts, |ctx, output| {
            async move { compute_single_validation(ctx, output).await }.boxed()
        })
        .await
        .map(|_| ())
    }

    async fn validate_children(
        &self,
        ctx: &mut DiceComputations<'_>,
        transitive_validations: TransitiveValidations,
    ) -> Result<(), TreatValidationFailureAsError> {
        ctx.try_compute_join(
            transitive_validations.0.children.iter().duped(),
            |ctx, label| {
                let key = TransitiveValidationKey(label);
                async move {
                    let result = ctx.compute(&key).await?;
                    tighten_cached_validation_result(result)
                }
                .boxed()
            },
        )
        .await
        .map(|_| ())
    }
}

#[async_trait]
impl Key for TransitiveValidationKey {
    type Value = buck2_error::Result<CachedValidationResult>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let transitive_validations = ctx
            .get_validations(&self.0)
            .await?
            .require_compatible()
            .internal_error("Incompatible node is not expected")?;
        let transitive_validations = match transitive_validations {
            Some(x) => x,
            // Means there are no transitive `ValidationInfo` providers, validation is nop.
            None => {
                return Ok(CachedValidationResult(Arc::new(
                    CachedValidationResultData::Success,
                )));
            }
        };
        let result = ctx
            .try_compute2(
                {
                    let transitive_validations = transitive_validations.dupe();
                    move |ctx| {
                        self.validate_current_node(ctx, transitive_validations)
                            .boxed()
                    }
                },
                move |ctx| self.validate_children(ctx, transitive_validations).boxed(),
            )
            .await;
        match result {
            Ok(_) => Ok(CachedValidationResult(Arc::new(
                CachedValidationResultData::Success,
            ))),
            Err(TreatValidationFailureAsError::Transient(e)) => Err(e),
            Err(TreatValidationFailureAsError::ValidationFailed(e)) => Ok(CachedValidationResult(
                Arc::new(CachedValidationResultData::Failure(e)),
            )),
        }
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }

    fn validity(x: &Self::Value) -> bool {
        x.is_ok()
    }
}

/// Auxiliary error type to be able to stop running validations early during transitive check
/// when we first encounter a validation failure. We do not treat validation failure
/// as error when doing single node check in order to be able to cache the result in DICE.
enum TreatValidationFailureAsError {
    ValidationFailed(ValidationFailedUserFacingError),
    Transient(buck2_error::Error),
}

impl From<buck2_error::Error> for TreatValidationFailureAsError {
    fn from(value: buck2_error::Error) -> Self {
        TreatValidationFailureAsError::Transient(value)
    }
}

impl From<DiceError> for TreatValidationFailureAsError {
    fn from(value: DiceError) -> Self {
        TreatValidationFailureAsError::Transient(buck2_error::Error::from(value))
    }
}

impl From<ValidationFailedUserFacingError> for TreatValidationFailureAsError {
    fn from(value: ValidationFailedUserFacingError) -> Self {
        TreatValidationFailureAsError::ValidationFailed(value)
    }
}

async fn compute_single_validation(
    ctx: &mut DiceComputations<'_>,
    validation_result: Artifact,
) -> Result<(), TreatValidationFailureAsError> {
    let action_key = validation_result
        .action_key()
        .ok_or_else(|| internal_error!("Expected validation to be a build artifact"))?;
    let key = SingleValidationKey(action_key.dupe());
    let result = ctx.compute(&key).await?;
    tighten_cached_validation_result(result)
}

fn tighten_cached_validation_result(
    result: buck2_error::Result<CachedValidationResult>,
) -> Result<(), TreatValidationFailureAsError> {
    match result {
        Ok(result) => match result.0.as_ref() {
            CachedValidationResultData::Success => Ok(()),
            CachedValidationResultData::Failure(user_facing_error) => {
                Err(user_facing_error.clone().into())
            }
        },
        Err(e) => Err(e.into()),
    }
}
