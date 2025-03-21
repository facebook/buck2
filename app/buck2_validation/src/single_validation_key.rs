/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::actions::key::ActionKey;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::actions::calculation::ActionCalculation;
use buck2_core::fs::async_fs_util;
use buck2_error::BuckErrorContext;
use buck2_execute::materialize::materializer::HasMaterializer;
use derive_more::Display;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;

use crate::cached_validation_result::CachedValidationResult;
use crate::validator_api::parse_validation_result;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Tier0)]
enum ParseValidationResultError {
    #[error("Validation result should produce exactly one artifact")]
    WrongNumberOfArtifacts,
}

/// DICE key that corresponds to a single validation, represented by a ValidationSpec object.
/// Computation is the whole process of:
/// 1) Building the validation result artifact.
/// 2) Materializing it.
/// 3) Reading and parsing it to produce result which could be cached in DICE.
#[derive(Clone, Display, Dupe, Debug, Eq, PartialEq, Hash, Allocative)]
#[repr(transparent)]
pub(crate) struct SingleValidationKey(pub ActionKey);

#[async_trait]
impl Key for SingleValidationKey {
    type Value = buck2_error::Result<CachedValidationResult>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let gen_path = {
            let build_result = ActionCalculation::build_action(ctx, &self.0).await?;
            if build_result.iter().count() != 1 {
                return Err(buck2_error::Error::from(
                    ParseValidationResultError::WrongNumberOfArtifacts,
                ));
            }
            let (gen_path, ..) = build_result
                .iter()
                .next()
                .internal_error("Just checked single element")?;
            gen_path.dupe()
        };

        let fs = ctx.get_artifact_fs().await?;
        let project_relative_path = fs.buck_out_path_resolver().resolve_gen(&gen_path);

        let validation_result_path = fs.fs().resolve(&project_relative_path);

        // Make sure validation result is materialized before we parse it
        ctx.per_transaction_data()
            .get_materializer()
            .ensure_materialized(vec![project_relative_path])
            .await?;

        let content = async_fs_util::read_to_string(&validation_result_path)
            .await
            .buck_error_context("Reading validation result")?;

        match parse_validation_result(&content) {
            Ok(r) => Ok(CachedValidationResult::new(
                r,
                self.0.owner().dupe(),
                validation_result_path,
            )),
            Err(e) => Err(buck2_error::Error::from(e)),
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
