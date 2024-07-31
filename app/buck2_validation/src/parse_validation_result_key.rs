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
use anyhow::Context;
use async_trait::async_trait;
use buck2_artifact::actions::key::ActionKey;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::actions::calculation::ActionCalculation;
use buck2_core::fs::buck_out_path::BuckOutPath;
use buck2_core::fs::fs_util;
use buck2_error::BuckErrorContext;
use derive_more::Display;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;

use crate::validator_api::parse_validation_result;
use crate::validator_api::ValidationResult;
use crate::validator_api::ValidationStatus;

#[derive(Debug, buck2_error::Error)]
enum ParseValidationResultError {
    #[error("Validation result should produce exactly one artifact")]
    WrongNumberOfArtifacts,
}

#[derive(Clone, Dupe, Allocative, PartialEq)]
pub(crate) struct CachedValidationResult(pub Arc<CachedValidationResultData>);

#[derive(Allocative, PartialEq)]
pub(crate) enum CachedValidationResultData {
    Success,
    Failure {
        /// Shortened message as we don't want to store too much data in DICE
        short_message: String,
        validation_result_path: BuckOutPath,
    },
}

/// We only want to parse again when the validation action changes, since
/// parsing result is deterministic and only depends on validation action output.
/// That's why parsing key is just a simple wrapper around validation action key.
#[derive(Clone, Display, Dupe, Debug, Eq, PartialEq, Hash, Allocative)]
#[repr(transparent)]
pub(crate) struct ParseValidationResultKey(pub ActionKey);

#[async_trait]
impl Key for ParseValidationResultKey {
    type Value = buck2_error::Result<CachedValidationResult>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let gen_path = {
            let build_result = ActionCalculation::build_action(ctx, &self.0).await?;
            if build_result.iter().count() != 1 {
                return Err(buck2_error::Error::new(
                    ParseValidationResultError::WrongNumberOfArtifacts,
                ));
            }
            let (gen_path, ..) = build_result
                .iter()
                .next()
                .internal_error("Just checked single element")?;
            gen_path.dupe()
        };

        let validation_result_path = {
            let fs = ctx.get_artifact_fs().await?;
            let project_relative_path = fs.buck_out_path_resolver().resolve_gen(&gen_path);
            fs.fs().resolve(&project_relative_path)
        };

        let content = fs_util::read_to_string(&validation_result_path)
            .context("Reading validation result")?;

        match parse_validation_result(&content) {
            Ok(r) => Ok(cached_validation_result(r, gen_path)),
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

/// Converts [`ValidationResult`] into [`CachedValidationResult`]
fn cached_validation_result(
    parsed_result: ValidationResult,
    validation_result_path: BuckOutPath,
) -> CachedValidationResult {
    let data = match parsed_result {
        ValidationResult {
            status: ValidationStatus::Success,
            ..
        } => CachedValidationResultData::Success,
        ValidationResult {
            status: ValidationStatus::Failure,
            message,
        } => {
            const MAX_CACHED_LENGTH: usize = 600;
            let short_message = shorten_message(&message.unwrap_or_default(), MAX_CACHED_LENGTH);
            CachedValidationResultData::Failure {
                short_message,
                validation_result_path,
            }
        }
    };
    CachedValidationResult(Arc::new(data))
}

/// If original message exceeds the limit, cut it and add an ellipsis. Also quote it.
fn shorten_message(message: &str, max_chars: usize) -> String {
    if message.chars().count() > max_chars {
        let max_len = message.chars().map(|c| c.len_utf8()).take(max_chars).sum();
        format!("\"{}...\"", message[..max_len].to_owned())
    } else {
        format!("\"{}\"", message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shorten_message() {
        assert_eq!(shorten_message("Hello World", 5), r#""Hello...""#);
        assert_eq!(shorten_message("Hello World", 11), r#""Hello World""#);
        assert_eq!(shorten_message("Hello World", 100), r#""Hello World""#);
        assert_eq!(shorten_message("Привет, мир", 6), r#""Привет...""#);
        assert_eq!(shorten_message("Привет, мир", 11), r#""Привет, мир""#);
        assert_eq!(shorten_message("你好世界", 2), r#""你好...""#);
        assert_eq!(shorten_message("你好世界", 4), r#""你好世界""#);
    }
}
