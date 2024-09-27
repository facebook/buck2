/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_build_api::validation::validation_impl::ValidationImpl;
use buck2_build_api::validation::validation_impl::VALIDATION_IMPL;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use dice::DiceComputations;

use crate::cached_validation_result::CachedValidationResultData;
use crate::transitive_validation_key::TransitiveValidationKey;

pub(crate) fn init_validation_impl() {
    VALIDATION_IMPL.init(&ValidationImplInstance);
}

struct ValidationImplInstance;

#[async_trait]
impl ValidationImpl for ValidationImplInstance {
    async fn validate_target_node_transitively(
        &self,
        ctx: &mut DiceComputations<'_>,
        target: ConfiguredTargetLabel,
    ) -> Result<(), buck2_error::Error> {
        let key = TransitiveValidationKey(target);
        let result = ctx.compute(&key).await??;
        match result.0.as_ref() {
            CachedValidationResultData::Success => Ok(()),
            CachedValidationResultData::Failure(e) => Err(buck2_error::Error::from(e.clone())),
        }
    }
}
