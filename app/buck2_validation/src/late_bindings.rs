/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Once;

use async_trait::async_trait;
use buck2_build_api::materialize::MaterializationContext;
use buck2_build_api::validation::validation_impl::ValidationImpl;
use buck2_build_api::validation::validation_impl::VALIDATION_IMPL;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use dice::LinearRecomputeDiceComputations;

pub(crate) fn init() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        VALIDATION_IMPL.init(&ValidationImplInstance);
    });
}

struct ValidationImplInstance;

#[async_trait]
impl ValidationImpl for ValidationImplInstance {
    async fn validate_target_node_transitively<'a>(
        &self,
        _ctx: &'a LinearRecomputeDiceComputations<'_>,
        _materialization_context: &'a MaterializationContext,
        _target_node: ConfiguredTargetNode,
    ) -> Result<(), buck2_error::Error> {
        Ok(())
    }
}
