/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_artifact::deferred::id::DeferredId;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_util::late_binding::LateBinding;
use indexmap::IndexSet;

use crate::analysis::registry::AnalysisValueFetcher;
use crate::deferred::types::DeferredRegistry;

pub trait DynamicRegistryDyn: Allocative + 'static {
    fn register(
        &mut self,
        dynamic: IndexSet<Artifact>,
        outputs: IndexSet<OutputArtifact>,
        registry: &mut DeferredRegistry,
    ) -> anyhow::Result<DeferredId>;

    fn ensure_bound(
        self: Box<Self>,
        registry: &mut DeferredRegistry,
        analysis_value_fetcher: &AnalysisValueFetcher,
    ) -> anyhow::Result<()>;
}

pub static DYNAMIC_REGISTRY_NEW: LateBinding<fn(BaseDeferredKey) -> Box<dyn DynamicRegistryDyn>> =
    LateBinding::new("DYNAMIC_REGISTRY_NEW");
