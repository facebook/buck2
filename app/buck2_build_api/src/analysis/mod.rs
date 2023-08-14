/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::sync::Arc;

use buck2_artifact::deferred::id::DeferredId;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_interpreter::starlark_profiler::StarlarkProfileDataAndStats;

use crate::deferred::types::DeferredLookup;
use crate::deferred::types::DeferredTable;

// TODO(@wendyy) move into `buck2_node`
pub mod anon_promises_dyn;
// TODO(@wendyy) move into `buck2_interpreter_for_build`
pub mod anon_targets_registry;
pub mod calculation;
pub(crate) mod promise_artifacts;
pub mod registry;

use allocative::Allocative;
use dupe::Dupe;

use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;

#[derive(Debug, Clone, Dupe, Allocative)]
pub struct AnalysisResult {
    /// The actual provider collection, validated to be the correct type (`FrozenProviderCollection`)
    pub provider_collection: FrozenProviderCollectionValue,
    deferred: DeferredTable,
    pub profile_data: Option<Arc<StarlarkProfileDataAndStats>>,
}

impl AnalysisResult {
    /// Create a new AnalysisResult
    pub fn new(
        provider_collection: FrozenProviderCollectionValue,
        deferred: DeferredTable,
        profile_data: Option<Arc<StarlarkProfileDataAndStats>>,
    ) -> Self {
        Self {
            provider_collection,
            deferred,
            profile_data,
        }
    }

    pub fn providers(&self) -> &FrozenProviderCollectionValue {
        &self.provider_collection
    }

    /// Used to lookup an inner named provider result.
    pub fn lookup_inner(
        &self,
        label: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<FrozenProviderCollectionValue> {
        self.provider_collection.lookup_inner(label)
    }

    pub fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<DeferredLookup<'_>> {
        self.deferred.lookup_deferred(id)
    }

    pub fn iter_deferreds(&self) -> impl Iterator<Item = DeferredLookup<'_>> {
        self.deferred.iter()
    }

    pub fn testing_deferred(&self) -> &DeferredTable {
        &self.deferred
    }
}
