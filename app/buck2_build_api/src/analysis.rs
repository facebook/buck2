/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Arc;

use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_interpreter::starlark_profiler::data::StarlarkProfileDataAndStats;

use crate::analysis::registry::RecordedAnalysisValues;
use crate::artifact_groups::promise::PromiseArtifactId;

// TODO(@wendyy) move into `buck2_node`
pub mod anon_promises_dyn;
// TODO(@wendyy) move into `buck2_interpreter_for_build`
pub mod anon_targets_registry;
pub mod calculation;
pub mod extra_v;
pub mod registry;

use allocative::Allocative;
use dupe::Dupe;

use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValueRef;
use crate::validation::transitive_validations::TransitiveValidations;

#[derive(Debug, Clone, Dupe, Allocative)]
pub struct AnalysisResult {
    analysis_values: Arc<RecordedAnalysisValues>,
    /// Profiling data after running analysis, for this analysis only, without dependencies.
    /// `None` when profiling is disabled.
    /// For forward node, this value is shared with underlying analysis (including this field).
    pub profile_data: Option<Arc<StarlarkProfileDataAndStats>>,
    promise_artifact_map: Arc<HashMap<PromiseArtifactId, Artifact>>,
    pub num_declared_actions: u64,
    pub num_declared_artifacts: u64,
    /// `None` means there are no `ValidationInfo` providers in transitive dependencies.
    pub validations: Option<TransitiveValidations>,
}

impl AnalysisResult {
    /// Create a new AnalysisResult
    pub fn new(
        analysis_values: RecordedAnalysisValues,
        profile_data: Option<Arc<StarlarkProfileDataAndStats>>,
        promise_artifact_map: HashMap<PromiseArtifactId, Artifact>,
        num_declared_actions: u64,
        num_declared_artifacts: u64,
        validations: Option<TransitiveValidations>,
    ) -> Self {
        Self {
            analysis_values: Arc::new(analysis_values),
            profile_data,
            promise_artifact_map: Arc::new(promise_artifact_map),
            num_declared_actions,
            num_declared_artifacts,
            validations,
        }
    }

    pub fn providers(&self) -> buck2_error::Result<FrozenProviderCollectionValueRef<'_>> {
        self.analysis_values.provider_collection()
    }

    pub fn promise_artifact_map(&self) -> &Arc<HashMap<PromiseArtifactId, Artifact>> {
        &self.promise_artifact_map
    }

    /// Used to lookup an inner named provider result.
    pub fn lookup_inner(
        &self,
        label: &ConfiguredProvidersLabel,
    ) -> buck2_error::Result<FrozenProviderCollectionValue> {
        Ok(self.providers()?.lookup_inner(label)?.to_owned())
    }

    pub fn analysis_values(&self) -> &RecordedAnalysisValues {
        &self.analysis_values
    }
}
