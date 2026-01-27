/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::fmt::Display;
use std::sync::Arc;

use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_interpreter::dice::starlark_provider::StarlarkEvalKind;
use buck2_node::rule_type::StarlarkRuleType;
use dupe::Dupe;
use starlark::collections::SmallMap;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::Value;
use starlark::values::ValueOfUncheckedGeneric;
use starlark::values::structs::StructRef;

use crate::analysis::AnalysisResult;
use crate::artifact_groups::promise::PromiseArtifactAttr;
use crate::artifact_groups::promise::PromiseArtifactId;
use crate::validation::transitive_validations::TransitiveValidations;

pub trait AnonTargetDyn: Send + Sync + Display {
    fn eval_kind(self: Arc<Self>) -> StarlarkEvalKind;

    fn rule_type(&self) -> &Arc<StarlarkRuleType>;

    fn base_deferred_key(self: Arc<Self>) -> BaseDeferredKey;

    fn get_fulfilled_promise_artifacts<'v>(
        self: Arc<Self>,
        promise_artifact_mappings: SmallMap<String, Value<'v>>,
        anon_target_result: Value<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> buck2_error::Result<HashMap<PromiseArtifactId, Artifact>>;

    fn resolve_attrs<'v>(
        &self,
        env: &Module<'v>,
        dependents_analyses: AnonTargetDependentAnalysisResults<'_>,
        exec_resolution: ExecutionPlatformResolution,
    ) -> buck2_error::Result<ValueOfUncheckedGeneric<Value<'v>, StructRef<'static>>>;
}

// Container for analysis results of the anon target dependents.
pub struct AnonTargetDependentAnalysisResults<'v> {
    pub dep_analysis_results: Vec<(&'v ConfiguredTargetLabel, AnalysisResult)>,
    pub promised_artifacts: HashMap<&'v PromiseArtifactAttr, Artifact>,
}

impl<'v> AnonTargetDependentAnalysisResults<'v> {
    pub fn validations(&self) -> SmallMap<ConfiguredTargetLabel, TransitiveValidations> {
        self.dep_analysis_results
            .iter()
            .filter_map(|(label, analysis_result)| {
                analysis_result
                    .validations
                    .dupe()
                    .map(|v| ((*label).dupe(), v))
            })
            .collect::<SmallMap<_, _>>()
    }
}
