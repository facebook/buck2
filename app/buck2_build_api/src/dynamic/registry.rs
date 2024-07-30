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
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_artifact::dynamic::DynamicLambdaResultsKey;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_util::late_binding::LateBinding;
use indexmap::IndexSet;
use starlark::collections::SmallMap;
use starlark::values::any_complex::StarlarkAnyComplex;
use starlark::values::ValueTyped;

use crate::analysis::registry::AnalysisValueFetcher;
use crate::analysis::registry::AnalysisValueStorage;
use crate::deferred::types::DeferredRegistry;
use crate::dynamic::lambda::DynamicLambda;
use crate::dynamic::params::DynamicLambdaParams;

pub trait DynamicRegistryDyn: Allocative + 'static {
    fn register<'v>(
        &mut self,
        dynamic: IndexSet<Artifact>,
        outputs: IndexSet<OutputArtifact>,
        lambda_params: ValueTyped<'v, StarlarkAnyComplex<DynamicLambdaParams<'v>>>,
        registry: &mut DeferredRegistry,
        storage: &mut AnalysisValueStorage<'v>,
    ) -> anyhow::Result<()>;

    fn ensure_bound(
        self: Box<Self>,
        registry: &mut DeferredRegistry,
        analysis_value_fetcher: &AnalysisValueFetcher,
    ) -> anyhow::Result<SmallMap<DynamicLambdaResultsKey, Arc<DynamicLambda>>>;
}

pub static DYNAMIC_REGISTRY_NEW: LateBinding<fn(BaseDeferredKey) -> Box<dyn DynamicRegistryDyn>> =
    LateBinding::new("DYNAMIC_REGISTRY_NEW");
