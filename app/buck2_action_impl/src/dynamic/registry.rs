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
use buck2_artifact::actions::key::ActionIndex;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_artifact::deferred::key::DeferredHolderKey;
use buck2_artifact::dynamic::DynamicLambdaIndex;
use buck2_artifact::dynamic::DynamicLambdaResultsKey;
use buck2_build_api::analysis::registry::AnalysisValueFetcher;
use buck2_build_api::analysis::registry::AnalysisValueStorage;
use buck2_build_api::deferred::types::DeferredRegistry;
use buck2_build_api::dynamic::lambda::DynamicLambda;
use buck2_build_api::dynamic::params::DynamicLambdaParams;
use buck2_build_api::dynamic::registry::DynamicRegistryDyn;
use buck2_build_api::dynamic::registry::DYNAMIC_REGISTRY_NEW;
use buck2_core::base_deferred_key::BaseDeferredKey;
use dupe::Dupe;
use indexmap::IndexSet;
use starlark::collections::SmallMap;
use starlark::values::any_complex::StarlarkAnyComplex;
use starlark::values::ValueTyped;

#[derive(Allocative)]
pub(crate) struct DynamicRegistry {
    owner: BaseDeferredKey,
    pending: SmallMap<DynamicLambdaResultsKey, DynamicLambda>,
}

pub(crate) fn init_dynamic_registry_new() {
    DYNAMIC_REGISTRY_NEW.init(|owner| {
        Box::new(DynamicRegistry {
            owner,
            pending: SmallMap::new(),
        })
    });
}

impl DynamicRegistryDyn for DynamicRegistry {
    fn register<'v>(
        &mut self,
        dynamic: IndexSet<Artifact>,
        outputs: IndexSet<OutputArtifact>,
        lambda_params: ValueTyped<'v, StarlarkAnyComplex<DynamicLambdaParams<'v>>>,
        registry: &mut DeferredRegistry,
        storage: &mut AnalysisValueStorage<'v>,
    ) -> anyhow::Result<()> {
        let key = DynamicLambdaResultsKey::new(
            registry.key().dupe(),
            DynamicLambdaIndex::new(self.pending.len().try_into()?),
        );

        let outputs = outputs
            .iter()
            .enumerate()
            .map(|(output_artifact_index, output)| {
                // We create ActionKeys that point directly to the dynamic_lambda's
                // output rather than our own. This saves the resolution of the key from
                // needing to first lookup our result just to get forwarded to the lambda's result.
                //
                // This means that we are creating ActionKeys for the lambda and it needs to offset
                // its key's index to account for this (see ActionRegistry where this is done).
                //
                // TODO(cjhopman): We should probably combine ActionRegistry and DynamicRegistry (and
                // probably ArtifactGroupRegistry too).
                let bound = output
                    .bind(ActionKey::new(
                        DeferredHolderKey::DynamicLambda(Arc::new(key.dupe())),
                        ActionIndex::new(output_artifact_index.try_into()?),
                    ))?
                    .as_base_artifact()
                    .dupe();
                Ok(bound)
            })
            .collect::<anyhow::Result<_>>()?;
        let lambda = DynamicLambda::new(self.owner.dupe(), dynamic, outputs);
        self.pending.insert(key.dupe(), lambda);
        storage.set_lambda_params(key, lambda_params);
        Ok(())
    }

    fn ensure_bound(
        self: Box<Self>,
        _registry: &mut DeferredRegistry,
        analysis_value_fetcher: &AnalysisValueFetcher,
    ) -> anyhow::Result<SmallMap<DynamicLambdaResultsKey, Arc<DynamicLambda>>> {
        let mut result = SmallMap::with_capacity(self.pending.len());
        for (key, mut data) in self.pending.into_iter_hashed() {
            let fv = analysis_value_fetcher
                .get_lambda_params(&key)?
                .with_context(|| {
                    format!(
                        "DynamicLambda params are missing in AnalysisValueFetcher: {:?}",
                        &key
                    )
                })?;

            data.bind(fv)?;
            result.insert_hashed(key, Arc::new(data));
        }
        Ok(result)
    }
}
