/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use anyhow::Context;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_artifact::deferred::key::DeferredKey;
use buck2_build_api::actions::key::ActionKeyExt;
use buck2_build_api::analysis::registry::AnalysisValueFetcher;
use buck2_build_api::deferred::types::DeferredRegistry;
use buck2_build_api::deferred::types::ReservedDeferredData;
use buck2_build_api::dynamic::DynamicRegistryDyn;
use buck2_build_api::dynamic::DYNAMIC_REGISTRY_NEW;
use buck2_build_api::interpreter::rule_defs::dynamic_value::DynamicValue;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_error::BuckErrorContext;
use dupe::Dupe;
use indexmap::IndexSet;

use crate::dynamic::deferred::DynamicAction;
use crate::dynamic::deferred::DynamicLambda;
use crate::dynamic::deferred::DynamicLambdaOutput;

#[derive(Allocative)]
pub(crate) struct DynamicRegistry {
    owner: BaseDeferredKey,
    pending: Vec<(ReservedDeferredData<DynamicLambdaOutput>, DynamicLambda)>,
}

pub(crate) fn init_dynamic_registry_new() {
    DYNAMIC_REGISTRY_NEW.init(|owner| {
        Box::new(DynamicRegistry {
            owner,
            pending: Vec::new(),
        })
    });
}

impl DynamicRegistryDyn for DynamicRegistry {
    fn register(
        &mut self,
        dynamic: IndexSet<Artifact>,
        promises: IndexSet<DynamicValue>,
        outputs: IndexSet<OutputArtifact>,
        registry: &mut DeferredRegistry,
    ) -> anyhow::Result<DeferredKey> {
        let reserved = registry.reserve::<DynamicLambdaOutput>();
        let outputs = outputs
            .iter()
            .enumerate()
            .map(|(output_artifact_index, output)| {
                let output_id =
                    registry.defer(DynamicAction::new(reserved.data(), output_artifact_index));
                let bound = output
                    .bind(ActionKey::new(output_id))?
                    .as_base_artifact()
                    .dupe();
                Ok(bound)
            })
            .collect::<anyhow::Result<_>>()?;
        let lambda = DynamicLambda::new(self.owner.dupe(), dynamic, promises, outputs);
        let lambda_key = reserved.data().deferred_key().dupe();
        self.pending.push((reserved, lambda));
        Ok(lambda_key)
    }

    fn ensure_bound(
        self: Box<Self>,
        registry: &mut DeferredRegistry,
        analysis_value_fetcher: &AnalysisValueFetcher,
    ) -> anyhow::Result<()> {
        for (key, mut data) in self.pending {
            let id = key.data().deferred_key().id();

            let fv = analysis_value_fetcher
                .get(id)?
                .with_context(|| format!("Key is missing in AnalysisValueFetcher: {:?}", id))?
                .downcast_anyhow()
                .internal_error("Incorrect type")?;

            data.bind(fv)?;
            registry.bind(key, data);
        }
        Ok(())
    }
}
