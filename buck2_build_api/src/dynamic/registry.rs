/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_execute::base_deferred_key::BaseDeferredKey;
use gazebo::prelude::*;
use indexmap::IndexSet;

use crate::actions::artifact::Artifact;
use crate::actions::artifact::OutputArtifact;
use crate::actions::key::ActionKey;
use crate::analysis::registry::AnalysisValueFetcher;
use crate::deferred::types::DeferredId;
use crate::deferred::types::DeferredRegistry;
use crate::deferred::types::ReservedDeferredData;
use crate::dynamic::deferred::DynamicAction;
use crate::dynamic::deferred::DynamicLambda;
use crate::dynamic::deferred::DynamicLambdaOutput;

pub(crate) struct DynamicRegistry {
    owner: BaseDeferredKey,
    pending: Vec<(ReservedDeferredData<DynamicLambdaOutput>, DynamicLambda)>,
}

impl DynamicRegistry {
    pub fn new(owner: BaseDeferredKey) -> Self {
        Self {
            owner,
            pending: Vec::new(),
        }
    }

    pub fn register(
        &mut self,
        dynamic: IndexSet<Artifact>,
        inputs: IndexSet<Artifact>,
        outputs: IndexSet<OutputArtifact>,
        registry: &mut DeferredRegistry,
    ) -> anyhow::Result<DeferredId> {
        let reserved = registry.reserve::<DynamicLambdaOutput>();
        let outputs = outputs
            .iter()
            .enumerate()
            .map(|(i, output)| {
                let output_id = registry.defer(DynamicAction::new(reserved.data(), i));
                let bound = output
                    .bind(ActionKey::new(output_id))?
                    .as_base_artifact()
                    .dupe();
                Ok(bound)
            })
            .collect::<anyhow::Result<_>>()?;
        let lambda = DynamicLambda::new(self.owner.dupe(), dynamic, inputs, outputs);
        let lambda_id = reserved.data().deferred_key().id();
        self.pending.push((reserved, lambda));
        Ok(lambda_id)
    }

    pub fn ensure_bound(
        self,
        registry: &mut DeferredRegistry,
        analysis_value_fetcher: &AnalysisValueFetcher,
    ) -> anyhow::Result<()> {
        for (key, mut data) in self.pending {
            let id = key.data().deferred_key().id();

            let fv = analysis_value_fetcher
                .get(id)
                .with_context(|| format!("Key is missing in AnalysisValueFetcher: {:?}", id))?;

            data.bind(fv);
            registry.bind(key, data);
        }
        Ok(())
    }
}
