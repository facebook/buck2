/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use anyhow::Context as _;
use gazebo::prelude::*;
use starlark::eval::Evaluator;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::analysis::registry::AnalysisValueFetcher;
use crate::artifact_groups::deferred::DeferredTransitiveSetData;
use crate::deferred::types::DeferredRegistry;
use crate::deferred::types::ReservedTrivialDeferredData;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;

#[derive(Allocative)]
pub struct ArtifactGroupRegistry {
    pending: Vec<ReservedTrivialDeferredData<DeferredTransitiveSetData>>,
}

impl ArtifactGroupRegistry {
    pub fn new() -> Self {
        Self {
            pending: Vec::new(),
        }
    }

    pub fn create_transitive_set<'v>(
        &mut self,
        definition: Value<'v>,
        value: Option<Value<'v>>,
        children: Option<Value<'v>>,
        deferred: &mut DeferredRegistry,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<TransitiveSet<'v>> {
        let reserved = deferred.reserve_trivial::<DeferredTransitiveSetData>();
        let set = TransitiveSet::new_from_values(
            reserved.data().dupe(),
            definition,
            value,
            children,
            eval,
        )?;
        self.pending.push(reserved);
        Ok(set)
    }

    pub(crate) fn ensure_bound(
        self,
        registry: &mut DeferredRegistry,
        analysis_value_fetcher: &AnalysisValueFetcher,
    ) -> anyhow::Result<()> {
        for key in self.pending {
            let id = key.data().deferred_key().id();

            let set = analysis_value_fetcher
                .get(id)?
                .with_context(|| format!("Key is missing in AnalysisValueFetcher: {:?}", id))?;

            if set.value().downcast_ref::<FrozenTransitiveSet>().is_some() {
                registry.bind_trivial(key, DeferredTransitiveSetData(set));
                continue;
            }

            return Err(anyhow::anyhow!(
                "ArtifactGroupRegistry::ensure_bound found a value that was not a FrozenTransitiveSet: {:?}",
                set
            ));
        }

        Ok(())
    }
}
