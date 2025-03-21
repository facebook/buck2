/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use dupe::Dupe;
use starlark::eval::Evaluator;
use starlark::values::FrozenValueTyped;
use starlark::values::Value;
use starlark::values::ValueTyped;

use crate::analysis::registry::AnalysisValueStorage;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSetDefinition;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;

#[derive(Allocative)]
pub struct ArtifactGroupRegistry;

impl ArtifactGroupRegistry {
    pub fn new() -> Self {
        Self
    }

    pub(crate) fn create_transitive_set<'v>(
        &mut self,
        definition: FrozenValueTyped<'v, FrozenTransitiveSetDefinition>,
        value: Option<Value<'v>>,
        children: Option<Value<'v>>,
        analysis_value_storage: &mut AnalysisValueStorage<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, TransitiveSet<'v>>> {
        Ok(analysis_value_storage.register_transitive_set(move |key| {
            let set =
                TransitiveSet::new_from_values(key.dupe(), definition, value, children, eval)?;
            Ok(eval.heap().alloc_typed(set))
        })?)
    }
}
