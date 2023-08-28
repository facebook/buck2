/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::context::ANALYSIS_ACTIONS_METHODS_ANON_TARGET;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use buck2_interpreter_for_build::rule::FrozenRuleCallable;
use starlark::environment::MethodsBuilder;
use starlark::starlark_module;
use starlark::values::dict::DictOf;
use starlark::values::Heap;
use starlark::values::Value;
use starlark::values::ValueTyped;

use crate::anon_targets::AnonTargetsRegistry;

#[starlark_module]
fn analysis_actions_methods_anon_target(builder: &mut MethodsBuilder) {
    /// An anonymous target is defined by the hash of its attributes, rather than its name.
    /// During analysis, rules can define and access the providers of anonymous targets before producing their own providers.
    /// Two distinct rules might ask for the same anonymous target, sharing the work it performs.
    ///
    /// For more details see https://buck2.build/docs/rule_authors/anon_targets/
    fn anon_target<'v>(
        this: &AnalysisActions<'v>,
        rule: ValueTyped<'v, FrozenRuleCallable>,
        attrs: DictOf<'v, &'v str, Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkPromise<'v>>> {
        let res = heap.alloc_typed(StarlarkPromise::new_unresolved());
        let mut this = this.state();
        let registry = AnonTargetsRegistry::downcast_mut(&mut *this.anon_targets)?;
        let key = registry.anon_target_key(rule, attrs)?;
        registry.register_one(res, key)?;
        Ok(res)
    }

    /// Generate a series of anonymous targets
    fn anon_targets<'v>(
        this: &AnalysisActions<'v>,
        rules: Vec<(
            ValueTyped<'v, FrozenRuleCallable>,
            DictOf<'v, &'v str, Value<'v>>,
        )>,
        heap: &'v Heap,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkPromise<'v>>> {
        let res = heap.alloc_typed(StarlarkPromise::new_unresolved());
        let mut this = this.state();
        AnonTargetsRegistry::downcast_mut(&mut *this.anon_targets)?.register_many(res, rules)?;
        Ok(res)
    }
}

pub(crate) fn init_analysis_actions_methods_anon_target() {
    ANALYSIS_ACTIONS_METHODS_ANON_TARGET.init(analysis_actions_methods_anon_target);
}
