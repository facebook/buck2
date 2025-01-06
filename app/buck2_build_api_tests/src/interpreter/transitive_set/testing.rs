/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

use anyhow::Context as _;
use buck2_build_api::artifact_groups::deferred::TransitiveSetIndex;
use buck2_build_api::artifact_groups::deferred::TransitiveSetKey;
use buck2_build_api::interpreter::rule_defs::transitive_set::transitive_set_definition::register_transitive_set;
use buck2_build_api::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;
use buck2_build_api::interpreter::rule_defs::transitive_set::FrozenTransitiveSetDefinition;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSet;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSetOrdering;
use buck2_core::deferred::key::DeferredHolderKey;
use indoc::indoc;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::FreezeErrorContext;
use starlark::values::FrozenValueTyped;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::Value;
use starlark::StarlarkResultExt;

use crate::interpreter::rule_defs::artifact::testing::artifactory;

#[starlark_module]
pub(crate) fn tset_factory(builder: &mut GlobalsBuilder) {
    fn make_tset<'v>(
        definition: FrozenValueTyped<'v, FrozenTransitiveSetDefinition>,
        value: Option<Value<'v>>,
        children: Option<Value<'v>>, // An iterable.
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<TransitiveSet<'v>> {
        static LAST_ID: AtomicU32 = AtomicU32::new(0);

        let tset_id = TransitiveSetIndex::testing_new(LAST_ID.fetch_add(1, Ordering::Relaxed));

        let set = TransitiveSet::new_from_values(
            TransitiveSetKey::new(DeferredHolderKey::testing_new("cell//more:tsets"), tset_id),
            definition,
            value,
            children,
            eval,
        )?;

        Ok(set)
    }
}

pub(crate) fn new_transitive_set(
    code: &str,
) -> anyhow::Result<OwnedFrozenValueTyped<FrozenTransitiveSet>> {
    let env = Module::new();

    let globals = GlobalsBuilder::standard()
        .with(register_transitive_set)
        .with(tset_factory)
        .with(artifactory)
        .build();

    buck2_interpreter_for_build::attrs::coerce::testing::to_value(&env, &globals, code);

    let frozen = env.freeze().freeze_error_context("Freeze failed")?;

    let make = frozen.get("make").context("`make` was not found")?;

    let env2 = Module::new();
    let ret = Evaluator::new(&env2)
        .eval_function(make.owned_value(&env2.frozen_heap()), &[], &[])
        .into_anyhow_result()?;

    env2.set_extra_value(ret);

    let frozen = env2.freeze()?;

    Ok(frozen
        .owned_extra_value()
        .context("Frozen value must be in extra value")?
        .downcast_starlark()
        .map_err(buck2_error::Error::from)?)
}

#[test]
fn test_new_transitive_set() -> anyhow::Result<()> {
    let set = new_transitive_set(indoc!(
        r#"
        FooSet = transitive_set()

        def make():
            s1 = make_tset(FooSet, value = "foo")
            return make_tset(FooSet, value = "bar", children = [s1])
        "#
    ))?;

    assert_eq!(
        set.as_ref().iter(TransitiveSetOrdering::Preorder).count(),
        2
    );

    Ok(())
}
