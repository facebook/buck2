/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Mutex;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

use buck2_build_api::artifact_groups::deferred::TransitiveSetIndex;
use buck2_build_api::artifact_groups::deferred::TransitiveSetKey;
use buck2_build_api::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;
use buck2_build_api::interpreter::rule_defs::transitive_set::FrozenTransitiveSetDefinition;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSet;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSetOrdering;
use buck2_build_api::interpreter::rule_defs::transitive_set::transitive_set_definition::register_transitive_set;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_error::BuckErrorContext as _;
use buck2_interpreter::from_freeze::from_freeze_error;
use indoc::indoc;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::FreezeErrorContext;
use starlark::values::FrozenValueTyped;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::Value;

use crate::interpreter::rule_defs::artifact::testing::artifactory;

/// Global mutex to serialize tests that use `make_tset()`, which increments a shared
/// global counter (LAST_ID). Without serialization, parallel test execution causes
/// non-deterministic TransitiveSetIndex assignment, leading to intermittent failures.
pub static TSET_TEST_LOCK: Mutex<()> = Mutex::new(());

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
) -> buck2_error::Result<OwnedFrozenValueTyped<FrozenTransitiveSet>> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard()
            .with(register_transitive_set)
            .with(tset_factory)
            .with(artifactory)
            .build();

        buck2_interpreter_for_build::attrs::coerce::testing::to_value(&env, &globals, code);

        let frozen = env
            .freeze()
            .freeze_error_context("Freeze failed")
            .map_err(from_freeze_error)?;

        let make = frozen.get("make").expect("`make` was not found");

        Module::with_temp_heap(|env2| {
            let ret = Evaluator::new(&env2).eval_function(
                make.owned_value(&env2.frozen_heap()),
                &[],
                &[],
            )?;

            env2.set_extra_value(ret);

            let frozen = env2.freeze().map_err(from_freeze_error)?;

            frozen
                .owned_extra_value()
                .buck_error_context("Frozen value must be in extra value")?
                .downcast_starlark()
                .map_err(buck2_error::Error::from)
        })
    })
}

#[test]
fn test_new_transitive_set() -> buck2_error::Result<()> {
    let _guard = TSET_TEST_LOCK.lock().unwrap();
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
