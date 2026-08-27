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
use buck2_build_api::interpreter::rule_defs::transitive_set::FrozenTransitiveSetDefinition;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSet;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSetOrdering;
use buck2_build_api::interpreter::rule_defs::transitive_set::transitive_set_definition::register_transitive_set;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_error::BuckErrorOptionContext;
use buck2_interpreter::from_freeze::from_freeze_error;
use buck2_interpreter::testing::Buck2TestHeapName;
use indoc::indoc;
use starlark::environment::FrozenModule;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::FreezeErrorContext;
use starlark::values::FrozenValueTyped;
use starlark::values::OwnedFrozen;
use starlark::values::Value;
use starlark::values::ValueTyped;

use crate::interpreter::rule_defs::artifact::testing::artifactory;

/// Global mutex to serialize tests that use `make_tset()`, which increments a shared
/// global counter (LAST_ID). Without serialization, parallel test execution causes
/// non-deterministic TransitiveSetIndex assignment, leading to intermittent failures.
pub static TSET_TEST_LOCK: Mutex<()> = Mutex::new(());

#[starlark_module]
pub(crate) fn tset_factory(builder: &mut GlobalsBuilder) {
    fn make_tset<'v>(
        definition: FrozenValueTyped<'v, FrozenTransitiveSetDefinition<'v>>,
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
) -> buck2_error::Result<OwnedFrozen<ValueTyped<'static, TransitiveSet<'static>>>> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard()
            .with(register_transitive_set)
            .with(tset_factory)
            .with(artifactory)
            .build();

        buck2_interpreter_for_build::attrs::coerce::testing::to_value(&env, &globals, code);

        let frozen = env
            .freeze_named(Buck2TestHeapName::frozen_heap_name())
            .freeze_error_context("Freeze failed")
            .map_err(from_freeze_error)?;

        let make = frozen.get_owned("make").expect("`make` was not found");

        Module::with_temp_heap(|env2| {
            let ret = Evaluator::new(&env2).eval_function(
                make.as_ref().add_to_heap(env2.heap()),
                &[],
                &[],
            )?;

            env2.set_extra_value(ret);

            let frozen = env2
                .freeze_named(Buck2TestHeapName::frozen_heap_name())
                .map_err(from_freeze_error)?;

            frozen
                .extra_value_owned()
                .internal_error("Frozen value must be in extra value")?
                .downcast_starlark::<TransitiveSet<'static>>()
                .map_err(buck2_error::Error::from)
        })
    })
}

/// Freeze `code` as `root//:defs.bzl`. Two calls produce two allocations of every definition in it,
/// which is what a page-out/page-in of one module heap can leave behind in a live daemon.
fn freeze_defs_module(code: &str) -> buck2_error::Result<FrozenModule> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard()
            .with(register_transitive_set)
            .with(tset_factory)
            .with(artifactory)
            .build();

        buck2_interpreter_for_build::attrs::coerce::testing::to_value(&env, &globals, code);

        env.freeze_named(Buck2TestHeapName::frozen_heap_name())
            .freeze_error_context("Freeze failed")
            .map_err(from_freeze_error)
    })
}

/// A child whose definition is a different allocation of the same logical `FooSet` must still be
/// accepted. Comparing definitions by pointer rejects it with an error whose `expected` and `got`
/// render identically.
#[test]
fn test_child_definition_from_other_incarnation_is_accepted() -> buck2_error::Result<()> {
    let _guard = TSET_TEST_LOCK.lock().unwrap();

    let defs = "FooSet = transitive_set()";

    let first = freeze_defs_module(defs)?;
    let second = freeze_defs_module(defs)?;

    let first_foo_set = first.get_owned("FooSet").expect("`FooSet` was not found");
    let second_foo_set = second.get_owned("FooSet").expect("`FooSet` was not found");

    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard()
            .with(register_transitive_set)
            .with(tset_factory)
            .with(artifactory)
            .build();

        buck2_interpreter_for_build::attrs::coerce::testing::to_value(
            &env,
            &globals,
            indoc!(
                r#"
                def make(first_foo_set, second_foo_set):
                    child = make_tset(first_foo_set, value = 1)
                    return make_tset(second_foo_set, value = 2, children = [child])
                "#
            ),
        );

        let make = env.get("make").expect("`make` was not found");
        let args = [
            first_foo_set.as_ref().add_to_heap(env.heap()),
            second_foo_set.as_ref().add_to_heap(env.heap()),
        ];
        Evaluator::new(&env).eval_function(make, &args, &[])?;

        buck2_error::Ok(())
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
        set.by_ref(|s| s.iter(TransitiveSetOrdering::Preorder).count()),
        2
    );

    Ok(())
}
