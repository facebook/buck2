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
use buck2_build_api::deferred::base_deferred_key::BaseDeferredKey;
use buck2_build_api::deferred::types::testing::DeferredDataExt;
use buck2_build_api::deferred::types::testing::DeferredIdExt;
use buck2_build_api::deferred::types::DeferredData;
use buck2_build_api::deferred::types::DeferredId;
use buck2_build_api::deferred::types::DeferredKey;
use buck2_build_api::interpreter::build_defs::register_transitive_set;
use buck2_build_api::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSet;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSetOrdering;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::target::label::ConfiguredTargetLabel;
use indoc::indoc;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::Value;

use crate::interpreter::rule_defs::artifact::testing::artifactory;

#[starlark_module]
pub(crate) fn tset_factory(builder: &mut GlobalsBuilder) {
    fn make_tset<'v>(
        definition: Value<'v>,
        value: Option<Value<'v>>,
        children: Option<Value<'v>>, // An iterable.
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<TransitiveSet<'v>> {
        static LAST_ID: AtomicU32 = AtomicU32::new(0);

        let target = ConfiguredTargetLabel::testing_parse(
            "cell//path:target",
            ConfigurationData::testing_new(),
        );
        let deferred_id = DeferredId::testing_new(LAST_ID.fetch_add(1, Ordering::Relaxed));
        let deferred_key = DeferredKey::Base(BaseDeferredKey::TargetLabel(target), deferred_id);

        let set = TransitiveSet::new_from_values(
            DeferredData::testing_new(deferred_key),
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

    let globals = GlobalsBuilder::extended()
        .with(register_transitive_set)
        .with(tset_factory)
        .with(artifactory)
        .build();

    let val = buck2_interpreter_for_build::attrs::coerce::testing::to_value(&env, &globals, code);

    env.set("", val);
    let frozen = env.freeze().context("Freeze failed")?;

    let value = frozen.get("").context("Frozen tset was not found!")?;
    value
        .downcast()
        .map_err(|v| anyhow::anyhow!("Value was not a TransitiveSet: {:?}", v))
}

#[test]
fn test_new_transitive_set() -> anyhow::Result<()> {
    let set = new_transitive_set(indoc!(
        r#"
        FooSet = transitive_set()
        s1 = make_tset(FooSet, value = "foo")
        make_tset(FooSet, value = "bar", children = [s1])
        "#
    ))?;

    assert_eq!(
        set.as_ref().iter(TransitiveSetOrdering::Preorder).count(),
        2
    );

    Ok(())
}
