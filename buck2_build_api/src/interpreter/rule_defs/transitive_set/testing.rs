/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::atomic::{AtomicUsize, Ordering};

use anyhow::Context as _;
use buck2_core::{
    configuration::Configuration,
    package::{testing::PackageExt, Package},
    target::{testing::ConfiguredTargetLabelExt, ConfiguredTargetLabel, TargetName},
};
use indoc::indoc;
use starlark::{
    environment::{GlobalsBuilder, Module},
    eval::Evaluator,
    values::{OwnedFrozenValueTyped, Value},
};

use crate::{
    deferred::{
        testing::{DeferredDataExt, DeferredIdExt},
        BaseDeferredKey, DeferredData, DeferredId, DeferredKey,
    },
    interpreter::rule_defs::{
        artifact::testing::artifactory,
        transitive_set::{FrozenTransitiveSet, TransitiveSet},
    },
};

#[starlark_module]
pub fn tset_factory(builder: &mut GlobalsBuilder) {
    fn make_tset<'v>(
        definition: Value<'v>,
        value: Option<Value<'v>>,
        children: Option<Value<'v>>, // An iterable.
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<TransitiveSet<'v>> {
        static LAST_ID: AtomicUsize = AtomicUsize::new(0);

        let target = ConfiguredTargetLabel::testing_new(
            Package::testing_new("cell", "path"),
            TargetName::unchecked_new("target"),
            Configuration::testing_new(),
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

pub fn new_transitive_set(
    code: &str,
) -> anyhow::Result<OwnedFrozenValueTyped<FrozenTransitiveSet>> {
    let env = Module::new();

    let globals = GlobalsBuilder::extended()
        .with(crate::interpreter::build_defs::register_natives)
        .with(tset_factory)
        .with(artifactory)
        .build();

    let val = crate::attrs::testing::to_value(&env, &globals, code);

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

    assert_eq!(set.as_ref().iter().count(), 2);

    Ok(())
}
