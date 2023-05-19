/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::marker::PhantomData;

use allocative::Allocative;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use buck2_interpreter_for_build::rule::FrozenRuleCallable;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use buck2_util::late_binding::LateBinding;
use starlark::values::dict::DictOf;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueTyped;

use crate::analysis::anon_promises_dyn::AnonPromisesDyn;

pub static ANON_TARGET_REGISTRY_NEW: LateBinding<
    for<'v> fn(
        PhantomData<Value<'v>>,
        ExecutionPlatformResolution,
    ) -> Box<dyn AnonTargetsRegistryDyn<'v> + 'v>,
> = LateBinding::new("ANON_TARGET_REGISTRY_NEW");

pub trait AnonTargetsRegistryDyn<'v>: Debug + Allocative + Trace<'v> + 'v {
    fn register_one(
        &mut self,
        promise: ValueTyped<'v, StarlarkPromise<'v>>,
        rule: ValueTyped<'v, FrozenRuleCallable>,
        attributes: DictOf<'v, &'v str, Value<'v>>,
    ) -> anyhow::Result<()>;
    fn register_many(
        &mut self,
        promise: ValueTyped<'v, StarlarkPromise<'v>>,
        rules: Vec<(
            ValueTyped<'v, FrozenRuleCallable>,
            DictOf<'v, &'v str, Value<'v>>,
        )>,
    ) -> anyhow::Result<()>;
    fn take_promises(&mut self) -> Option<Box<dyn AnonPromisesDyn<'v>>>;
    fn assert_no_promises(&self) -> anyhow::Result<()>;
}
