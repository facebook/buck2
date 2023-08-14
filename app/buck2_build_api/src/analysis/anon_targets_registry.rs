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
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_util::late_binding::LateBinding;
use starlark::any::AnyLifetime;
use starlark::values::Trace;
use starlark::values::Value;

use crate::analysis::anon_promises_dyn::AnonPromisesDyn;

pub static ANON_TARGET_REGISTRY_NEW: LateBinding<
    for<'v> fn(
        PhantomData<Value<'v>>,
        ExecutionPlatformResolution,
    ) -> Box<dyn AnonTargetsRegistryDyn<'v> + 'v>,
> = LateBinding::new("ANON_TARGET_REGISTRY_NEW");

pub trait AnonTargetsRegistryDyn<'v>:
    Debug + Allocative + Trace<'v> + AnyLifetime<'v> + 'v
{
    fn as_any_mut(&mut self) -> &mut dyn AnyLifetime<'v>;
    fn take_promises(&mut self) -> Option<Box<dyn AnonPromisesDyn<'v>>>;
    fn assert_no_promises(&self) -> anyhow::Result<()>;
}
