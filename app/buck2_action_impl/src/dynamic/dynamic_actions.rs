/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use starlark::any::ProvidesStaticType;
use starlark::values::starlark_value;
use starlark::values::AllocValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;

use crate::dynamic::attrs::DynamicAttrValues;
use crate::dynamic::dynamic_actions_callable::FrozenStarlarkDynamicActionsCallable;

#[derive(Debug, Trace, Allocative)]
pub(crate) struct StarlarkDynamicActionsData<'v> {
    pub(crate) callable: FrozenValueTyped<'v, FrozenStarlarkDynamicActionsCallable>,
    pub(crate) attr_values: DynamicAttrValues<Value<'v>, OutputArtifact>,
}

#[derive(
    Debug,
    NoSerialize,
    ProvidesStaticType,
    derive_more::Display,
    Trace,
    Allocative
)]
#[display("DynamicActions<...>")]
pub(crate) struct StarlarkDynamicActions<'v> {
    pub(crate) data: RefCell<Option<StarlarkDynamicActionsData<'v>>>,
}

#[starlark_value(type = "DynamicAction")]
impl<'v> StarlarkValue<'v> for StarlarkDynamicActions<'v> {}

impl<'v> AllocValue<'v> for StarlarkDynamicActions<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        // No need to freeze: we unpack contents of this struct
        // in `ctx.actions.dynamic_output` call.
        heap.alloc_complex_no_freeze(self)
    }
}
