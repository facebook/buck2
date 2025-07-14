/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::RefCell;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::values::AllocValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::starlark_value;

use crate::dynamic::attrs::DynamicAttrValues;
use crate::dynamic::dynamic_actions_callable::FrozenStarlarkDynamicActionsCallable;

#[derive(Debug, Trace, Allocative)]
pub(crate) struct StarlarkDynamicActionsData<'v> {
    pub(crate) callable: FrozenValueTyped<'v, FrozenStarlarkDynamicActionsCallable>,
    pub(crate) attr_values: DynamicAttrValues<Value<'v>, OutputArtifact<'v>>,
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
impl<'v> StarlarkValue<'v> for StarlarkDynamicActions<'v> {
    // Used to add type documentation to the generated documentation
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(dynamic_actions_methods)
    }
}

impl<'v> AllocValue<'v> for StarlarkDynamicActions<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        // No need to freeze: we unpack contents of this struct
        // in `ctx.actions.dynamic_output` call.
        heap.alloc_complex_no_freeze(self)
    }
}

/// Opaque thunk type returned from calling the returned value of a `dynamic_actions` or
/// `bxl.dynamic_actions` invocation. Conceptually, a tuple of (function, args).
///
/// Must be passed to
/// [`AnalysisActions.dynamic_output_new`](../AnalysisActions#analysisactionsdynamic_output_new).
#[starlark_module]
fn dynamic_actions_methods(builder: &mut MethodsBuilder) {}
