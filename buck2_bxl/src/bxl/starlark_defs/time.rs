/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Instant;

use allocative::Allocative;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::StarlarkDocs;

/// Starlark object for Instant.
#[derive(
    Clone,
    Debug,
    derive_more::Display,
    ProvidesStaticType,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs_attrs(directory = "bxl")]
#[display(fmt = "{:?}", _0)]
pub struct StarlarkInstant(pub(crate) Instant);

/// Instant methods, to aid in debugging
#[starlark_module]
fn starlark_instant_methods(builder: &mut MethodsBuilder) {
    /// Elapsed time in secs as a float
    fn elapsed_secs<'v>(this: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let secs = this
            .downcast_ref::<StarlarkInstant>()
            .unwrap()
            .0
            .elapsed()
            .as_secs() as f64;

        Ok(heap.alloc(secs))
    }

    /// Elapsed time in millis as a float
    fn elapsed_millis<'v>(this: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let millis = this
            .downcast_ref::<StarlarkInstant>()
            .unwrap()
            .0
            .elapsed()
            .as_millis() as f64;

        Ok(heap.alloc(millis))
    }
}

starlark_simple_value!(StarlarkInstant);

impl<'v> StarlarkValue<'v> for StarlarkInstant {
    starlark_type!("instant");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_instant_methods)
    }
}
