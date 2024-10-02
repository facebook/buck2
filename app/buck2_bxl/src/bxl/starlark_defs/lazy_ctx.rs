/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use derivative::Derivative;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::starlark_value;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::StarlarkDocs;

/// Context for lazy/batch/error handling operations.
/// Available as `ctx.lazy`, has type `bxl.LazyContext`.
#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    Allocative,
    StarlarkDocs
)]
#[starlark_docs(directory = "bxl")]
#[derivative(Debug)]
#[display("bxl.LazyContext")]
pub(crate) struct StarlarkLazyCtx;

starlark_simple_value!(StarlarkLazyCtx);

#[starlark_value(type = "bxl.LazyContext")]
impl<'v> StarlarkValue<'v> for StarlarkLazyCtx {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(lazy_ctx_methods)
    }
}

#[starlark_module]
fn lazy_ctx_methods(builder: &mut MethodsBuilder) {}
