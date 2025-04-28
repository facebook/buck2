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
use starlark::values::NoSerialize;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::starlark_value;

/// Starlark object for Instant.
#[derive(
    Clone,
    Debug,
    derive_more::Display,
    ProvidesStaticType,
    NoSerialize,
    Allocative
)]
#[display("{:?}", _0)]
pub(crate) struct StarlarkInstant(pub(crate) Instant);

/// Instant methods, to aid in debugging/timing individual pieces of the bxl script.
#[starlark_module]
fn starlark_instant_methods(builder: &mut MethodsBuilder) {
    /// Elapsed time in secs as a float
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_elapsed_secs(ctx):
    ///     now = now()
    ///     time_a = now.elapsed_secs()
    ///     # do something that takes a long time
    ///     time_b = now.elapsed_secs()
    ///
    ///     ctx.output.print(time_a)
    ///     ctx.output.print(time_b)
    /// ```
    fn elapsed_secs<'v>(this: Value<'v>) -> starlark::Result<f64> {
        let secs = this
            .downcast_ref::<StarlarkInstant>()
            .unwrap()
            .0
            .elapsed()
            .as_secs() as f64;

        Ok(secs)
    }

    /// Elapsed time in millis as a float
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_elapsed_millis(ctx):
    ///     now = now()
    ///     time_a = now.elapsed_millis()
    ///     # do something that takes a long time
    ///     time_b = now.elapsed_millis()
    ///
    ///     ctx.output.print(time_a)
    ///     ctx.output.print(time_b)
    /// ```
    fn elapsed_millis<'v>(this: Value<'v>) -> starlark::Result<f64> {
        let millis = this
            .downcast_ref::<StarlarkInstant>()
            .unwrap()
            .0
            .elapsed()
            .as_millis() as f64;

        Ok(millis)
    }
}

starlark_simple_value!(StarlarkInstant);

#[starlark_value(type = "bxl.Instant")]
impl<'v> StarlarkValue<'v> for StarlarkInstant {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_instant_methods)
    }
}
