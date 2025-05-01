/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_build_api::bxl::build_result::BxlBuildResult;
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
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;

use crate::bxl::starlark_defs::context::build::StarlarkFailedArtifactIterable;
use crate::bxl::starlark_defs::context::build::StarlarkFailedArtifactIterableGen;
use crate::bxl::starlark_defs::context::build::StarlarkProvidersArtifactIterable;
use crate::bxl::starlark_defs::context::build::StarlarkProvidersArtifactIterableGen;

/// Starlark object for `StarlarkBuildResult` (which is not Starlark value).
#[derive(
    Clone,
    Debug,
    derive_more::Display,
    ProvidesStaticType,
    NoSerialize,
    Allocative
)]
pub(crate) struct StarlarkBxlBuildResult(pub(crate) BxlBuildResult);

/// The result of building in bxl.
#[starlark_module]
fn starlark_build_result_methods(builder: &mut MethodsBuilder) {
    /// Returns an optional iterable of artifacts that was successfully built.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl(ctx):
    ///     outputs = {}
    ///     for target, value in ctx.build(ctx.cli_args.target).items():
    ///         ctx.output.print(value.artifacts())
    /// ```
    fn artifacts<'v>(
        this: Value<'v>,
    ) -> starlark::Result<NoneOr<StarlarkProvidersArtifactIterable<'v>>> {
        match &this.downcast_ref::<StarlarkBxlBuildResult>().unwrap().0 {
            BxlBuildResult::None => Ok(NoneOr::None),
            BxlBuildResult::Built { .. } => {
                Ok(NoneOr::Other(StarlarkProvidersArtifactIterableGen(this)))
            }
        }
    }

    /// Returns an optional of iterable of artifacts that failed to be built.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl(ctx):
    ///     outputs = {}
    ///     for target, value in ctx.build(ctx.cli_args.target).items():
    ///         ctx.output.print(value.failures())
    /// ```
    fn failures<'v>(
        this: Value<'v>,
    ) -> starlark::Result<NoneOr<StarlarkFailedArtifactIterable<'v>>> {
        match &this.downcast_ref::<StarlarkBxlBuildResult>().unwrap().0 {
            BxlBuildResult::None => Ok(NoneOr::None),
            BxlBuildResult::Built { .. } => {
                Ok(NoneOr::Other(StarlarkFailedArtifactIterableGen(this)))
            }
        }
    }
}

starlark_simple_value!(StarlarkBxlBuildResult);

#[starlark_value(type = "bxl.BuildResult")]
impl<'v> StarlarkValue<'v> for StarlarkBxlBuildResult {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_build_result_methods)
    }
}
