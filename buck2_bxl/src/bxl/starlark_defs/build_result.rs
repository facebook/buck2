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
use starlark::starlark_type;
use starlark::values::NoSerialize;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::StarlarkDocs;

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
    StarlarkDocs,
    Allocative
)]
#[starlark_docs_attrs(directory = "bxl")]
pub(crate) struct StarlarkBxlBuildResult(pub(crate) BxlBuildResult);

/// The result of building in bxl
#[starlark_module]
fn starlark_build_result_methods(builder: &mut MethodsBuilder) {
    /// Returns an optional iterable of artifacts that was successfully built.
    fn artifacts<'v>(
        this: Value<'v>,
    ) -> anyhow::Result<Option<StarlarkProvidersArtifactIterable<'v>>> {
        match &this.downcast_ref::<StarlarkBxlBuildResult>().unwrap().0 {
            BxlBuildResult::None => Ok(None),
            BxlBuildResult::Built { .. } => Ok(Some(StarlarkProvidersArtifactIterableGen(this))),
        }
    }

    /// Returns an optional of iterable of artifacts that failed to be built.
    fn failures<'v>(this: Value<'v>) -> anyhow::Result<Option<StarlarkFailedArtifactIterable<'v>>> {
        match &this.downcast_ref::<StarlarkBxlBuildResult>().unwrap().0 {
            BxlBuildResult::None => Ok(None),
            BxlBuildResult::Built { .. } => Ok(Some(StarlarkFailedArtifactIterableGen(this))),
        }
    }
}

starlark_simple_value!(StarlarkBxlBuildResult);

impl<'v> StarlarkValue<'v> for StarlarkBxlBuildResult {
    starlark_type!("bxl_build_result");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_build_result_methods)
    }
}
