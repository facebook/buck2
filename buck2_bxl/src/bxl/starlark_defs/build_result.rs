/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::bxl::build_result::StarlarkBuildResult;
use gazebo::dupe::Dupe;
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    starlark_module, starlark_simple_value, starlark_type,
    values::{NoSerialize, ProvidesStaticType, StarlarkValue, Value, ValueLike},
};

use crate::bxl::starlark_defs::context::build::{
    StarlarkFailedArtifactIterable, StarlarkFailedArtifactIterableGen,
    StarlarkProvidersArtifactIterable, StarlarkProvidersArtifactIterableGen,
};

/// Starlark object for `StarlarkBuildResult` (which is not Starlark value).
#[derive(Clone, Debug, derive_more::Display, ProvidesStaticType, NoSerialize)]
pub(crate) struct StarlarkBxlBuildResult(pub(crate) StarlarkBuildResult);

#[starlark_module]
fn starlark_build_result_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn err(this: &StarlarkBxlBuildResult) -> anyhow::Result<Option<String>> {
        Ok(match &this.0 {
            StarlarkBuildResult::Error(e) => Some(format!("{:?}", e)),
            _ => None,
        })
    }

    fn artifacts<'v>(
        this: Value<'v>,
    ) -> anyhow::Result<Option<StarlarkProvidersArtifactIterable<'v>>> {
        match &this.downcast_ref::<StarlarkBxlBuildResult>().unwrap().0 {
            StarlarkBuildResult::Error(e) => Err(e.dupe().into()),
            StarlarkBuildResult::None => Ok(None),
            StarlarkBuildResult::Built { .. } => {
                Ok(Some(StarlarkProvidersArtifactIterableGen(this)))
            }
        }
    }

    fn failures<'v>(this: Value<'v>) -> anyhow::Result<Option<StarlarkFailedArtifactIterable<'v>>> {
        match &this.downcast_ref::<StarlarkBxlBuildResult>().unwrap().0 {
            StarlarkBuildResult::Error(e) => Err(e.dupe().into()),
            StarlarkBuildResult::None => Ok(None),
            StarlarkBuildResult::Built { .. } => Ok(Some(StarlarkFailedArtifactIterableGen(this))),
        }
    }
}

starlark_simple_value!(StarlarkBxlBuildResult);

impl<'v> StarlarkValue<'v> for StarlarkBxlBuildResult {
    starlark_type!("bxl-build-result");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_build_result_methods)
    }
}
