/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::bxl::build_result::BxlBuildResult;
use gazebo::dupe::Dupe;
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

use crate::bxl::starlark_defs::context::build::StarlarkFailedArtifactIterable;
use crate::bxl::starlark_defs::context::build::StarlarkFailedArtifactIterableGen;
use crate::bxl::starlark_defs::context::build::StarlarkProvidersArtifactIterable;
use crate::bxl::starlark_defs::context::build::StarlarkProvidersArtifactIterableGen;

/// Starlark object for `StarlarkBuildResult` (which is not Starlark value).
#[derive(Clone, Debug, derive_more::Display, ProvidesStaticType, NoSerialize)]
pub(crate) struct StarlarkBxlBuildResult(pub(crate) BxlBuildResult);

#[starlark_module]
fn starlark_build_result_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn err(this: &StarlarkBxlBuildResult) -> anyhow::Result<Option<String>> {
        Ok(match &this.0 {
            BxlBuildResult::Error(e) => Some(format!("{:?}", e)),
            _ => None,
        })
    }

    fn artifacts<'v>(
        this: Value<'v>,
    ) -> anyhow::Result<Option<StarlarkProvidersArtifactIterable<'v>>> {
        match &this.downcast_ref::<StarlarkBxlBuildResult>().unwrap().0 {
            BxlBuildResult::Error(e) => Err(e.dupe().into()),
            BxlBuildResult::None => Ok(None),
            BxlBuildResult::Built { .. } => Ok(Some(StarlarkProvidersArtifactIterableGen(this))),
        }
    }

    fn failures<'v>(this: Value<'v>) -> anyhow::Result<Option<StarlarkFailedArtifactIterable<'v>>> {
        match &this.downcast_ref::<StarlarkBxlBuildResult>().unwrap().0 {
            BxlBuildResult::Error(e) => Err(e.dupe().into()),
            BxlBuildResult::None => Ok(None),
            BxlBuildResult::Built { .. } => Ok(Some(StarlarkFailedArtifactIterableGen(this))),
        }
    }
}

starlark_simple_value!(StarlarkBxlBuildResult);

impl<'v> StarlarkValue<'v> for StarlarkBxlBuildResult {
    starlark_type!("bxl-build-result");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_build_result_methods)
    }
}
