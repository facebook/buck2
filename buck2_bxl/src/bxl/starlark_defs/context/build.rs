/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! Implements the ability for bxl to build targets
use std::sync::Arc;

use buck2_build_api::build::build_configured_label;
use buck2_build_api::build::MaterializationContext;
use buck2_build_api::build::ProvidersToBuild;
use buck2_build_api::bxl::build_result::BxlBuildResult;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_common::result::ToSharedResultExt;
use buck2_interpreter::types::label::Label;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::coerce::Coerce;
use gazebo::dupe::Dupe;
use starlark::eval::Evaluator;
use starlark::starlark_complex_value;
use starlark::starlark_type;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark_map::small_map::SmallMap;

use crate::bxl::starlark_defs::build_result::StarlarkBxlBuildResult;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::providers_expr::ProvidersExpr;

#[derive(
    Debug,
    Clone,
    Trace,
    Coerce,
    Freeze,
    Display,
    ProvidesStaticType,
    NoSerialize
)]
#[repr(C)]
pub(crate) struct StarlarkProvidersArtifactIterableGen<V>(pub(crate) V);

starlark_complex_value!(pub(crate) StarlarkProvidersArtifactIterable);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StarlarkProvidersArtifactIterableGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("bxl_built_artifacts_iterable");

    fn iterate<'a>(
        &'a self,
        heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        Ok(box self
            .0
            .downcast_ref::<StarlarkBxlBuildResult>()
            .unwrap()
            .0
            .unpack_built()
            .unwrap()
            .outputs
            .iter()
            .flat_map(|built| match built {
                Ok(built) => itertools::Either::Left(
                    box built
                        .values
                        .iter()
                        .map(|(artifact, _)| heap.alloc(StarlarkArtifact::new(artifact.dupe()))),
                ),
                Err(_) => itertools::Either::Right(std::iter::empty()),
            }))
    }
}

#[derive(
    Debug,
    Clone,
    Trace,
    Coerce,
    Freeze,
    Display,
    ProvidesStaticType,
    NoSerialize
)]
#[repr(C)]
pub(crate) struct StarlarkFailedArtifactIterableGen<V>(pub(crate) V);

starlark_complex_value!(pub(crate) StarlarkFailedArtifactIterable);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StarlarkFailedArtifactIterableGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("bxl_failed_artifacts_iterable");

    fn iterate<'a>(
        &'a self,
        heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        Ok(box self
            .0
            .downcast_ref::<StarlarkBxlBuildResult>()
            .unwrap()
            .0
            .unpack_built()
            .unwrap()
            .outputs
            .iter()
            .filter_map(|built| match built {
                Ok(_) => None,
                Err(e) => Some(heap.alloc(format!("{}", e))),
            }))
    }
}

pub(crate) fn build<'v>(
    ctx: &'v BxlContext,
    spec: Value<'v>,
    target_platform: Value<'v>,
    eval: &Evaluator<'v, '_>,
) -> anyhow::Result<SmallMap<Value<'v>, Value<'v>>> {
    let build_spec = ProvidersExpr::unpack(spec, target_platform, ctx, eval)?;

    let build_result = ctx.async_ctx.via_dice(async move |dice| {
        let materialization_ctx = MaterializationContext::Materialize {
            map: Arc::new(Default::default()),
            force: false,
        };

        futures::future::join_all(build_spec.labels().map(|target| {
            async {
                (
                    target.clone(),
                    build_configured_label(
                        dice,
                        &materialization_ctx,
                        target,
                        &ProvidersToBuild {
                            default: true,
                            default_other: true,
                            run: true,
                            tests: true,
                        }, // TODO support skipping/configuring?
                        false,
                    )
                    .await
                    .shared_error(),
                )
            }
        }))
        .await
    });

    build_result
        .into_iter()
        .map(|(target, result)| {
            Ok((
                eval.heap()
                    .alloc(Label::new(eval.heap(), target))
                    .get_hashed()
                    .unwrap(),
                eval.heap()
                    .alloc(StarlarkBxlBuildResult(BxlBuildResult::new(result?))),
            ))
        })
        .collect::<anyhow::Result<SmallMap<_, _>>>()
}
