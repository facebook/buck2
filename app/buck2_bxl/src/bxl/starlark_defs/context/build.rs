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
use allocative::Allocative;
use buck2_build_api::build::build_configured_label;
use buck2_build_api::build::BuildTargetResult;
use buck2_build_api::build::ConvertMaterializationContext;
use buck2_build_api::build::ProvidersToBuild;
use buck2_build_api::bxl::build_result::BxlBuildResult;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_cli_proto::build_request::Materializations;
use buck2_interpreter::types::label::Label;
use derive_more::Display;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::StreamExt;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
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
    NoSerialize,
    Allocative
)]
#[repr(C)]
pub(crate) struct StarlarkProvidersArtifactIterableGen<V>(pub(crate) V);

starlark_complex_value!(pub(crate) StarlarkProvidersArtifactIterable);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StarlarkProvidersArtifactIterableGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("bxl_built_artifacts_iterable");

    fn iterate_collect(&self, heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        Ok(self
            .0
            .downcast_ref::<StarlarkBxlBuildResult>()
            .unwrap()
            .0
            .unpack_built()
            .unwrap()
            .outputs
            .iter()
            .flat_map(|built| match built {
                Ok(built) => itertools::Either::Left(Box::new(
                    built
                        .values
                        .iter()
                        .map(|(artifact, _)| heap.alloc(StarlarkArtifact::new(artifact.dupe()))),
                )),
                Err(_) => itertools::Either::Right(std::iter::empty()),
            })
            .collect())
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
    NoSerialize,
    Allocative
)]
#[repr(C)]
pub(crate) struct StarlarkFailedArtifactIterableGen<V>(pub(crate) V);

starlark_complex_value!(pub(crate) StarlarkFailedArtifactIterable);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StarlarkFailedArtifactIterableGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("bxl_failed_artifacts_iterable");

    fn iterate_collect(&self, heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        Ok(self
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
            })
            .collect())
    }
}

pub(crate) fn build<'v>(
    ctx: &'v BxlContext,
    spec: Value<'v>,
    target_platform: Value<'v>,
    materializations: Materializations,
    eval: &Evaluator<'v, '_>,
) -> anyhow::Result<SmallMap<Value<'v>, Value<'v>>> {
    let build_spec = ProvidersExpr::unpack(spec, target_platform, ctx, eval)?;

    let materializations =
        ConvertMaterializationContext::with_existing_map(materializations, &ctx.materializations);

    let build_result = ctx.async_ctx.via_dice(async move |dice| {
        let materializations = &materializations;

        let stream = build_spec
            .labels()
            .map(|target| async move {
                let res = build_configured_label(
                    dice,
                    materializations,
                    target.clone(),
                    &ProvidersToBuild {
                        default: true,
                        default_other: true,
                        run: true,
                        tests: true,
                    }, // TODO support skipping/configuring?
                    false,
                )
                .await;

                match res {
                    Ok(stream) => stream.map(Ok).left_stream(),
                    Err(e) => futures::stream::once(futures::future::ready(Err(e))).right_stream(),
                }
            })
            .collect::<FuturesUnordered<_>>()
            .flatten_unordered(None);

        BuildTargetResult::collect_stream(stream).await
    })?;

    build_result
        .into_iter()
        .map(|(target, result)| {
            Ok((
                eval.heap().alloc(Label::new(target)).get_hashed().unwrap(),
                eval.heap()
                    .alloc(StarlarkBxlBuildResult(BxlBuildResult::new(result))),
            ))
        })
        .collect()
}
