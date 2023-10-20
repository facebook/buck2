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

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::build::build_configured_label;
use buck2_build_api::build::BuildConfiguredLabelOptions;
use buck2_build_api::build::BuildEvent;
use buck2_build_api::build::ConfiguredBuildTargetResult;
use buck2_build_api::build::ConvertMaterializationContext;
use buck2_build_api::build::ProvidersToBuild;
use buck2_build_api::bxl::build_result::BxlBuildResult;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_cli_proto::build_request::Materializations;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_error::shared_result::SharedError;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use dashmap::DashMap;
use derive_more::Display;
use dice::DiceComputationsParallel;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::stream::FuturesUnordered;
use futures::FutureExt;
use futures::StreamExt;
use itertools::Itertools;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::eval::Evaluator;
use starlark::starlark_complex_value;
use starlark::values::starlark_value;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue as _;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use starlark_map::small_map::SmallMap;

use crate::bxl::starlark_defs::build_result::StarlarkBxlBuildResult;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::providers_expr::ConfiguredProvidersExprArg;
use crate::bxl::starlark_defs::providers_expr::ProvidersExpr;
use crate::bxl::value_as_starlark_target_label::ValueAsStarlarkTargetLabel;

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

impl<'v, V: ValueLike<'v> + 'v> StarlarkProvidersArtifactIterableGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn iter(&self) -> impl Iterator<Item = &'v Artifact> {
        self.0
            .downcast_ref::<StarlarkBxlBuildResult>()
            .unwrap()
            .0
            .unpack_built()
            .unwrap()
            .outputs
            .iter()
            .filter_map(|built| built.as_ref().ok())
            .flat_map(|built| built.values.iter().map(|(artifact, _)| artifact))
    }
}

#[starlark_value(type = "bxl_built_artifacts_iterable")]
impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StarlarkProvidersArtifactIterableGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn iterate_collect(&self, heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        Ok(self
            .iter()
            .map(|artifact| heap.alloc(StarlarkArtifact::new(artifact.dupe())))
            .collect())
    }

    fn at(&self, index: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let i = i32::unpack_value_err(index)?;
        if let Ok(i) = usize::try_from(i) {
            if let Some(artifact) = self.iter().nth(i) {
                return Ok(heap.alloc(StarlarkArtifact::new(artifact.dupe())));
            }
        }
        Err(anyhow::anyhow!(ValueError::IndexOutOfBound(i)))
    }

    fn length(&self) -> anyhow::Result<i32> {
        i32::try_from(self.iter().count()).map_err(|e| e.into())
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

impl<'v, V: ValueLike<'v> + 'v> StarlarkFailedArtifactIterableGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn iter(&self) -> impl Iterator<Item = &'v SharedError> {
        self.0
            .downcast_ref::<StarlarkBxlBuildResult>()
            .unwrap()
            .0
            .unpack_built()
            .unwrap()
            .outputs
            .iter()
            .filter_map(|built| built.as_ref().err())
    }
}

#[starlark_value(type = "bxl_failed_artifacts_iterable")]
impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StarlarkFailedArtifactIterableGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn iterate_collect(&self, heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        Ok(self.iter().map(|e| heap.alloc(format!("{}", e))).collect())
    }

    fn at(&self, index: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let i = i32::unpack_value_err(index)?;
        if let Ok(i) = usize::try_from(i) {
            if let Some(e) = self.iter().nth(i) {
                return Ok(heap.alloc(format!("{}", e)));
            }
        }
        Err(anyhow::anyhow!(ValueError::IndexOutOfBound(i)))
    }

    fn length(&self) -> anyhow::Result<i32> {
        i32::try_from(self.iter().count()).map_err(|e| e.into())
    }
}

pub(crate) fn build<'v>(
    ctx: &BxlContext<'v>,
    materializations_map: &Arc<DashMap<BuildArtifact, ()>>,
    spec: ConfiguredProvidersExprArg<'v>,
    target_platform: ValueAsStarlarkTargetLabel<'v>,
    materializations: Materializations,
    eval: &Evaluator<'v, '_>,
) -> anyhow::Result<
    SmallMap<
        ValueTyped<'v, StarlarkConfiguredProvidersLabel>,
        ValueTyped<'v, StarlarkBxlBuildResult>,
    >,
> {
    let materializations =
        ConvertMaterializationContext::with_existing_map(materializations, materializations_map);

    let target_platform = target_platform.parse_target_platforms(
        &ctx.data.target_alias_resolver,
        &ctx.data.cell_resolver,
        ctx.data.cell_name,
        &ctx.data.global_target_platform,
    )?;

    let build_result = ctx.via_dice(
        |mut dice, ctx|
            dice.via(|dice|
        async {
            let build_spec = ProvidersExpr::<ConfiguredProvidersLabel>::unpack(
                spec,
                target_platform,
                ctx,
                dice,
            )
            .await?;

            let stream = dice
                .compute_many(build_spec.labels().unique().map(|target| {
                    let target = target.clone();
                    let materializations = materializations.dupe();
                    higher_order_closure! {
                        for <'x> move |dice: &'x mut DiceComputationsParallel<'_>| -> BoxFuture<'x, Vec<BuildEvent>> {
                            async move {
                                build_configured_label(
                                    dice,
                                    &materializations,
                                    target,
                                    &ProvidersToBuild {
                                        default: true,
                                        default_other: true,
                                        run: true,
                                        tests: true,
                                    }, // TODO support skipping/configuring?
                                    BuildConfiguredLabelOptions {
                                        skippable: false,
                                        want_configured_graph_size: false,
                                    },
                                ).await
                            }.then(|stream| stream.collect::<Vec<_>>()).boxed()
                        }
                    }
                }))
                .into_iter().collect::<FuturesUnordered<_>>().map(|v| v.into_iter().map(futures::future::ready).collect::<FuturesUnordered<_>>()).flatten();

            // TODO (torozco): support --fail-fast in BXL.
            ConfiguredBuildTargetResult::collect_stream(stream.map(Ok), false).await
        }.boxed_local())
    )?;

    if let Some(err) = build_result
        .values()
        .flatten()
        .flat_map(|r| &r.errors)
        .next()
    {
        return Err(err.dupe().into());
    }

    Ok(build_result
        .into_iter()
        .map(|(target, result)| {
            (
                eval.heap()
                    .alloc_typed(StarlarkConfiguredProvidersLabel::new(target))
                    .hashed()
                    .unwrap(),
                eval.heap()
                    .alloc_typed(StarlarkBxlBuildResult(BxlBuildResult::new(result))),
            )
        })
        .collect())
}
