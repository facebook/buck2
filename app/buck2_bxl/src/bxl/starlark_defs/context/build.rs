/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//!
//! Implements the ability for bxl to build targets

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_build_api::build::AsyncBuildTargetResultBuilder;
use buck2_build_api::build::BuildConfiguredLabelOptions;
use buck2_build_api::build::ProvidersToBuild;
use buck2_build_api::build::build_configured_label;
use buck2_build_api::bxl::build_result::BxlBuildResult;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_cli_proto::build_request::Materializations;
use buck2_cli_proto::build_request::Uploads;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use derive_more::Display;
use dupe::Dupe;
use futures::FutureExt;
use itertools::Itertools;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::eval::Evaluator;
use starlark::starlark_complex_value;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue as _;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use starlark::values::starlark_value;
use starlark_map::small_map::SmallMap;

use crate::bxl::starlark_defs::build_result::StarlarkBxlBuildResult;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::providers_expr::AnyProvidersExprArg;
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
pub(crate) struct StarlarkProvidersArtifactIterableGen<V: ValueLifetimeless>(pub(crate) V);

starlark_complex_value!(pub(crate) StarlarkProvidersArtifactIterable);

impl<'v, V: ValueLike<'v>> StarlarkProvidersArtifactIterableGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn iter(&self) -> impl Iterator<Item = &'v Artifact> + use<'v, V> {
        self.0
            .downcast_ref::<StarlarkBxlBuildResult>()
            .unwrap()
            .0
            .unpack_built()
            .unwrap()
            .1
            .outputs
            .iter()
            .filter_map(|built| built.as_ref().ok())
            .flat_map(|built| built.values.iter().map(|(artifact, _)| artifact))
    }
}

#[starlark_value(type = "bxl.BuiltArtifactsIterable")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkProvidersArtifactIterableGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn iterate_collect(&self, heap: Heap<'v>) -> starlark::Result<Vec<Value<'v>>> {
        Ok(self
            .iter()
            .map(|artifact| heap.alloc(StarlarkArtifact::new(artifact.dupe())))
            .collect())
    }

    fn at(&self, index: Value<'v>, heap: Heap<'v>) -> starlark::Result<Value<'v>> {
        let i = i32::unpack_value_err(index)?;
        if let Ok(i) = usize::try_from(i) {
            if let Some(artifact) = self.iter().nth(i) {
                return Ok(heap.alloc(StarlarkArtifact::new(artifact.dupe())));
            }
        }
        Err(ValueError::IndexOutOfBound(i).into())
    }

    fn length(&self) -> starlark::Result<i32> {
        i32::try_from(self.iter().count()).map_err(starlark::Error::new_other)
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
pub(crate) struct StarlarkFailedArtifactIterableGen<V: ValueLifetimeless>(pub(crate) V);

starlark_complex_value!(pub(crate) StarlarkFailedArtifactIterable);

impl<'v, V: ValueLike<'v>> StarlarkFailedArtifactIterableGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn iter(&self) -> impl Iterator<Item = &'v buck2_error::Error> + use<'v, V> {
        self.0
            .downcast_ref::<StarlarkBxlBuildResult>()
            .unwrap()
            .0
            .unpack_built()
            .unwrap()
            .1
            .outputs
            .iter()
            .filter_map(|built| built.as_ref().err())
    }
}

#[starlark_value(type = "bxl.FailedArtifactsIterable")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkFailedArtifactIterableGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn iterate_collect(&self, heap: Heap<'v>) -> starlark::Result<Vec<Value<'v>>> {
        Ok(self.iter().map(|e| heap.alloc(format!("{e}"))).collect())
    }

    fn at(&self, index: Value<'v>, heap: Heap<'v>) -> starlark::Result<Value<'v>> {
        let i = i32::unpack_value_err(index)?;
        if let Ok(i) = usize::try_from(i) {
            if let Some(e) = self.iter().nth(i) {
                return Ok(heap.alloc(format!("{e}")));
            }
        }
        Err(ValueError::IndexOutOfBound(i).into())
    }

    fn length(&self) -> starlark::Result<i32> {
        i32::try_from(self.iter().count()).map_err(starlark::Error::new_other)
    }
}

pub(crate) fn build<'v>(
    ctx: &BxlContext<'v>,
    spec: AnyProvidersExprArg<'v>,
    target_platform: ValueAsStarlarkTargetLabel<'v>,
    materializations: Materializations,
    uploads: Uploads,
    eval: &mut Evaluator<'v, '_, '_>,
) -> buck2_error::Result<
    SmallMap<
        ValueTyped<'v, StarlarkConfiguredProvidersLabel>,
        ValueTyped<'v, StarlarkBxlBuildResult>,
    >,
> {
    let global_cfg_options = ctx.resolve_global_cfg_options(target_platform, vec![])?;

    let build_result = ctx.via_dice(eval, |dice| {
        dice.via(|dice| {
            async {
                let build_spec = ProvidersExpr::<ConfiguredProvidersLabel>::unpack(
                    spec,
                    &global_cfg_options,
                    &ctx,
                    dice,
                )
                .await?;

                let (result_builder, consumer) = AsyncBuildTargetResultBuilder::new(None);
                result_builder
                    .wait_for(
                        // TODO (torozco): support --fail-fast in BXL.
                        false,
                        dice.compute_join(build_spec.labels().unique(), |ctx, target| {
                            let consumer = consumer.clone();
                            async move {
                                let target = target.clone();

                                ctx.with_linear_recompute(|ctx| async move {
                                    build_configured_label(
                                        &consumer,
                                        &ctx,
                                        (materializations, uploads).into(),
                                        target,
                                        &ProvidersToBuild {
                                            default: true,
                                            default_other: true,
                                            run: true,
                                            tests: true,
                                        }, // TODO support skipping/configuring?
                                        BuildConfiguredLabelOptions {
                                            skippable: false,
                                            graph_properties: Default::default(),
                                        },
                                        None, // TODO: support timeouts?
                                    )
                                    .await
                                })
                                .await
                            }
                            .boxed()
                        })
                        .map(|_| ()),
                    )
                    .await
            }
            .boxed_local()
        })
    })?;

    if let Some(err) = build_result
        .configured
        .values()
        .flatten()
        .flat_map(|r| &r.errors)
        .chain(build_result.other_errors.values().flatten())
        .next()
    {
        return Err(err.dupe());
    }

    Ok(build_result
        .configured
        .into_iter()
        .map(|(target, result)| {
            (
                eval.heap()
                    .alloc_typed(StarlarkConfiguredProvidersLabel::new(target.clone()))
                    .hashed()
                    .unwrap(),
                eval.heap()
                    .alloc_typed(StarlarkBxlBuildResult(BxlBuildResult::new(
                        target.clone(),
                        result,
                    ))),
            )
        })
        .collect())
}
