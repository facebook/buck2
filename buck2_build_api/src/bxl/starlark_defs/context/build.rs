//!
//! Implements the ability for bxl to build targets

use std::sync::Arc;

use buck2_core::result::ToSharedResultExt;
use derive_more::Display;
use gazebo::{any::ProvidesStaticType, coerce::Coerce, dupe::Dupe};
use starlark::{
    collections::small_map::SmallMap,
    eval::Evaluator,
    values::{Freeze, Heap, NoSerialize, StarlarkValue, Trace, Value, ValueLike},
};

use crate::{
    build::{build_configured_label, MaterializationContext, ProvidersToBuild},
    bxl::{
        build_result::StarlarkBuildResult,
        starlark_defs::{
            build_result::StarlarkBxlBuildResult, context::BxlContext,
            providers_expr::ProvidersExpr,
        },
    },
    interpreter::rule_defs::{artifact::StarlarkArtifact, label::Label},
};

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
    starlark_type!("bxl-built-artifacts-iterable");

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
            .2
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
    starlark_type!("bxl-failed-artifacts-iterable");

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
            .2
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

    Ok(build_result
        .into_iter()
        .map(|(target, result)| {
            (
                eval.heap()
                    .alloc(Label::new(eval.heap(), target))
                    .get_hashed()
                    .unwrap(),
                eval.heap()
                    .alloc(StarlarkBxlBuildResult(StarlarkBuildResult::new(result))),
            )
        })
        .collect::<SmallMap<_, _>>())
}
