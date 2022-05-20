//!
//! Implements the ability for bxl to build targets

use std::sync::Arc;

use buck2_core::result::{SharedError, SharedResult, ToSharedResultExt};
use derive_more::Display;
use gazebo::{
    any::{AnyLifetime, ProvidesStaticType},
    coerce::Coerce,
    prelude::*,
    variants::UnpackVariants,
};
use starlark::{
    collections::small_map::SmallMap,
    environment::{Methods, MethodsBuilder, MethodsStatic},
    eval::Evaluator,
    values::{Freeze, Heap, NoSerialize, StarlarkValue, Trace, Value, ValueLike},
};

use crate::{
    build::{build_configured_label, MaterializationContext, ProviderArtifacts, ProvidersToBuild},
    bxl::starlark_defs::{context::BxlContext, providers_expr::ProvidersExpr},
    interpreter::rule_defs::{
        artifact::StarlarkArtifact, label::Label, provider::FrozenProviderCollectionValue,
    },
};

#[derive(Clone, Debug, Display, AnyLifetime, NoSerialize, UnpackVariants)]
pub enum StarlarkBuildResult {
    Error(SharedError),
    None,
    #[display(fmt = "{:?}", self)]
    Built {
        providers: FrozenProviderCollectionValue,
        run_args: Option<Vec<String>>,
        built: Vec<SharedResult<ProviderArtifacts>>,
    },
}

impl StarlarkBuildResult {
    fn new(
        result: SharedResult<
            Option<(
                FrozenProviderCollectionValue,
                Option<Vec<String>>,
                Vec<SharedResult<ProviderArtifacts>>,
            )>,
        >,
    ) -> Self {
        match result {
            Ok(Some((providers, run_args, built))) => Self::Built {
                providers,
                run_args,
                built,
            },
            Ok(None) => Self::None,
            Err(e) => Self::Error(e),
        }
    }
}

#[starlark_module]
fn starlark_build_result_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn err(this: &StarlarkBuildResult) -> anyhow::Result<Option<String>> {
        Ok(match this {
            StarlarkBuildResult::Error(e) => Some(format!("{:?}", e)),
            _ => None,
        })
    }

    fn artifacts<'v>(
        this: Value<'v>,
    ) -> anyhow::Result<Option<StarlarkProvidersArtifactIterable<'v>>> {
        match this.downcast_ref::<StarlarkBuildResult>().unwrap() {
            StarlarkBuildResult::Error(e) => Err(e.dupe().into()),
            StarlarkBuildResult::None => Ok(None),
            StarlarkBuildResult::Built { .. } => {
                Ok(Some(StarlarkProvidersArtifactIterableGen(this)))
            }
        }
    }

    fn failures<'v>(this: Value<'v>) -> anyhow::Result<Option<StarlarkFailedArtifactIterable<'v>>> {
        match this.downcast_ref::<StarlarkBuildResult>().unwrap() {
            StarlarkBuildResult::Error(e) => Err(e.dupe().into()),
            StarlarkBuildResult::None => Ok(None),
            StarlarkBuildResult::Built { .. } => Ok(Some(StarlarkFailedArtifactIterableGen(this))),
        }
    }
}

starlark_simple_value!(StarlarkBuildResult);

impl<'v> StarlarkValue<'v> for StarlarkBuildResult {
    starlark_type!("bxl-build-result");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_build_result_methods)
    }
}

#[derive(Debug, Clone, Trace, Coerce, Freeze, Display, AnyLifetime, NoSerialize)]
#[repr(C)]
struct StarlarkProvidersArtifactIterableGen<V>(V);

starlark_complex_value!(StarlarkProvidersArtifactIterable);

impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkProvidersArtifactIterableGen<V>
where
    Self: AnyLifetime<'v> + ProvidesStaticType,
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
            .downcast_ref::<StarlarkBuildResult>()
            .unwrap()
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

#[derive(Debug, Clone, Trace, Coerce, Freeze, Display, AnyLifetime, NoSerialize)]
#[repr(C)]
struct StarlarkFailedArtifactIterableGen<V>(V);

starlark_complex_value!(StarlarkFailedArtifactIterable);

impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkFailedArtifactIterableGen<V>
where
    Self: AnyLifetime<'v> + ProvidesStaticType,
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
            .downcast_ref::<StarlarkBuildResult>()
            .unwrap()
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
                eval.heap().alloc(StarlarkBuildResult::new(result)),
            )
        })
        .collect::<SmallMap<_, _>>())
}
