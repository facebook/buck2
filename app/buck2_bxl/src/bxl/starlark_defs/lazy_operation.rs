/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use async_recursion::async_recursion;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use futures::FutureExt;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::list::AllocList;
use starlark::values::starlark_value;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::analysis_result::StarlarkAnalysisResult;
use crate::bxl::starlark_defs::eval_extra::BxlEvalExtra;

#[derive(Derivative, Debug, Clone, Allocative)]
pub(crate) enum LazyOperation {
    Analysis(ConfiguredProvidersLabel),
    #[allow(dead_code)]
    Batch(Vec<Arc<LazyOperation>>),
}

#[derive(Allocative)]
pub(crate) enum LazyResult {
    Analysis(StarlarkAnalysisResult),
    Batch(Vec<LazyResult>),
}

impl LazyResult {
    fn into_value<'v>(self, heap: &'v Heap) -> Value<'v> {
        match self {
            LazyResult::Analysis(analysis_res) => heap.alloc(analysis_res),
            LazyResult::Batch(res) => {
                heap.alloc(AllocList(res.into_iter().map(|v| v.into_value(heap))))
            }
        }
    }
}

impl LazyOperation {
    #[async_recursion]
    async fn resolve(&self, dice: &mut DiceComputations<'_>) -> anyhow::Result<LazyResult> {
        match self {
            LazyOperation::Analysis(label) => {
                Ok(LazyResult::Analysis(analysis(dice, label).await?))
            }
            LazyOperation::Batch(lazies) => {
                let res = dice
                    .try_compute_join(lazies, |dice, lazy| {
                        async move { lazy.resolve(dice).await }.boxed()
                    })
                    .await?;
                Ok(LazyResult::Batch(res))
            }
        }
    }
}

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    Allocative,
    StarlarkDocs
)]
#[starlark_docs(directory = "bxl")]
#[derivative(Debug)]
#[display("{:?}", self)]
pub(crate) struct StarlarkLazy {
    pub(crate) lazy: Arc<LazyOperation>,
}

starlark_simple_value!(StarlarkLazy);

impl StarlarkLazy {
    pub(crate) fn new_analysis(label: ConfiguredProvidersLabel) -> Self {
        Self {
            lazy: Arc::new(LazyOperation::Analysis(label)),
        }
    }
}

#[starlark_value(type = "bxl.Lazy")]
impl<'v> StarlarkValue<'v> for StarlarkLazy {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(lazy_operation_methods)
    }
}

async fn analysis<'v>(
    dice: &mut DiceComputations<'_>,
    label: &ConfiguredProvidersLabel,
) -> anyhow::Result<StarlarkAnalysisResult> {
    let maybe_result = dice.get_analysis_result(label.target()).await?;
    match maybe_result {
        MaybeCompatible::Incompatible(reason) => Err(reason.to_err()),
        MaybeCompatible::Compatible(result) => StarlarkAnalysisResult::new(result, label.dupe()),
    }
}

/// bxl.Lazy can be resolved to the actual result. The computation only happens when called `.resolve()` or `try_resolve()`.
#[starlark_module]
fn lazy_operation_methods(builder: &mut MethodsBuilder) {
    /// Resolve the operation to the actual result without catching the error.
    ///
    /// Example:
    /// ```text
    /// def _impl(ctx):
    ///     target = ctx.configured_targets("cell//path/to:target")
    ///     analysis_result = ctx.lazy.analysis(target).resolve()
    /// ```
    fn resolve<'v>(
        this: &StarlarkLazy,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let bxl_eval_extra = BxlEvalExtra::from_context(eval)?;
        let lazy = this.lazy.clone();
        let res = bxl_eval_extra
            .via_dice(|dice| dice.via(|dice| async { lazy.resolve(dice).await }.boxed_local()));

        let heap = eval.heap();
        res.map(|v| v.into_value(heap))
    }
}
