/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(async_closure)]
#![feature(async_fn_in_trait)]

pub(crate) mod calculation;
pub(crate) mod registration;

use std::future::Future;
use std::pin::Pin;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::interpreter::rule_defs::provider::builtin::platform_info::PlatformInfo;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::target::label::TargetLabel;
use buck2_core::unsafe_send_future::UnsafeSendFuture;
use buck2_events::dispatch::get_dispatcher;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_node::cfg_constructor::CfgConstructorImpl;
use buck2_node::cfg_constructor::CFG_CONSTRUCTOR_CALCULATION_IMPL;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::RuleKind;
use calculation::CfgConstructorCalculationInstance;
use dice::DiceComputations;
use futures::future::try_join_all;
use starlark::collections::SmallMap;
use starlark::collections::SmallSet;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::structs::AllocStruct;
use starlark::values::OwnedFrozenValue;
use starlark::values::UnpackValue;
use starlark::values::Value;

use crate::registration::init_registration;

#[derive(Debug, thiserror::Error)]
enum CfgConstructorError {
    #[error(
        "Parameter `refs` to post-constraint analysis function must only contain configuration rules. {0} is not a configuration rule."
    )]
    PostConstraintAnalysisRefsMustBeConfigurationRules(String),
}

#[derive(Allocative, Debug)]
pub(crate) struct CfgConstructor {
    pub(crate) cfg_constructor_pre_constraint_analysis: OwnedFrozenValue,
    pub(crate) cfg_constructor_post_constraint_analysis: OwnedFrozenValue,
}

async fn eval_pre_constraint_analysis<'v>(
    cfg_constructor_pre_constraint_analysis: Value<'v>,
    ctx: &DiceComputations,
    cfg: &ConfigurationData,
    module: &'v Module,
    print: &'v EventDispatcherPrintHandler,
) -> anyhow::Result<(Vec<String>, Value<'v>, Evaluator<'v, 'v>)> {
    with_starlark_eval_provider(
        ctx,
        // TODO: pass proper profiler (T163570348)
        &mut StarlarkProfilerOrInstrumentation::disabled(),
        "pre constraint-analysis invocation".to_owned(),
        |provider, _| {
            let mut eval = provider.make(module)?;
            eval.set_print_handler(print);

            let pre_constraint_analysis_args = vec![(
                "legacy_platform",
                // TODO: should eventually accept cli modifiers, target modifiers, and PACKAGE modifiers (T163570597)
                // and unbound platform case will be handled properly
                if cfg.is_bound() {
                    eval.heap()
                        .alloc_complex(PlatformInfo::from_configuration(cfg, eval.heap())?)
                } else {
                    Value::new_none()
                },
            )];

            // Type check + unpack
            let (refs, params) =
                <(UnpackListOrTuple<String>, Value)>::unpack_value_err(eval.eval_function(
                    cfg_constructor_pre_constraint_analysis,
                    &[],
                    &pre_constraint_analysis_args,
                )?)?;

            // `params` Value lives on eval.heap() so we need to move eval out of the closure to keep it alive
            Ok((refs.items, params, eval))
        },
    )
    .await
}

async fn analyze_constraints(
    ctx: &DiceComputations,
    refs: Vec<String>,
) -> anyhow::Result<SmallMap<String, FrozenProviderCollectionValue>> {
    let res: SmallMap<String, FrozenProviderCollectionValue> =
        try_join_all(SmallSet::from_iter(refs).into_iter().map(
            async move |label_str| -> anyhow::Result<(String, FrozenProviderCollectionValue)> {
                let cell_resolver = ctx.get_cell_resolver().await?;

                // Ensure all refs are configuration rules
                let label =
                    TargetLabel::parse(&label_str, cell_resolver.root_cell(), &cell_resolver)?;

                if ctx.get_target_node(&label).await?.rule_kind() == RuleKind::Configuration {
                    Ok((
                        label_str,
                        ctx.get_configuration_analysis_result(&label)
                            .await?
                            .provider_collection,
                    ))
                } else {
                    Err(
                        CfgConstructorError::PostConstraintAnalysisRefsMustBeConfigurationRules(
                            label_str,
                        )
                        .into(),
                    )
                }
            },
        ))
        .await?
        .into_iter()
        .collect();
    Ok(res)
}

async fn eval_post_constraint_analysis<'v>(
    cfg_constructor_post_constraint_analysis: Value<'v>,
    ctx: &DiceComputations,
    params: Value<'v>,
    mut eval: Evaluator<'v, 'v>,
    refs_providers_map: SmallMap<String, FrozenProviderCollectionValue>,
) -> anyhow::Result<ConfigurationData> {
    with_starlark_eval_provider(
        ctx,
        // TODO: pass proper profiler (T163570348)
        &mut StarlarkProfilerOrInstrumentation::disabled(),
        "post constraint-analysis invocation for cfg".to_owned(),
        |_, _| -> anyhow::Result<ConfigurationData> {
            let post_constraint_analysis_args = vec![
                (
                    "refs",
                    eval.heap().alloc(AllocStruct(
                        refs_providers_map
                            .into_iter()
                            .map(|(label, providers)| {
                                (label, providers.value().owned_value(eval.frozen_heap()))
                            })
                            .collect::<SmallMap<String, Value<'_>>>(),
                    )),
                ),
                ("params", params),
            ];

            let post_constraint_analysis_result = eval.eval_function(
                cfg_constructor_post_constraint_analysis,
                &[],
                &post_constraint_analysis_args,
            )?;

            // Type check + unpack
            <&PlatformInfo>::unpack_value_err(post_constraint_analysis_result)?.to_configuration()
        },
    )
    .await
}

async fn eval_underlying(
    cfg_constructor: &CfgConstructor,
    ctx: &DiceComputations,
    cfg: &ConfigurationData,
) -> anyhow::Result<ConfigurationData> {
    let module = Module::new();
    let print = EventDispatcherPrintHandler(get_dispatcher());

    // Pre constraint-analysis
    let (refs, params, eval) = eval_pre_constraint_analysis(
        cfg_constructor
            .cfg_constructor_pre_constraint_analysis
            .value(),
        ctx,
        cfg,
        &module,
        &print,
    )
    .await?;

    // Constraint analysis
    let refs_providers_map = analyze_constraints(ctx, refs).await?;

    // Post constraint-analysis
    eval_post_constraint_analysis(
        cfg_constructor
            .cfg_constructor_post_constraint_analysis
            .value(),
        ctx,
        params,
        eval,
        refs_providers_map,
    )
    .await
}

#[async_trait]
impl CfgConstructorImpl for CfgConstructor {
    fn eval<'a>(
        &'a self,
        ctx: &'a DiceComputations,
        cfg: &'a ConfigurationData,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<ConfigurationData>> + Send + 'a>> {
        // Get around issue of Evaluator not being send by wrapping future in UnsafeSendFuture
        let fut = async move { eval_underlying(self, ctx, cfg).await };
        unsafe { Box::pin(UnsafeSendFuture::new_encapsulates_starlark(fut)) }
    }
}

pub fn init_late_bindings() {
    CFG_CONSTRUCTOR_CALCULATION_IMPL.init(&CfgConstructorCalculationInstance);
    init_registration();
}
