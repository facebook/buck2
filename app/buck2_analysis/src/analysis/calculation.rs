/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_build_api::analysis::calculation::RuleAnalsysisCalculationImpl;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::analysis::calculation::EVAL_ANALYSIS_QUERY;
use buck2_build_api::analysis::calculation::RULE_ANALYSIS_CALCULATION;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::keep_going;
use buck2_common::result::SharedResult;
use buck2_common::result::ToUnsharedResultExt;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_data::ToProtoMessage;
use buck2_events::dispatch::async_record_root_spans;
use buck2_events::dispatch::span_async;
use buck2_events::span::SpanId;
use buck2_interpreter::dice::starlark_profiler::GetStarlarkProfilerInstrumentation;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::starlark_profiler::StarlarkProfileDataAndStats;
use buck2_interpreter::starlark_profiler::StarlarkProfileModeOrInstrumentation;
use buck2_node::attrs::attr_type::query::ResolvedQueryLiterals;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::rule_type::RuleType;
use buck2_node::rule_type::StarlarkRuleType;
use buck2_query::query::syntax::simple::eval::label_indexed::LabelIndexedSet;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use dupe::IterDupedExt;
use futures::stream::FuturesOrdered;
use futures::stream::FuturesUnordered;
use futures::StreamExt;
use smallvec::SmallVec;
use starlark::eval::ProfileMode;

use crate::analysis::env::get_user_defined_rule_impl;
use crate::analysis::env::run_analysis;
use crate::analysis::env::RuleImplFunction;
use crate::attrs::resolve::ctx::AnalysisQueryResult;

#[derive(Debug, thiserror::Error)]
enum AnalysisCalculationError {
    #[error("Internal error: literal `{0}` not found in `deps`")]
    LiteralNotFoundInDeps(String),
}

struct RuleAnalysisCalculationInstance;

#[derive(
    Clone,
    Dupe,
    derive_more::Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Allocative
)]
#[display(fmt = "{}", "_0")]
pub struct AnalysisKey(pub ConfiguredTargetLabel);

pub(crate) fn init_rule_analysis_calculation() {
    RULE_ANALYSIS_CALCULATION.init(&RuleAnalysisCalculationInstance);
}

#[async_trait]
impl RuleAnalsysisCalculationImpl for RuleAnalysisCalculationInstance {
    async fn get_analysis_result(
        &self,
        ctx: &DiceComputations,
        target: &ConfiguredTargetLabel,
    ) -> anyhow::Result<MaybeCompatible<AnalysisResult>> {
        #[async_trait]
        impl Key for AnalysisKey {
            type Value = SharedResult<MaybeCompatible<AnalysisResult>>;
            async fn compute(
                &self,
                ctx: &DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                let profile_mode = ctx.get_profile_mode_for_intermediate_analysis().await?;
                Ok(get_analysis_result(ctx, &self.0, &profile_mode)
                    .await
                    .with_context(|| format!("Error running analysis for `{}`", &self.0))?)
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                // analysis result is not comparable
                // TODO consider if we want analysis result to be eq
                false
            }
        }

        ctx.compute(&AnalysisKey(target.dupe()))
            .await?
            .unshared_error()
    }
}

pub async fn resolve_queries(
    ctx: &DiceComputations,
    configured_node: &ConfiguredTargetNode,
) -> anyhow::Result<HashMap<String, Arc<AnalysisQueryResult>>> {
    let mut queries = configured_node.queries().peekable();

    if queries.peek().is_none() {
        return Ok(Default::default());
    }

    span_async(
        buck2_data::AnalysisStageStart {
            stage: Some(buck2_data::analysis_stage_start::Stage::ResolveQueries(())),
        },
        async {
            (
                resolve_queries_impl(ctx, configured_node, queries).await,
                buck2_data::AnalysisStageEnd {},
            )
        },
    )
    .await
}

async fn resolve_queries_impl(
    ctx: &DiceComputations,
    configured_node: &ConfiguredTargetNode,
    queries: impl Iterator<Item = (String, ResolvedQueryLiterals<ConfiguredProvidersLabel>)>,
) -> anyhow::Result<HashMap<String, Arc<AnalysisQueryResult>>> {
    let deps: TargetSet<_> = configured_node.deps().duped().collect();
    let query_results =
        futures::future::try_join_all(queries.map(|(query, resolved_literals_labels)| {
            let ctx = ctx;
            let deps = &deps;
            async move {
                let mut resolved_literals =
                    HashMap::with_capacity(resolved_literals_labels.0.len());
                for (literal, label) in resolved_literals_labels.0 {
                    let node = deps.get(label.target()).ok_or_else(|| {
                        AnalysisCalculationError::LiteralNotFoundInDeps(literal.clone())
                    })?;
                    resolved_literals.insert(literal, node.dupe());
                }

                let result = (EVAL_ANALYSIS_QUERY.get()?)(ctx, &query, resolved_literals).await?;

                // analysis for all the deps in the query result should already have been run since they must
                // be in our dependency graph, and so we don't worry about parallelizing these lookups.
                let mut query_results = Vec::new();
                for node in result.iter() {
                    let label = node.label();
                    query_results.push((
                        label.dupe(),
                        ctx.get_analysis_result(label)
                            .await?
                            .require_compatible()?
                            .provider_collection,
                    ))
                }

                anyhow::Ok((query.to_owned(), Arc::new(query_results)))
            }
        }))
        .await?;

    let query_results: HashMap<_, _> = query_results.into_iter().collect();
    Ok(query_results)
}

pub async fn get_dep_analysis<'v>(
    configured_node: &'v ConfiguredTargetNode,
    ctx: &DiceComputations,
) -> anyhow::Result<Vec<(&'v ConfiguredTargetLabel, AnalysisResult)>> {
    keep_going::try_join_all(
        ctx,
        configured_node
            .deps()
            .map(async move |dep| {
                let res = ctx
                    .get_analysis_result(dep.label())
                    .await
                    .and_then(|v| v.require_compatible());
                res.map(|x| (dep.label(), x))
            })
            .collect::<FuturesUnordered<_>>(),
    )
    .await
}

pub async fn get_rule_impl(
    ctx: &DiceComputations,
    func: &StarlarkRuleType,
) -> anyhow::Result<impl RuleImplFunction> {
    let module = ctx
        .get_loaded_module_from_import_path(&func.import_path)
        .await?;
    Ok(get_user_defined_rule_impl(module.env().dupe(), func))
}

async fn get_analysis_result(
    ctx: &DiceComputations,
    target: &ConfiguredTargetLabel,
    profile_mode: &StarlarkProfileModeOrInstrumentation,
) -> anyhow::Result<MaybeCompatible<AnalysisResult>> {
    let configured_node: MaybeCompatible<ConfiguredTargetNode> =
        ctx.get_configured_target_node(target).await?;
    let configured_node: ConfiguredTargetNode = match configured_node {
        MaybeCompatible::Incompatible(reason) => {
            return Ok(MaybeCompatible::Incompatible(reason));
        }
        MaybeCompatible::Compatible(configured_node) => configured_node,
    };

    let configured_node = &configured_node;
    let mut dep_analysis = get_dep_analysis(configured_node, ctx).await?;

    let now = Instant::now();

    let (res, spans) = async_record_root_spans(async move {
        let func = configured_node.rule_type();
        match func {
            RuleType::Starlark(func) => {
                let rule_impl = get_rule_impl(ctx, func).await?;
                let start_event = buck2_data::AnalysisStart {
                    target: Some(target.as_proto().into()),
                    rule: func.to_string(),
                };

                span_async(start_event, async {
                    let mut profile = None;

                    let result: anyhow::Result<_> = try {
                        let query_results = resolve_queries(ctx, configured_node).await?;

                        let result = span_async(
                            buck2_data::AnalysisStageStart {
                                stage: Some(buck2_data::analysis_stage_start::Stage::EvaluateRule(
                                    (),
                                )),
                            },
                            async {
                                (
                                    run_analysis(
                                        ctx,
                                        target,
                                        dep_analysis,
                                        query_results,
                                        configured_node.execution_platform_resolution(),
                                        &rule_impl,
                                        configured_node,
                                        profile_mode,
                                    )
                                    .await,
                                    buck2_data::AnalysisStageEnd {},
                                )
                            },
                        )
                        .await?;

                        profile = Some(make_analysis_profile(&result));

                        MaybeCompatible::Compatible(result)
                    };

                    (
                        result,
                        buck2_data::AnalysisEnd {
                            target: Some(target.as_proto().into()),
                            rule: func.to_string(),
                            profile,
                        },
                    )
                })
                .await
            }
            RuleType::Forward => {
                assert!(dep_analysis.len() == 1);
                Ok(MaybeCompatible::Compatible(dep_analysis.pop().unwrap().1))
            }
        }
    })
    .await;

    ctx.store_evaluation_data(AnalysisKeyActivationData {
        duration: now.elapsed(),
        spans,
    })?;

    res
}

fn make_analysis_profile(res: &AnalysisResult) -> buck2_data::AnalysisProfile {
    let heap = res.providers().value().owner();

    buck2_data::AnalysisProfile {
        starlark_allocated_bytes: heap.allocated_bytes() as u64,
        starlark_available_bytes: heap.available_bytes() as u64,
    }
}

#[derive(Debug, thiserror::Error)]
enum ProfileAnalysisError {
    #[error("recursive analysis configured incorrectly (internal error)")]
    RecursiveProfileConfiguredIncorrectly,
}

/// Run get_analysis_result but discard the results (public outside the `analysis` module, unlike
/// get_analysis_result)
pub async fn profile_analysis(
    ctx: &DiceComputations,
    target: &ConfiguredTargetLabel,
    profile_mode: &ProfileMode,
) -> anyhow::Result<Arc<StarlarkProfileDataAndStats>> {
    get_analysis_result(
        ctx,
        target,
        &StarlarkProfileModeOrInstrumentation::Profile(profile_mode.dupe()),
    )
    .await?
    .require_compatible()?
    .profile_data
    .context("profile_data not set (internal error)")
}

fn all_deps(node: ConfiguredTargetNode) -> LabelIndexedSet<ConfiguredTargetNode> {
    let mut stack = vec![node];
    let mut visited = LabelIndexedSet::new();
    while let Some(node) = stack.pop() {
        if visited.insert(node.dupe()) {
            stack.extend(node.deps().duped());
        }
    }
    visited
}

pub async fn profile_analysis_recursively(
    ctx: &DiceComputations,
    target: &ConfiguredTargetLabel,
) -> anyhow::Result<StarlarkProfileDataAndStats> {
    // Self check.
    let profile_mode = ctx.get_profile_mode_for_intermediate_analysis().await?;
    if !matches!(
        profile_mode,
        StarlarkProfileModeOrInstrumentation::Profile(_)
    ) {
        return Err(ProfileAnalysisError::RecursiveProfileConfiguredIncorrectly.into());
    }

    let node = ctx
        .get_configured_target_node(target)
        .await?
        .require_compatible()?;

    let all_deps = all_deps(node);

    let mut futures = all_deps
        .iter()
        .map(|node| ctx.get_analysis_result(node.label()))
        .collect::<FuturesOrdered<_>>();

    let mut profile_datas: Vec<Arc<StarlarkProfileDataAndStats>> = Vec::new();
    while let Some(result) = futures.next().await {
        profile_datas.push(
            result?
                .require_compatible()?
                .profile_data
                .context("profile_data not set (internal error)")?,
        );
    }

    StarlarkProfileDataAndStats::merge(profile_datas.iter().map(|x| &**x))
}

pub struct AnalysisKeyActivationData {
    pub duration: Duration,
    pub spans: SmallVec<[SpanId; 1]>,
}
