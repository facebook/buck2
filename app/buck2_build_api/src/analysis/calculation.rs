/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Rule analysis related Dice calculations
use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_common::result::SharedResult;
use buck2_common::result::ToUnsharedResultExt;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_core::target::label::TargetLabel;
use buck2_data::ToProtoMessage;
use buck2_events::dispatch::span_async;
use buck2_interpreter::dice::starlark_profiler::GetStarlarkProfilerInstrumentation;
use buck2_interpreter::path::StarlarkModulePath;
use buck2_interpreter::starlark_profiler::StarlarkProfileDataAndStats;
use buck2_interpreter::starlark_profiler::StarlarkProfileModeOrInstrumentation;
use buck2_interpreter_for_build::interpreter::dice_calculation_delegate::HasCalculationDelegate;
use buck2_node::attrs::attr_type::query::ResolvedQueryLiterals;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::compatibility::MaybeCompatible;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::rule_type::RuleType;
use buck2_node::rule_type::StarlarkRuleType;
use buck2_query::query::syntax::simple::eval::evaluator::QueryEvaluator;
use buck2_query::query::syntax::simple::eval::label_indexed::LabelIndexedSet;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use dupe::IterDupedExt;
use futures::stream::FuturesOrdered;
use futures::stream::FuturesUnordered;
use futures::StreamExt;
use starlark::eval::ProfileMode;

use crate::analysis::calculation::keys::AnalysisKey;
use crate::analysis::configured_graph::AnalysisConfiguredGraphQueryDelegate;
use crate::analysis::configured_graph::AnalysisDiceQueryDelegate;
use crate::analysis::get_user_defined_rule_impl;
use crate::analysis::run_analysis;
use crate::analysis::AnalysisResult;
use crate::analysis::RuleImplFunction;
use crate::attrs::resolve::ctx::AnalysisQueryResult;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use crate::keep_going;
use crate::nodes::calculation::NodeCalculation;
use crate::query::analysis::environment::ConfiguredGraphQueryEnvironment;

#[async_trait]
pub trait RuleAnalysisCalculation {
    /// Returns the analysis result for a ConfiguredTargetLabel. This is the full set of Providers
    /// returned by the target's rule implementation function.
    async fn get_analysis_result(
        &self,
        target: &ConfiguredTargetLabel,
    ) -> anyhow::Result<MaybeCompatible<AnalysisResult>>;

    /// Return the analysis result for a configuration rule `TargetLabel`
    /// (e. g. `constraint_value`).
    async fn get_configuration_analysis_result(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<AnalysisResult>;

    /// Returns the provider collection for a ConfiguredProvidersLabel. This is the full set of Providers
    /// returned by the target's rule implementation function.
    async fn get_providers(
        &self,
        target: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<MaybeCompatible<FrozenProviderCollectionValue>>;
}

#[async_trait]
impl RuleAnalysisCalculation for DiceComputations {
    async fn get_analysis_result(
        &self,
        target: &ConfiguredTargetLabel,
    ) -> anyhow::Result<MaybeCompatible<AnalysisResult>> {
        #[async_trait]
        impl Key for AnalysisKey {
            type Value = SharedResult<MaybeCompatible<AnalysisResult>>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let profile_mode = ctx.get_profile_mode_for_intermediate_analysis().await?;
                Ok(get_analysis_result(ctx, &self.0, &profile_mode)
                    .await
                    .with_context(|| format!("When running analysis for `{}`", &self.0))?)
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                // analysis result is not comparable
                // TODO consider if we want analysis result to be eq
                false
            }
        }

        self.compute(&AnalysisKey(target.dupe()))
            .await?
            .unshared_error()
    }

    async fn get_configuration_analysis_result(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<AnalysisResult> {
        // Analysis for configuration nodes is always done with the unbound configuration.
        let target = target.configure_pair(ConfigurationNoExec::unbound().cfg_pair().dupe());
        Ok(self
            .get_analysis_result(&target)
            .await?
            .require_compatible()?)
    }

    async fn get_providers(
        &self,
        target: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<MaybeCompatible<FrozenProviderCollectionValue>> {
        let analysis = self.get_analysis_result(target.target()).await?;

        analysis.try_map(|analysis| analysis.lookup_inner(target))
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
                resolve_queries_impl(ctx, queries).await,
                buck2_data::AnalysisStageEnd {},
            )
        },
    )
    .await
}

async fn resolve_queries_impl(
    ctx: &DiceComputations,
    queries: impl Iterator<Item = (String, ResolvedQueryLiterals<ConfiguredAttr>)>,
) -> anyhow::Result<HashMap<String, Arc<AnalysisQueryResult>>> {
    let mut all_query_results = HashMap::new();

    for (query, resolved_literals_labels) in queries {
        let mut node_lookups: FuturesOrdered<_> = resolved_literals_labels
            .iter()
            .map(|(literal, label)| async move {
                (
                    literal,
                    ctx.get_configured_target_node(label.target()).await,
                )
            })
            .collect();

        let mut resolved_literals: HashMap<&str, _> = HashMap::new();
        while let Some((literal, result)) = node_lookups.next().await {
            resolved_literals.insert(literal, result?.require_compatible()?);
        }

        let dice_query_delegate = Arc::new(AnalysisDiceQueryDelegate { ctx });
        let delegate = AnalysisConfiguredGraphQueryDelegate {
            dice_query_delegate,
            resolved_literals,
        };

        let functions = ConfiguredGraphQueryEnvironment::functions();
        let env = ConfiguredGraphQueryEnvironment::new(&delegate);
        let result = {
            let evaluator = QueryEvaluator::new(&env, &functions);

            let result = evaluator.eval_query(&query).await?;
            result.try_into_targets()?
        };

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

        all_query_results.insert(query.to_owned(), std::sync::Arc::new(query_results));
    }

    Ok(all_query_results)
}

pub async fn get_dep_analysis<'v>(
    configured_node: &'v ConfiguredTargetNode,
    ctx: &DiceComputations,
) -> anyhow::Result<Vec<(&'v ConfiguredTargetLabel, AnalysisResult)>> {
    keep_going::try_join_all(
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

pub(crate) async fn get_rule_impl(
    ctx: &DiceComputations,
    func: &StarlarkRuleType,
) -> anyhow::Result<impl RuleImplFunction> {
    let interpreter_calculation = ctx
        .get_interpreter_calculator(func.import_path.cell(), func.import_path.build_file_cell())
        .await?;
    let module = interpreter_calculation
        .eval_module(StarlarkModulePath::LoadFile(&func.import_path))
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
        MaybeCompatible::Incompatible(reason) => return Ok(MaybeCompatible::Incompatible(reason)),
        MaybeCompatible::Compatible(configured_node) => configured_node,
    };

    let mut dep_analysis = get_dep_analysis(&configured_node, ctx).await?;

    let func = configured_node.rule_type();
    match func {
        RuleType::Starlark(func) => {
            let rule_impl = get_rule_impl(ctx, func).await?;
            let start_event = buck2_data::AnalysisStart {
                target: Some(target.as_proto()),
                rule: func.to_string(),
            };

            span_async(start_event, async {
                let mut profile = None;

                let result: anyhow::Result<_> = try {
                    let query_results = resolve_queries(ctx, &configured_node).await?;

                    let result = span_async(
                        buck2_data::AnalysisStageStart {
                            stage: Some(buck2_data::analysis_stage_start::Stage::EvaluateRule(())),
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
                                    &configured_node,
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
                        target: Some(target.as_proto()),
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

mod keys {
    use allocative::Allocative;
    use buck2_core::target::label::ConfiguredTargetLabel;
    use derive_more::Display;
    use dupe::Dupe;

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
    #[display(fmt = "{}", "_0")]
    pub(crate) struct AnalysisKey(pub ConfiguredTargetLabel);

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
    #[display(fmt = "{}", "_0")]
    pub struct ConfiguredGraphKey(pub ConfiguredTargetLabel);
}

#[cfg(test)]
pub mod testing {
    // re-exports for testing
    pub(crate) use super::keys::AnalysisKey;
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::sync::Arc;

    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_common::executor_config::CommandExecutorConfig;
    use buck2_common::legacy_configs::LegacyBuckConfig;
    use buck2_common::legacy_configs::LegacyBuckConfigs;
    use buck2_common::package_listing::listing::testing::PackageListingExt;
    use buck2_common::package_listing::listing::PackageListing;
    use buck2_core::build_file_path::BuildFilePath;
    use buck2_core::bzl::ImportPath;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::CellAlias;
    use buck2_core::cells::CellAliasResolver;
    use buck2_core::cells::CellsAggregator;
    use buck2_core::collections::ordered_map::OrderedMap;
    use buck2_core::configuration::ConfigurationData;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_core::package::PackageLabel;
    use buck2_core::provider::id::testing::ProviderIdExt;
    use buck2_core::provider::id::ProviderId;
    use buck2_core::target::label::TargetLabel;
    use buck2_events::dispatch::EventDispatcher;
    use buck2_execute::digest_config::DigestConfig;
    use buck2_execute::digest_config::SetDigestConfig;
    use buck2_execute::execute::dice_data::set_fallback_executor_config;
    use buck2_interpreter::extra::InterpreterHostArchitecture;
    use buck2_interpreter::extra::InterpreterHostPlatform;
    use buck2_interpreter::file_loader::LoadedModules;
    use buck2_interpreter::path::OwnedStarlarkModulePath;
    use buck2_interpreter_for_build::interpreter::calculation::testing::InterpreterResultsKey;
    use buck2_interpreter_for_build::interpreter::configuror::BuildInterpreterConfiguror;
    use buck2_interpreter_for_build::interpreter::dice_calculation_delegate::testing::EvalImportKey;
    use buck2_interpreter_for_build::interpreter::interpreter_setup::setup_interpreter_basic;
    use buck2_interpreter_for_build::interpreter::testing::Tester;
    use dice::testing::DiceBuilder;
    use dice::UserComputationData;
    use dupe::Dupe;
    use indoc::indoc;
    use itertools::Itertools;
    use maplit::hashmap;

    use crate::analysis::calculation::RuleAnalysisCalculation;
    use crate::configuration::calculation::ExecutionPlatformsKey;
    use crate::deferred::types::testing::DeferredAnalysisResultExt;
    use crate::interpreter::build_defs::register_provider;
    use crate::interpreter::rule_defs::provider::builtin::default_info::DefaultInfoCallable;
    use crate::interpreter::rule_defs::register_rule_defs;
    use crate::spawner::BuckSpawner;

    #[tokio::test]
    async fn test_analysis_calculation() -> anyhow::Result<()> {
        let bzlfile = ImportPath::testing_new("cell//pkg:foo.bzl");
        let resolver = {
            let mut cells = CellsAggregator::new();
            cells.add_cell_entry(
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell".to_owned())),
                CellAlias::new("root".to_owned()),
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("".to_owned())),
            )?;
            cells.add_cell_entry(
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell".to_owned())),
                CellAlias::new("cell".to_owned()),
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell".to_owned())),
            )?;
            cells.make_cell_resolver()?
        };
        let configs = LegacyBuckConfigs::new(hashmap![
            CellName::testing_new("root") =>
            LegacyBuckConfig::empty(),
            CellName::testing_new("cell") =>
            LegacyBuckConfig::empty(),
        ]);
        let mut interpreter = Tester::with_cells((
            CellAliasResolver::new(CellName::testing_new("cell"), Arc::new(HashMap::new()))?,
            resolver.dupe(),
            configs.dupe(),
        ))?;
        interpreter.set_additional_globals(|g| {
            register_rule_defs(g);
            register_provider(g);
        });
        let module = interpreter
            .eval_import(
                &bzlfile,
                indoc!(r#"
                            FooInfo = provider(fields=["str"])

                            def impl(ctx):
                                str = ""
                                if ctx.attrs.dep:
                                    str = ctx.attrs.dep[FooInfo].str
                                return [FooInfo(str=(str + ctx.attrs.str)), DefaultInfo()]
                            foo_binary = rule(impl=impl, attrs={"dep": attrs.option(attrs.dep(providers=[FooInfo]), default = None), "str": attrs.string()})
                        "#),
                LoadedModules::default(),
            )?;

        let buildfile = BuildFilePath::testing_new("cell//pkg:BUCK");
        let eval_res = interpreter.eval_build_file_with_loaded_modules(
            &buildfile,
            indoc!(
                r#"
                    load(":foo.bzl", "FooInfo", "foo_binary")

                    foo_binary(
                        name = "rule1",
                        str = "a",
                        dep = ":rule2",
                    )
                    foo_binary(
                        name = "rule2",
                        str = "b",
                        dep = ":rule3",
                    )
                    foo_binary(
                        name = "rule3",
                        str = "c",
                        dep = None,
                    )
                "#
            ),
            LoadedModules {
                map: OrderedMap::from_iter([(
                    OwnedStarlarkModulePath::LoadFile(bzlfile.clone()),
                    module.dupe(),
                )]),
            },
            PackageListing::testing_new(&[], "BUCK"),
        )?;

        let fs = ProjectRootTemp::new()?;
        let mut dice = DiceBuilder::new()
            .mock_and_return(
                EvalImportKey(OwnedStarlarkModulePath::LoadFile(bzlfile.clone())),
                Ok(module),
            )
            .mock_and_return(
                InterpreterResultsKey(PackageLabel::testing_parse("cell//pkg")),
                Ok(Arc::new(eval_res)),
            )
            .mock_and_return(ExecutionPlatformsKey, Ok(None))
            .set_data(|data| {
                data.set_testing_io_provider(&fs);
                data.set_digest_config(DigestConfig::compat());
            })
            .build({
                let mut data = UserComputationData::new();
                set_fallback_executor_config(
                    &mut data.data,
                    CommandExecutorConfig::testing_local(),
                );
                data.data.set(EventDispatcher::null());
                data.spawner = Arc::new(BuckSpawner::default());
                data
            })?;
        setup_interpreter_basic(
            &mut dice,
            resolver,
            BuildInterpreterConfiguror::new(
                None,
                InterpreterHostPlatform::Linux,
                InterpreterHostArchitecture::X86_64,
                false,
                |_| {},
                |_| {},
                register_rule_defs,
                |_| {},
                None,
            ),
            configs,
        )?;
        let dice = dice.commit().await;

        let analysis = dice
            .get_analysis_result(
                &TargetLabel::testing_parse("cell//pkg:rule1")
                    .configure(ConfigurationData::testing_new()),
            )
            .await?
            .require_compatible()?;

        assert_eq!(analysis.deferred.get_registered().len(), 0);

        assert_eq!(
            analysis
                .provider_collection
                .provider_collection()
                .provider_names()
                .iter()
                .sorted()
                .eq(vec!["DefaultInfo", "FooInfo"]),
            true
        );

        assert_eq!(
            analysis
                .provider_collection
                .provider_collection()
                .get_provider_raw(&ProviderId::testing_new(bzlfile.path().clone(), "FooInfo"))
                .is_some(),
            true
        );
        assert_eq!(
            analysis
                .provider_collection
                .provider_collection()
                .get_provider_raw(DefaultInfoCallable::provider_id())
                .is_some(),
            true
        );

        Ok(())
    }
}
