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
use buck2_common::result::ToSharedResultExt;
use buck2_core::configuration::Configuration;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use buck2_interpreter::common::StarlarkModulePath;
use buck2_interpreter::dice::starlark_profiler::GetStarlarkProfilerInstrumentation;
use buck2_interpreter::dice::HasCalculationDelegate;
use buck2_interpreter::dice::HasEvents;
use buck2_interpreter::starlark_profiler;
use buck2_interpreter::starlark_profiler::StarlarkProfiler;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_node::attrs::attr_type::query::ResolvedQueryLiterals;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::compatibility::MaybeCompatible;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::rule_type::RuleType;
use buck2_query::query::syntax::simple::eval::evaluator::QueryEvaluator;
use dice::DiceComputations;
use dice::Key;
use events::dispatch::EventDispatcher;
use futures::stream::FuturesOrdered;
use futures::stream::FuturesUnordered;
use futures::StreamExt;
use gazebo::prelude::*;
use once_cell::sync::OnceCell;

use crate::analysis::calculation::keys::AnalysisKey;
use crate::analysis::configured_graph::AnalysisConfiguredGraphQueryDelegate;
use crate::analysis::configured_graph::AnalysisDiceQueryDelegate;
use crate::analysis::get_user_defined_rule_impl;
use crate::analysis::run_analysis;
use crate::analysis::AnalysisResult;
use crate::attrs::analysis::AnalysisQueryResult;
use crate::events::proto::ToProtoMessage;
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
    ) -> SharedResult<MaybeCompatible<AnalysisResult>>;

    /// Return the analysis result for a configuration rule `TargetLabel`
    /// (e. g. `constraint_value`).
    async fn get_configuration_analysis_result(
        &self,
        target: &TargetLabel,
    ) -> SharedResult<AnalysisResult>;

    /// Returns the provider collection for a ConfiguredProvidersLabel. This is the full set of Providers
    /// returned by the target's rule implementation function.
    async fn get_providers(
        &self,
        target: &ConfiguredProvidersLabel,
    ) -> SharedResult<MaybeCompatible<FrozenProviderCollectionValue>>;
}

#[async_trait]
impl RuleAnalysisCalculation for DiceComputations {
    async fn get_analysis_result(
        &self,
        target: &ConfiguredTargetLabel,
    ) -> SharedResult<MaybeCompatible<AnalysisResult>> {
        #[async_trait]
        impl Key for AnalysisKey {
            type Value = SharedResult<MaybeCompatible<AnalysisResult>>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                Ok(
                    get_analysis_result(ctx, &self.0, &mut starlark_profiler::Disabled)
                        .await
                        .with_context(|| format!("When running analysis for `{}`", &self.0))?,
                )
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                // analysis result is not comparable
                // TODO consider if we want analysis result to be eq
                false
            }
        }

        self.compute(&AnalysisKey(target.dupe())).await?
    }

    async fn get_configuration_analysis_result(
        &self,
        target: &TargetLabel,
    ) -> SharedResult<AnalysisResult> {
        // Analysis for configuration nodes is always done with the unbound configuration.
        let target = target.configure(Configuration::unbound());
        Ok(self
            .get_analysis_result(&target)
            .await?
            .require_compatible()?)
    }

    async fn get_providers(
        &self,
        target: &ConfiguredProvidersLabel,
    ) -> SharedResult<MaybeCompatible<FrozenProviderCollectionValue>> {
        let analysis = self.get_analysis_result(target.target()).await?;

        analysis
            .try_map(|analysis| analysis.lookup_inner(target))
            .shared_error()
    }
}

pub async fn resolve_queries(
    ctx: &DiceComputations,
    events: &EventDispatcher,
    configured_node: &ConfiguredTargetNode,
) -> anyhow::Result<HashMap<String, Arc<AnalysisQueryResult>>> {
    let mut queries = configured_node.queries().peekable();

    if queries.peek().is_none() {
        return Ok(Default::default());
    }

    events
        .span_async(
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

async fn get_analysis_result(
    ctx: &DiceComputations,
    target: &ConfiguredTargetLabel,
    profiler: &mut dyn StarlarkProfiler,
) -> anyhow::Result<MaybeCompatible<AnalysisResult>> {
    let configured_node: MaybeCompatible<ConfiguredTargetNode> =
        ctx.get_configured_target_node(target).await?;
    let configured_node: ConfiguredTargetNode = match configured_node {
        MaybeCompatible::Incompatible(reason) => return Ok(MaybeCompatible::Incompatible(reason)),
        MaybeCompatible::Compatible(configured_node) => configured_node,
    };

    let mut dep_analysis = keep_going::try_join_all(
        configured_node
            .deps()
            .chain(configured_node.execution_deps())
            .map(async move |dep| {
                let res = ctx
                    .get_analysis_result(dep.name())
                    .await
                    .and_then(|v| v.require_compatible().shared_error());
                res.map(|x| (dep.name(), x))
            })
            .collect::<FuturesUnordered<_>>(),
    )
    .await?;

    let func = configured_node.rule_type();
    match func {
        RuleType::Starlark(func) => {
            let interpreter_calculation = ctx
                .get_interpreter_calculator(
                    func.import_path.cell(),
                    func.import_path.build_file_cell(),
                )
                .await?;
            let module = interpreter_calculation
                .eval_module(StarlarkModulePath::LoadFile(&func.import_path))
                .await?;
            let rule_impl = get_user_defined_rule_impl(module.env().dupe(), func);

            let start_event = buck2_data::AnalysisStart {
                target: Some(target.as_proto()),
                rule: func.to_string(),
            };

            let starlark_profiler_instrumentation =
                ctx.get_starlark_profiler_instrumentation().await?;

            let events = ctx.per_transaction_data().get_dispatcher();

            events
                .span_async(start_event, async {
                    let mut profile = None;

                    let result: anyhow::Result<_> = try {
                        let query_results = resolve_queries(ctx, events, &configured_node).await?;

                        let result = events.span(
                            buck2_data::AnalysisStageStart {
                                stage: Some(buck2_data::analysis_stage_start::Stage::EvaluateRule(
                                    (),
                                )),
                            },
                            || {
                                (
                                    run_analysis(
                                        target,
                                        dep_analysis,
                                        query_results,
                                        configured_node.execution_platform_resolution(),
                                        &rule_impl,
                                        &configured_node,
                                        &mut StarlarkProfilerOrInstrumentation::new(
                                            profiler,
                                            starlark_profiler_instrumentation,
                                        ),
                                    ),
                                    buck2_data::AnalysisStageEnd {},
                                )
                            },
                        )?;

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
    static CELL: OnceCell<bool> = OnceCell::new();

    let detailed: bool =
        *CELL.get_or_init(|| std::env::var_os("BUCK2_ANALYSIS_BREAKDOWN").is_some());

    let heap = res.providers().value().owner();

    let starlark_total = if detailed {
        let entries = heap
            .allocated_summary()
            .summary
            .iter()
            .map(
                |(kind, (count, bytes))| buck2_data::analysis_profile::StarlarkTotalMemoryEntry {
                    kind: kind.into(),
                    count: *count as u64,
                    total_bytes: *bytes as u64,
                },
            )
            .collect();

        Some(buck2_data::analysis_profile::StarlarkTotalMemory { entries })
    } else {
        None
    };

    buck2_data::AnalysisProfile {
        starlark_allocated_bytes: heap.allocated_bytes() as u64,
        starlark_available_bytes: heap.available_bytes() as u64,
        starlark_total,
    }
}

/// Run get_analysis_result but discard the results (public outside the `analysis` module, unlike
/// get_analysis_result)
pub async fn profile_analysis(
    ctx: &DiceComputations,
    target: &ConfiguredTargetLabel,
    profiler: &mut dyn StarlarkProfiler,
) -> anyhow::Result<()> {
    get_analysis_result(ctx, target, profiler).await?;
    Ok(())
}

mod keys {
    use buck2_core::target::ConfiguredTargetLabel;
    use derive_more::Display;
    use gazebo::prelude::*;

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
    #[display(fmt = "AnalysisKey({})", "_0")]
    pub(crate) struct AnalysisKey(pub ConfiguredTargetLabel);

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
    #[display(fmt = "ConfiguredGraphKey({})", "_0")]
    pub struct ConfiguredGraphKey(pub ConfiguredTargetLabel);
}

#[cfg(test)]
pub mod testing {
    // re-exports for testing
    pub(crate) use super::keys::AnalysisKey;
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_common::legacy_configs::LegacyBuckConfig;
    use buck2_common::legacy_configs::LegacyBuckConfigs;
    use buck2_common::package_listing::listing::testing::PackageListingExt;
    use buck2_common::package_listing::listing::PackageListing;
    use buck2_core::build_file_path::BuildFilePath;
    use buck2_core::bzl::ImportPath;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::CellAlias;
    use buck2_core::cells::CellAliasResolver;
    use buck2_core::cells::CellName;
    use buck2_core::cells::CellsAggregator;
    use buck2_core::configuration::Configuration;
    use buck2_core::fs::project::ProjectFilesystemTemp;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use buck2_core::package::testing::PackageExt;
    use buck2_core::package::Package;
    use buck2_core::provider::id::testing::ProviderIdExt;
    use buck2_core::provider::id::ProviderId;
    use buck2_core::target::testing::TargetLabelExt;
    use buck2_core::target::TargetLabel;
    use buck2_interpreter::common::OwnedStarlarkModulePath;
    use buck2_interpreter::dice::interpreter_setup::setup_interpreter_basic;
    use buck2_interpreter::dice::testing::EvalImportKey;
    use buck2_interpreter::extra::InterpreterHostArchitecture;
    use buck2_interpreter::extra::InterpreterHostPlatform;
    use buck2_interpreter::file_loader::LoadedModules;
    use buck2_node::execute::config::CommandExecutorConfig;
    use dice::testing::DiceBuilder;
    use dice::UserComputationData;
    use events::dispatch::EventDispatcher;
    use gazebo::prelude::*;
    use indexmap::IndexMap;
    use indoc::indoc;
    use itertools::Itertools;

    use crate::analysis::calculation::RuleAnalysisCalculation;
    use crate::configuration::calculation::ExecutionPlatformsKey;
    use crate::deferred::testing::DeferredAnalysisResultExt;
    use crate::execute::commands::dice_data::set_fallback_executor_config;
    use crate::interpreter::calculation::testing::InterpreterResultsKey;
    use crate::interpreter::context::configure_build_file_globals;
    use crate::interpreter::context::configure_extension_file_globals;
    use crate::interpreter::context::BuildInterpreterConfiguror;
    use crate::interpreter::rule_defs::provider::builtin::default_info::DefaultInfoCallable;
    use crate::interpreter::testing::Tester;

    #[tokio::test]
    async fn test_analysis_calculation() -> anyhow::Result<()> {
        let bzlfile = ImportPath::unchecked_new("cell", "pkg", "foo.bzl");
        let resolver = {
            let mut cells = CellsAggregator::new();
            cells.add_cell_alias_entry(
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell".to_owned())),
                CellAlias::new("".to_owned()),
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("".to_owned())),
            )?;
            cells.add_cell_alias_entry(
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell".to_owned())),
                CellAlias::new("cell".to_owned()),
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell".to_owned())),
            )?;
            cells.make_cell_resolver()?
        };
        let configs = LegacyBuckConfigs::new(hashmap![
            CellName::unchecked_new("".to_owned()) =>
            LegacyBuckConfig::empty(),
            CellName::unchecked_new("cell".to_owned()) =>
            LegacyBuckConfig::empty(),
        ]);
        let interpreter = Tester::with_cells((
            CellAliasResolver::new(Arc::new(hashmap![
                CellAlias::new("".to_owned()) =>
                CellName::unchecked_new("cell".to_owned()),
            ]))?,
            resolver.dupe(),
            configs.dupe(),
        ))?;
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
                            foo_binary = rule(impl=impl, attrs={"dep": attrs.option(attrs.dep(providers=[FooInfo])), "str": attrs.string()})
                        "#),
                LoadedModules::default(),
            )?;

        let buildfile = BuildFilePath::unchecked_new("cell", "pkg", "BUCK");
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
                map: IndexMap::from_iter([(bzlfile.id().clone(), module.dupe())]),
            },
            PackageListing::testing_new(&[], "BUCK"),
        )?;

        let fs = ProjectFilesystemTemp::new()?;
        let dice = DiceBuilder::new()
            .mock_and_return(
                EvalImportKey(OwnedStarlarkModulePath::LoadFile(bzlfile.clone())),
                Ok(module),
            )
            .mock_and_return(
                InterpreterResultsKey(Package::testing_new("cell", "pkg")),
                Ok(Arc::new(eval_res)),
            )
            .mock_and_return(ExecutionPlatformsKey, Ok(None))
            .set_data(|data| data.set_testing_io_provider(&fs))
            .build({
                let mut data = UserComputationData::new();
                set_fallback_executor_config(
                    &mut data.data,
                    CommandExecutorConfig::testing_local(),
                );
                data.data.set(EventDispatcher::null());
                data
            });
        setup_interpreter_basic(
            &dice,
            resolver,
            BuildInterpreterConfiguror::new(
                None,
                InterpreterHostPlatform::Linux,
                InterpreterHostArchitecture::X86_64,
                false,
                configure_build_file_globals,
                configure_extension_file_globals,
                |_| {},
            ),
            configs,
        );
        let dice = dice.commit();

        let analysis = dice
            .get_analysis_result(
                &TargetLabel::testing_parse("cell//pkg:rule1")
                    .configure(Configuration::testing_new()),
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
