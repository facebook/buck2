/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::pattern::package_roots::find_package_roots_stream;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_common::result::SharedResult;
use buck2_core::configuration::Configuration;
use buck2_core::package::Package;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::PatternType;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use buck2_core::target::TargetName;
use buck2_execute::path::buck_out_path::BuckOutPathResolver;
use buck2_execute::path::buck_out_path::BuckPathResolver;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_interpreter_for_build::interpreter::module_internals::EvaluationResult;
use buck2_node::compatibility::MaybeCompatible;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::RuleKind;
use buck2_node::nodes::unconfigured::TargetNode;
use dice::DiceComputations;
use futures::future::BoxFuture;
use futures::stream::FuturesUnordered;
use futures::FutureExt;
use futures::Stream;
use futures::StreamExt;
use gazebo::dupe::Dupe;
use itertools::Itertools;
use owning_ref::ArcRef;
use thiserror::Error;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::artifact::fs::ArtifactFs;
use crate::actions::calculation as action_calculation;
use crate::actions::key::ActionKey;
use crate::analysis::calculation as analysis_calculation;
use crate::artifact_groups::calculation as artifact_group_calculation;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::configuration::ConfigurationCalculation;
use crate::context::HasBuildContextData;
use crate::deferred::calculation as deferred_calculation;
use crate::deferred::types::AnyValue;
use crate::deferred::types::DeferredData;
use crate::execute::ActionOutputs;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use crate::nodes::calculation as node_calculation;
use crate::nodes::calculation::get_execution_platform_toolchain_dep;

#[derive(Debug, Error)]
pub enum BuildErrors {
    #[error("No target with name `{1}` in package `{0}`.")]
    MissingTarget(Package, TargetName),

    #[error("Did not find package with name `{0}`.")]
    MissingPackage(Package),
}

pub trait ConfigurableTarget: Send + Sync {
    type Configured;

    fn target(&self) -> &TargetLabel;

    fn configure(&self, cfg: Configuration) -> Self::Configured;

    fn configure_with_exec(&self, cfg: Configuration, exec_cfg: Configuration) -> Self::Configured;
}

impl ConfigurableTarget for TargetLabel {
    type Configured = ConfiguredTargetLabel;

    fn target(&self) -> &TargetLabel {
        self
    }

    fn configure(&self, cfg: Configuration) -> Self::Configured {
        self.configure(cfg)
    }

    fn configure_with_exec(&self, cfg: Configuration, exec_cfg: Configuration) -> Self::Configured {
        self.configure_with_exec(cfg, exec_cfg)
    }
}

impl ConfigurableTarget for ProvidersLabel {
    type Configured = ConfiguredProvidersLabel;

    fn target(&self) -> &TargetLabel {
        self.target()
    }

    fn configure(&self, cfg: Configuration) -> Self::Configured {
        self.configure(cfg)
    }

    fn configure_with_exec(&self, cfg: Configuration, exec_cfg: Configuration) -> Self::Configured {
        self.configure_with_exec(cfg, exec_cfg)
    }
}

/// Provides the Dice calculations used for implementing builds and related operations.
///
/// Most of this is implemented within the buck2_build_api, with some thin wrappers over some
/// interpreter things.
#[async_trait]
pub trait Calculation<'c> {
    /// Get the configured ArtifactFs
    async fn get_artifact_fs(&self) -> SharedResult<ArtifactFs>;

    /// Returns the Configuration for an unconfigured TargetLabel or ProvidersLabel.
    ///
    /// This performs "target platform resolution" on the provided target and returns the configured
    /// result.
    ///
    /// Normally, a TargetLabel and ProvidersLabel would
    /// get its Configuration based on the context it's being requested in (i.e configuration is
    /// passed down from higher nodes). For top-level requested things, though, we will have an
    /// unconfigured (or "lightly"-configured) thing and the Configuration will be determined as
    /// a mix of the global Configuration, the target's `default_target_platform` and
    /// (potentially) self-transitions on that node.
    async fn get_configured_target<T: ConfigurableTarget>(
        &self,
        target: &T,
        global_target_platform: Option<&TargetLabel>,
    ) -> SharedResult<T::Configured>;

    /// For a TargetLabel, returns the TargetNode. This is really just part of the the interpreter
    /// results for the the label's package, and so this is just a utility for accessing that, it
    /// isn't separately cached.
    async fn get_target_node(&self, target: &TargetLabel) -> SharedResult<TargetNode>;

    /// Returns the ConfiguredTargetNode corresponding to a ConfiguredTargetLabel.
    async fn get_configured_target_node(
        &self,
        target: &ConfiguredTargetLabel,
    ) -> SharedResult<MaybeCompatible<ConfiguredTargetNode>>;

    /// Returns the provider collection for a ConfiguredProvidersLabel. This is the full set of Providers
    /// returned by the target's rule implementation function.
    async fn get_providers(
        &self,
        target: &ConfiguredProvidersLabel,
    ) -> SharedResult<MaybeCompatible<FrozenProviderCollectionValue>>;

    /// Builds a specific 'Action' given the 'Action' itself
    async fn build_action(&self, action_key: &ActionKey) -> SharedResult<ActionOutputs>;

    /// Builds and materializes the given 'BuildArtifact'
    async fn build_artifact(&self, artifact: &BuildArtifact) -> SharedResult<ActionOutputs>;

    /// Makes an 'Artifact' available to be accessed
    async fn ensure_artifact_group(
        &self,
        input: &ArtifactGroup,
    ) -> anyhow::Result<ArtifactGroupValues>;

    /// Computes and returns the evaluated value of an 'DeferredData'
    async fn compute_deferred_data<T: Send + Sync + 'static>(
        &self,
        data: &DeferredData<T>,
    ) -> SharedResult<ArcRef<dyn AnyValue, T>>;
}

#[async_trait]
impl<'c> Calculation<'c> for DiceComputations {
    async fn get_artifact_fs(&self) -> SharedResult<ArtifactFs> {
        let buck_out_path_resolver =
            BuckOutPathResolver::new((*self.get_buck_out_path().await?).to_buf());
        let project_filesystem = self.global_data().get_io_provider().fs().dupe();
        let buck_path_resolver = BuckPathResolver::new(self.get_cell_resolver().await?);
        Ok(ArtifactFs::new(
            buck_path_resolver,
            buck_out_path_resolver,
            project_filesystem,
        ))
    }

    async fn get_configured_target<T: ConfigurableTarget>(
        &self,
        target: &T,
        global_target_platform: Option<&TargetLabel>,
    ) -> SharedResult<T::Configured> {
        let node = self.get_target_node(target.target()).await?;

        let get_platform_configuration = async || -> SharedResult<Configuration> {
            Ok(match global_target_platform {
                Some(global_target_platform) => {
                    self.get_platform_configuration(global_target_platform)
                        .await?
                }
                None => match node.get_default_target_platform() {
                    Some(target) => self.get_platform_configuration(target.target()).await?,
                    None => self.get_default_platform(target.target()).await?,
                },
            })
        };

        match node.rule_kind() {
            RuleKind::Configuration => Ok(target.configure(Configuration::unbound())),
            RuleKind::Normal => Ok(target.configure(get_platform_configuration().await?)),
            RuleKind::Toolchain => {
                let cfg = get_platform_configuration().await?;
                let exec_cfg = get_execution_platform_toolchain_dep(
                    self,
                    &target.target().configure(cfg.dupe()),
                    &node,
                )
                .await?
                .cfg();
                Ok(target.configure_with_exec(cfg, exec_cfg))
            }
        }
    }

    async fn get_target_node(&self, target: &TargetLabel) -> SharedResult<TargetNode> {
        node_calculation::NodeCalculation::get_target_node(self, target).await
    }

    async fn get_configured_target_node(
        &self,
        target: &ConfiguredTargetLabel,
    ) -> SharedResult<MaybeCompatible<ConfiguredTargetNode>> {
        node_calculation::NodeCalculation::get_configured_target_node(self, target).await
    }

    async fn get_providers(
        &self,
        target: &ConfiguredProvidersLabel,
    ) -> SharedResult<MaybeCompatible<FrozenProviderCollectionValue>> {
        analysis_calculation::RuleAnalysisCalculation::get_providers(self, target).await
    }

    async fn build_action(&self, action_key: &ActionKey) -> SharedResult<ActionOutputs> {
        action_calculation::ActionCalculation::build_action(self, action_key).await
    }

    async fn build_artifact(&self, artifact: &BuildArtifact) -> SharedResult<ActionOutputs> {
        action_calculation::ActionCalculation::build_artifact(self, artifact).await
    }

    /// makes the 'Artifact' available to be accessed
    async fn ensure_artifact_group(
        &self,
        input: &ArtifactGroup,
    ) -> anyhow::Result<ArtifactGroupValues> {
        // TODO consider if we need to cache this
        artifact_group_calculation::ArtifactGroupCalculation::ensure_artifact_group(self, input)
            .await
    }

    async fn compute_deferred_data<T: Send + Sync + 'static>(
        &self,
        data: &DeferredData<T>,
    ) -> SharedResult<ArcRef<dyn AnyValue, T>> {
        deferred_calculation::DeferredCalculation::compute_deferred_data(self, data).await
    }
}

async fn resolve_patterns_and_load_buildfiles<'c, T: PatternType>(
    ctx: &'c DiceComputations,
    parsed_patterns: Vec<ParsedPattern<T>>,
) -> anyhow::Result<(
    ResolvedPattern<T>,
    impl Stream<Item = (Package, SharedResult<Arc<EvaluationResult>>)> + 'c,
)> {
    let mut spec = ResolvedPattern::<T>::new();
    let load_package_futs = FuturesUnordered::new();
    let mut recursive_packages = Vec::new();

    fn load_package<'a>(
        ctx: &'a DiceComputations,
        package: Package,
    ) -> BoxFuture<'a, (Package, SharedResult<Arc<EvaluationResult>>)> {
        // it's important that this is not async and the temporary spawn happens when the function is called as we don't immediately start polling these.
        ctx.temporary_spawn(async move |ctx| {
            let res = ctx.get_interpreter_results(&package).await;
            (package, res)
        })
        .boxed()
    }

    for pattern in parsed_patterns {
        match pattern {
            ParsedPattern::Target(package, target) => {
                spec.add_target(&package, &target);
                load_package_futs.push(load_package(ctx, package));
            }
            ParsedPattern::Package(package) => {
                spec.add_package(&package);
                load_package_futs.push(load_package(ctx, package));
            }
            ParsedPattern::Recursive(package) => {
                recursive_packages.push(package);
            }
        }
    }

    let recursive_pattern_packages = find_package_roots_stream(ctx, recursive_packages).await;
    tokio::pin!(recursive_pattern_packages);
    while let Some(res) = recursive_pattern_packages.next().await {
        let package = res?;
        spec.add_package(&package);
        load_package_futs.push(load_package(ctx, package));
    }

    Ok((spec, load_package_futs))
}

pub struct LoadedPatterns<T> {
    results: BTreeMap<Package, SharedResult<BTreeMap<T, TargetNode>>>,
}

impl<T> LoadedPatterns<T> {
    pub fn iter(&self) -> impl Iterator<Item = (&Package, &SharedResult<BTreeMap<T, TargetNode>>)> {
        self.results.iter()
    }

    // Implementing IntoIterator requires explicitly specifying the iterator type, which seems higher cost than the value of doing it.
    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(
        self,
    ) -> impl Iterator<Item = (Package, SharedResult<BTreeMap<T, TargetNode>>)> {
        self.results.into_iter()
    }

    pub fn iter_loaded_targets(&self) -> impl Iterator<Item = SharedResult<&TargetNode>> {
        self.results
            .values()
            .map(|result| match result {
                Ok(label_to_node) => Ok(label_to_node.values()),
                Err(e) => Err(e.dupe()),
            })
            .flatten_ok()
    }

    pub fn iter_loaded_targets_by_package(
        &self,
    ) -> impl Iterator<Item = (&Package, SharedResult<Vec<TargetNode>>)> {
        self.results.iter().map(|(package, result)| {
            let targets = result
                .as_ref()
                .map(|label_to_node| label_to_node.values().map(|t| t.dupe()).collect::<Vec<_>>())
                .map_err(|e| e.dupe());
            (package, targets)
        })
    }
}

/// Finds all the requested targets in `spec` from a map of loaded targets in `load_result`.
fn apply_spec<T: PatternType>(
    spec: ResolvedPattern<T>,
    load_results: BTreeMap<Package, SharedResult<Arc<EvaluationResult>>>,
) -> anyhow::Result<LoadedPatterns<T>> {
    let mut all_targets = BTreeMap::new();
    for (pkg, pkg_spec) in spec.specs.into_iter() {
        let result = match load_results.get(&pkg) {
            Some(r) => r,
            None => return Err(anyhow::anyhow!(BuildErrors::MissingPackage(pkg))),
        };
        match result {
            Ok(res) => {
                let mut label_to_node = BTreeMap::new();
                match pkg_spec {
                    PackageSpec::Targets(targets) => {
                        for target in targets {
                            match res.targets().get(target.target()) {
                                Some(node) => {
                                    label_to_node.insert(target, node.dupe());
                                }
                                None => {
                                    return Err(anyhow::anyhow!(BuildErrors::MissingTarget(
                                        pkg,
                                        target.target().to_owned()
                                    )));
                                }
                            }
                        }
                    }
                    PackageSpec::All => {
                        for target_info in res.targets().values() {
                            let key = T::from_parts(
                                target_info.label().name().dupe(),
                                Default::default(),
                            );
                            label_to_node.insert(key, target_info.dupe());
                        }
                    }
                };
                all_targets.insert(pkg, Ok(label_to_node));
            }
            Err(e) => {
                all_targets.insert(pkg, Err(e.dupe()));
            }
        }
    }
    Ok(LoadedPatterns {
        results: all_targets,
    })
}

pub async fn load_patterns<T: PatternType>(
    ctx: &DiceComputations,
    parsed_patterns: Vec<ParsedPattern<T>>,
) -> anyhow::Result<LoadedPatterns<T>> {
    let (spec, mut load_package_futs) =
        resolve_patterns_and_load_buildfiles(ctx, parsed_patterns).await?;

    let mut results: BTreeMap<Package, SharedResult<Arc<EvaluationResult>>> = BTreeMap::new();
    while let Some((pkg, load_res)) = load_package_futs.next().await {
        results.insert(pkg, load_res);
    }

    apply_spec(spec, results)
}
