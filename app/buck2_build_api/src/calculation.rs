/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::fmt::Write;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::cycles::CycleAdapterDescriptor;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::pattern::package_roots::find_package_roots_stream;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::buck_path::resolver::BuckPathResolver;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::buck_out_path::BuckOutPathResolver;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern_type::PatternType;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ParsedPattern;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_core::target::label::TargetLabel;
use buck2_core::target::name::TargetName;
use buck2_events::dispatch::console_message;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::unconfigured::RuleKind;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_util::cycle_detector::CycleDescriptor;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::future::FutureExt;
use futures::stream::FuturesUnordered;
use futures::Stream;
use futures::StreamExt;
use gazebo::prelude::*;
use itertools::Itertools;
use thiserror::Error;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::calculation as action_calculation;
use crate::actions::execute::action_executor::ActionOutputs;
use crate::actions::key::ActionKey;
use crate::configuration::calculation::ConfigurationCalculation;
use crate::context::HasBuildContextData;
use crate::nodes::calculation::get_execution_platform_toolchain_dep;
use crate::nodes::calculation::ConfiguredTargetNodeKey;
use crate::nodes::calculation::NodeCalculation;

#[derive(Debug, Error)]
enum BuildErrors {
    #[error("Did not find package with name `{0}`.")]
    MissingPackage(PackageLabel),
}

pub trait ConfigurableTargetLabel: Send + Sync {
    type Configured;

    fn target(&self) -> &TargetLabel;

    fn configure(&self, cfg: ConfigurationData) -> Self::Configured;

    fn configure_with_exec(
        &self,
        cfg: ConfigurationData,
        exec_cfg: ConfigurationData,
    ) -> Self::Configured;
}

impl ConfigurableTargetLabel for TargetLabel {
    type Configured = ConfiguredTargetLabel;

    fn target(&self) -> &TargetLabel {
        self
    }

    fn configure(&self, cfg: ConfigurationData) -> Self::Configured {
        self.configure(cfg)
    }

    fn configure_with_exec(
        &self,
        cfg: ConfigurationData,
        exec_cfg: ConfigurationData,
    ) -> Self::Configured {
        self.configure_with_exec(cfg, exec_cfg)
    }
}

impl ConfigurableTargetLabel for ProvidersLabel {
    type Configured = ConfiguredProvidersLabel;

    fn target(&self) -> &TargetLabel {
        self.target()
    }

    fn configure(&self, cfg: ConfigurationData) -> Self::Configured {
        self.configure(cfg)
    }

    fn configure_with_exec(
        &self,
        cfg: ConfigurationData,
        exec_cfg: ConfigurationData,
    ) -> Self::Configured {
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
    async fn get_artifact_fs(&self) -> anyhow::Result<ArtifactFs>;

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
    async fn get_configured_target<T: ConfigurableTargetLabel>(
        &self,
        target: &T,
        global_target_platform: Option<&TargetLabel>,
    ) -> anyhow::Result<T::Configured>;

    async fn get_default_configured_target<T: ConfigurableTargetLabel>(
        &self,
        target: &T,
    ) -> anyhow::Result<T::Configured>;

    /// Builds a specific 'Action' given the 'Action' itself
    async fn build_action(&self, action_key: &ActionKey) -> anyhow::Result<ActionOutputs>;

    /// Builds and materializes the given 'BuildArtifact'
    async fn build_artifact(&self, artifact: &BuildArtifact) -> anyhow::Result<ActionOutputs>;
}

#[async_trait]
impl<'c> Calculation<'c> for DiceComputations {
    async fn get_artifact_fs(&self) -> anyhow::Result<ArtifactFs> {
        let buck_out_path_resolver = BuckOutPathResolver::new(self.get_buck_out_path().await?);
        let project_filesystem = self.global_data().get_io_provider().project_root().dupe();
        let buck_path_resolver = BuckPathResolver::new(self.get_cell_resolver().await?);
        Ok(ArtifactFs::new(
            buck_path_resolver,
            buck_out_path_resolver,
            project_filesystem,
        ))
    }

    async fn get_configured_target<T: ConfigurableTargetLabel>(
        &self,
        target: &T,
        global_target_platform: Option<&TargetLabel>,
    ) -> anyhow::Result<T::Configured> {
        let node = self.get_target_node(target.target()).await?;

        let get_platform_configuration = async || -> SharedResult<ConfigurationData> {
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
            RuleKind::Configuration => Ok(target.configure(ConfigurationData::unbound())),
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
                Ok(target.configure_with_exec(cfg, exec_cfg.cfg().dupe()))
            }
        }
    }

    async fn get_default_configured_target<T: ConfigurableTargetLabel>(
        &self,
        target: &T,
    ) -> anyhow::Result<T::Configured> {
        self.get_configured_target(target, None).await
    }

    async fn build_action(&self, action_key: &ActionKey) -> anyhow::Result<ActionOutputs> {
        action_calculation::ActionCalculation::build_action(self, action_key).await
    }

    async fn build_artifact(&self, artifact: &BuildArtifact) -> anyhow::Result<ActionOutputs> {
        action_calculation::ActionCalculation::build_artifact(self, artifact).await
    }
}

async fn resolve_patterns_and_load_buildfiles<'c, T: PatternType>(
    ctx: &'c DiceComputations,
    parsed_patterns: Vec<ParsedPattern<T>>,
) -> anyhow::Result<(
    ResolvedPattern<T>,
    impl Stream<Item = (PackageLabel, anyhow::Result<Arc<EvaluationResult>>)> + 'c,
)> {
    let mut spec = ResolvedPattern::<T>::new();
    let mut recursive_packages = Vec::new();

    struct Builder<'c> {
        ctx: &'c DiceComputations,
        load_package_futs:
            FuturesUnordered<BoxFuture<'c, (PackageLabel, anyhow::Result<Arc<EvaluationResult>>)>>,
    }

    let mut builder = Builder {
        ctx,
        load_package_futs: FuturesUnordered::new(),
    };

    impl<'c> Builder<'c> {
        fn load_package(&mut self, package: PackageLabel) {
            // it's important that this is not async and the temporary spawn happens when the function is called as we don't immediately start polling these.
            self.load_package_futs.push(
                self.ctx
                    .temporary_spawn(move |ctx, _cancellation| {
                        async move {
                            let res = ctx.get_interpreter_results(package.dupe()).await;
                            (package, res)
                        }
                        .boxed()
                    })
                    .boxed(),
            )
        }
    }

    for pattern in parsed_patterns {
        match pattern {
            ParsedPattern::Target(package, target_name, extra) => {
                spec.add_target(package.dupe(), target_name, extra);
                builder.load_package(package.dupe());
            }
            ParsedPattern::Package(package) => {
                spec.add_package(package.dupe());
                builder.load_package(package.dupe());
            }
            ParsedPattern::Recursive(package) => {
                recursive_packages.push(package);
            }
        }
    }

    let mut recursive_pattern_packages = find_package_roots_stream(ctx, recursive_packages);
    while let Some(res) = recursive_pattern_packages.next().await {
        let package = res?;
        spec.add_package(package.dupe());
        builder.load_package(package);
    }

    Ok((spec, builder.load_package_futs))
}

pub struct LoadedPatterns<T: PatternType> {
    results: BTreeMap<PackageLabel, SharedResult<BTreeMap<(TargetName, T), TargetNode>>>,
}

impl<T: PatternType> LoadedPatterns<T> {
    pub fn iter(
        &self,
    ) -> impl Iterator<
        Item = (
            PackageLabel,
            &SharedResult<BTreeMap<(TargetName, T), TargetNode>>,
        ),
    > {
        self.results.iter().map(|(k, v)| (k.dupe(), v))
    }

    // Implementing IntoIterator requires explicitly specifying the iterator type, which seems higher cost than the value of doing it.
    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(
        self,
    ) -> impl Iterator<
        Item = (
            PackageLabel,
            SharedResult<BTreeMap<(TargetName, T), TargetNode>>,
        ),
    > {
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
    ) -> impl Iterator<Item = (PackageLabel, anyhow::Result<Vec<TargetNode>>)> + '_ {
        self.results.iter().map(|(package, result)| {
            let targets = result
                .as_ref()
                .map(|label_to_node| label_to_node.values().map(|t| t.dupe()).collect::<Vec<_>>())
                .map_err(|e| anyhow::Error::new(e.dupe()));
            (package.dupe(), targets)
        })
    }
}

/// Option to skip missing targets instead of failing.
/// This is not a good option to use long term, but we need it now to deal with our legacy setup.
#[derive(Clone, Dupe, Copy, Eq, PartialEq, Debug)]
pub enum MissingTargetBehavior {
    /// Skip missing targets (but error on missing packages or evaluation errors).
    /// When skipping, we emit a warning to the console.
    Warn,
    Fail,
}

impl MissingTargetBehavior {
    pub fn from_skip(skip: bool) -> MissingTargetBehavior {
        if skip {
            MissingTargetBehavior::Warn
        } else {
            MissingTargetBehavior::Fail
        }
    }
}

/// Finds all the requested targets in `spec` from a map of loaded targets in `load_result`.
fn apply_spec<T: PatternType>(
    spec: ResolvedPattern<T>,
    load_results: BTreeMap<PackageLabel, SharedResult<Arc<EvaluationResult>>>,
    skip_missing_targets: MissingTargetBehavior,
) -> anyhow::Result<LoadedPatterns<T>> {
    let mut all_targets: BTreeMap<_, SharedResult<BTreeMap<(TargetName, T), _>>> = BTreeMap::new();
    let mut missing_targets: Vec<TargetLabel> = Vec::new();
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
                        for (target_name, extra) in targets {
                            match skip_missing_targets {
                                MissingTargetBehavior::Warn => {
                                    let node = res.get_target(target_name.as_ref());
                                    match node {
                                        Some(node) => {
                                            label_to_node.insert((target_name, extra), node.dupe());
                                        }
                                        None => missing_targets.push(TargetLabel::new(
                                            pkg.dupe(),
                                            target_name.as_ref(),
                                        )),
                                    }
                                }
                                MissingTargetBehavior::Fail => {
                                    let node = res.resolve_target(target_name.as_ref())?;
                                    label_to_node.insert((target_name, extra), node.dupe());
                                }
                            }
                        }
                    }
                    PackageSpec::All => {
                        for target_info in res.targets().values() {
                            label_to_node.insert(
                                (target_info.label().name().to_owned(), T::default()),
                                target_info.dupe(),
                            );
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

    if !missing_targets.is_empty() {
        console_message(missing_targets_message(missing_targets));
    }

    Ok(LoadedPatterns {
        results: all_targets,
    })
}

/// Warning message emitted when missing targets are skipped.
fn missing_targets_message(mut missing_targets: Vec<TargetLabel>) -> String {
    missing_targets.sort_unstable();
    let (head, middle, tail): (&[TargetLabel], &str, &[TargetLabel]) = if missing_targets.len() > 15
    {
        let head = &missing_targets[..5];
        let tail = &missing_targets[missing_targets.len() - 5..];
        (head, "...", tail)
    } else {
        (&missing_targets, "", &[])
    };
    let mut message = String::new();
    writeln!(
        message,
        "Skipped {} missing targets:",
        missing_targets.len()
    )
    .unwrap();
    for target in head {
        writeln!(message, "  {}", target).unwrap();
    }
    if !middle.is_empty() {
        writeln!(message, "  {}", middle).unwrap();
    }
    for target in tail {
        writeln!(message, "  {}", target).unwrap();
    }
    message
}

pub async fn load_patterns<T: PatternType>(
    ctx: &DiceComputations,
    parsed_patterns: Vec<ParsedPattern<T>>,
    skip_missing_targets: MissingTargetBehavior,
) -> anyhow::Result<LoadedPatterns<T>> {
    let (spec, mut load_package_futs) =
        resolve_patterns_and_load_buildfiles(ctx, parsed_patterns).await?;

    let mut results: BTreeMap<PackageLabel, SharedResult<Arc<EvaluationResult>>> = BTreeMap::new();
    while let Some((pkg, load_res)) = load_package_futs.next().await {
        results.insert(pkg, load_res.shared_error());
    }

    apply_spec(spec, results, skip_missing_targets)
}

#[derive(Error, Debug, Clone, Dupe)]
pub struct ConfiguredGraphCycleError {
    cycle: Arc<Vec<ConfiguredGraphCycleKeys>>,
}

impl std::fmt::Display for ConfiguredGraphCycleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Configured target cycle detected (`->` means \"depends on\"):"
        )?;
        for p in self.cycle.iter() {
            writeln!(f, "  {} ->", p)?;
        }
        // point back at the first item in the cycle.
        writeln!(f, "  {}", self.cycle.first().unwrap())?;
        Ok(())
    }
}

// TODO(cjhopman): There's other keys that could be involved in a cycle in the configured graph and they should probably also be tracked
// here. Would be good to check on things like transitions, toolchains, configuration nodes. Still, this will currently catch most
// configured graph cycles.
#[derive(Debug, Display, Clone, Eq, PartialEq, Hash)]
pub enum ConfiguredGraphCycleKeys {
    #[display(fmt = "{}", _0)]
    ConfiguredTargetNode(ConfiguredTargetNodeKey),
}

#[derive(Debug)]
pub struct ConfiguredGraphCycleDescriptor;

impl CycleDescriptor for ConfiguredGraphCycleDescriptor {
    type Key = ConfiguredGraphCycleKeys;

    type Error = ConfiguredGraphCycleError;

    fn cycle_error(cycle: Vec<&Self::Key>) -> Self::Error {
        ConfiguredGraphCycleError {
            cycle: Arc::new(cycle.cloned()),
        }
    }
}

impl CycleAdapterDescriptor for ConfiguredGraphCycleDescriptor {
    fn to_key(key: &dyn std::any::Any) -> Option<Self::Key> {
        if let Some(v) = key.downcast_ref::<ConfiguredTargetNodeKey>() {
            return Some(ConfiguredGraphCycleKeys::ConfiguredTargetNode(v.dupe()));
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::target::label::TargetLabel;

    use crate::calculation::missing_targets_message;

    #[test]
    fn test_missing_targets_message() {
        let targets: Vec<TargetLabel> = (0..30)
            .map(|i| TargetLabel::testing_parse(&format!("aaa//bbb:{i:02}")))
            .collect();
        assert_eq!(
            "Skipped 1 missing targets:\n  aaa//bbb:00\n",
            missing_targets_message(targets[..1].to_vec())
        );
        assert_eq!(
            "Skipped 20 missing targets:\
                \n  aaa//bbb:00\n  aaa//bbb:01\n  aaa//bbb:02\n  aaa//bbb:03\n  aaa//bbb:04\
                \n  ...\
                \n  aaa//bbb:15\n  aaa//bbb:16\n  aaa//bbb:17\n  aaa//bbb:18\n  aaa//bbb:19\
                \n",
            missing_targets_message(targets[..20].to_vec())
        );

        for i in 0..targets.len() {
            // Test it does not panic.
            missing_targets_message(targets[..i].to_vec());
        }
    }
}
