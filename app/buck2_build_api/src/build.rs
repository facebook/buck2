/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::sync::Arc;

use allocative::Allocative;
use buck2_common::liveliness_observer::LivelinessObserver;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::execution_types::executor_config::PathSeparatorKind;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::console_message;
use buck2_execute::artifact::fs::ExecutorFs;
use dice::LinearRecomputeDiceComputations;
use dice::UserComputationData;
use dupe::Dupe;
use dupe::OptionDupedExt;
use futures::future::Either;
use futures::stream::BoxStream;
use futures::stream::FuturesUnordered;
use futures::stream::Stream;
use futures::stream::StreamExt;
use futures::FutureExt;
use itertools::Itertools;
use tokio::sync::Mutex;

use crate::actions::artifact::get_artifact_fs::GetArtifactFs;
use crate::actions::calculation::get_target_rule_type_name;
use crate::actions::calculation::BuildKey;
use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::artifact_groups::calculation::EnsureTransitiveSetProjectionKey;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::artifact_groups::ResolvedArtifactGroup;
use crate::artifact_groups::ResolvedArtifactGroupBuildSignalsKey;
use crate::build_signals::HasBuildSignals;
use crate::interpreter::rule_defs::cmd_args::AbsCommandLineContext;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use crate::interpreter::rule_defs::provider::builtin::run_info::FrozenRunInfo;
use crate::interpreter::rule_defs::provider::test_provider::TestProvider;
use crate::keep_going::KeepGoing;
use crate::materialize::materialize_and_upload_artifact_group;
use crate::materialize::MaterializationAndUploadContext;
use crate::validation::validation_impl::VALIDATION_IMPL;

mod action_error;
pub mod build_report;
mod graph_size;

/// The types of provider to build on the configured providers label
#[derive(Debug, Clone, Dupe, Allocative, PartialEq)]
pub enum BuildProviderType {
    Default,
    DefaultOther,
    Run,
    Test,
}

#[derive(Clone, Debug, Allocative)]
pub struct ConfiguredBuildTargetResultGen<T> {
    pub outputs: Vec<T>,
    pub run_args: Option<Vec<String>>,
    pub target_rule_type_name: Option<String>,
    pub configured_graph_size: Option<buck2_error::Result<MaybeCompatible<u64>>>,
    pub errors: Vec<buck2_error::Error>,
}

pub type ConfiguredBuildTargetResult =
    ConfiguredBuildTargetResultGen<buck2_error::Result<ProviderArtifacts>>;

pub struct BuildTargetResult {
    pub configured: BTreeMap<ConfiguredProvidersLabel, Option<ConfiguredBuildTargetResult>>,
    /// Errors that could not be associated with a specific configured target. These errors may be
    /// associated with a providers label, or might not be associated with any target at all.
    pub other_errors: BTreeMap<Option<ProvidersLabel>, Vec<buck2_error::Error>>,
    pub build_failed: bool,
}

impl BuildTargetResult {
    pub fn new() -> Self {
        Self {
            configured: BTreeMap::new(),
            other_errors: BTreeMap::new(),
            build_failed: false,
        }
    }

    pub fn extend(&mut self, other: BuildTargetResult) {
        self.configured.extend(other.configured);
        self.other_errors.extend(other.other_errors);
    }

    pub fn is_empty(&self) -> bool {
        self.configured.is_empty() && self.other_errors.is_empty()
    }

    pub async fn collect_stream(
        mut stream: impl Stream<Item = BuildEvent> + Unpin,
        fail_fast: bool,
    ) -> buck2_error::Result<Self> {
        // Create a map of labels to outputs, but retain the expected index of each output.
        let mut res = HashMap::<
            ConfiguredProvidersLabel,
            Option<ConfiguredBuildTargetResultGen<(usize, buck2_error::Result<ProviderArtifacts>)>>,
        >::new();
        let mut other_errors = BTreeMap::<_, Vec<_>>::new();
        let mut build_failed = false;

        while let Some(event) = stream.next().await {
            let ConfiguredBuildEvent { variant, label } = match event {
                BuildEvent::Configured(variant) => variant,
                BuildEvent::OtherError { label: target, err } => {
                    other_errors.entry(target).or_default().push(err);
                    build_failed = true;
                    continue;
                }
            };
            match variant {
                ConfiguredBuildEventVariant::SkippedIncompatible => {
                    res.entry((*label).dupe()).or_insert(None);
                }
                ConfiguredBuildEventVariant::Prepared {
                    run_args,
                    target_rule_type_name,
                } => {
                    res.entry((*label).dupe())
                        .or_insert(Some(ConfiguredBuildTargetResultGen {
                            outputs: Vec::new(),
                            run_args,
                            target_rule_type_name: Some(target_rule_type_name),
                            configured_graph_size: None,
                            errors: Vec::new(),
                        }));
                }
                ConfiguredBuildEventVariant::Execution(execution_variant) => {
                    let is_err = {
                        let results = res.get_mut(label.as_ref())
                            .with_internal_error(|| format!("ConfiguredBuildEventVariant::Execution before ConfiguredBuildEventVariant::Prepared for {}", label))?
                            .as_mut()
                            .with_internal_error(|| format!("ConfiguredBuildEventVariant::Execution for a skipped target: `{}`", label))?;
                        match execution_variant {
                            ConfiguredBuildEventExecutionVariant::Validation { result } => {
                                if let Err(e) = result {
                                    results.errors.push(e);
                                    true
                                } else {
                                    false
                                }
                            }
                            ConfiguredBuildEventExecutionVariant::BuildOutput { index, output } => {
                                let is_err = output.is_err();
                                results.outputs.push((index, output));
                                is_err
                            }
                        }
                    };
                    if is_err {
                        build_failed = true;
                        if fail_fast {
                            break;
                        }
                    }
                }
                ConfiguredBuildEventVariant::GraphSize {
                    configured_graph_size,
                } => {
                    res.get_mut(label.as_ref())
                         .with_internal_error(|| format!("ConfiguredBuildEventVariant::GraphSize before ConfiguredBuildEventVariant::Prepared for {}", label))?
                         .as_mut()
                         .with_internal_error(|| format!("ConfiguredBuildEventVariant::GraphSize for a skipped target: `{}`", label))?
                         .configured_graph_size = Some(configured_graph_size);
                }
                ConfiguredBuildEventVariant::Timeout => {
                    res.get_mut(label.as_ref())
                         .with_internal_error(|| format!("ConfiguredBuildEventVariant::Timeout before ConfiguredBuildEventVariant::Prepared for {}", label))?
                         .as_mut()
                         .with_internal_error(|| format!("ConfiguredBuildEventVariant::Timeout for a skipped target: `{}`", label))?
                         .errors.push(buck2_error::Error::from(BuildDeadlineExpired));
                    build_failed = true;
                }
                ConfiguredBuildEventVariant::Error { err } => {
                    build_failed = true;
                    res.entry((*label).dupe())
                        .or_insert(Some(ConfiguredBuildTargetResultGen {
                            outputs: Vec::new(),
                            run_args: None,
                            target_rule_type_name: None,
                            configured_graph_size: None,
                            errors: Vec::new(),
                        }))
                        .as_mut()
                        .unwrap()
                        .errors
                        .push(err);
                    if fail_fast {
                        break;
                    }
                }
            }
        }

        // Sort our outputs within each individual BuildTargetResult, then return those.
        // Also, turn our HashMap into a BTreeMap.
        let res = res
            .into_iter()
            .map(|(label, result)| {
                let result = result.map(|result| {
                    let ConfiguredBuildTargetResultGen {
                        mut outputs,
                        run_args,
                        target_rule_type_name,
                        configured_graph_size,
                        errors,
                    } = result;

                    // No need for a stable sort: the indices are unique (see below).
                    outputs.sort_unstable_by_key(|(index, _outputs)| *index);

                    // TODO: This whole building thing needs quite a bit of refactoring. We might
                    // request the same targets multiple times here, but since we know that
                    // ConfiguredTargetLabel -> Output is going to be deterministic, we just dedupe
                    // them using the index.
                    ConfiguredBuildTargetResult {
                        outputs: outputs
                            .into_iter()
                            .unique_by(|(index, _outputs)| *index)
                            .map(|(_index, outputs)| outputs)
                            .collect(),
                        run_args,
                        target_rule_type_name,
                        configured_graph_size,
                        errors,
                    }
                });

                (label, result)
            })
            .collect();

        Ok(Self {
            configured: res,
            other_errors,
            build_failed,
        })
    }
}

pub enum ConfiguredBuildEventExecutionVariant {
    BuildOutput {
        output: buck2_error::Result<ProviderArtifacts>,
        /// Ensure a stable ordering of outputs.
        index: usize,
    },
    Validation {
        result: buck2_error::Result<()>,
    },
}

pub enum ConfiguredBuildEventVariant {
    SkippedIncompatible,
    Prepared {
        run_args: Option<Vec<String>>,
        target_rule_type_name: String,
    },
    Execution(ConfiguredBuildEventExecutionVariant),
    GraphSize {
        configured_graph_size: buck2_error::Result<MaybeCompatible<u64>>,
    },
    Error {
        /// An error that can't be associated with a single artifact.
        err: buck2_error::Error,
    },
    // This target did not build within the allocated time.
    Timeout,
}

/// Events to be accumulated using BuildTargetResult::collect_stream.
pub struct ConfiguredBuildEvent {
    label: Arc<ConfiguredProvidersLabel>,
    variant: ConfiguredBuildEventVariant,
}

pub enum BuildEvent {
    Configured(ConfiguredBuildEvent),
    // An error that cannot be associated with a specific configured target
    OtherError {
        label: Option<ProvidersLabel>,
        err: buck2_error::Error,
    },
}

impl BuildEvent {
    pub fn new_configured(
        label: ConfiguredProvidersLabel,
        variant: ConfiguredBuildEventVariant,
    ) -> Self {
        Self::Configured(ConfiguredBuildEvent {
            label: Arc::new(label),
            variant,
        })
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = BuildDeadlineExpired)]
#[error("Build timed out")]
struct BuildDeadlineExpired;

#[derive(Copy, Clone, Dupe, Debug)]
pub struct BuildConfiguredLabelOptions {
    pub skippable: bool,
    pub want_configured_graph_size: bool,
}

pub async fn build_configured_label<'a>(
    ctx: &'a LinearRecomputeDiceComputations<'_>,
    materialization_and_upload: &'a MaterializationAndUploadContext,
    providers_label: ConfiguredProvidersLabel,
    providers_to_build: &ProvidersToBuild,
    opts: BuildConfiguredLabelOptions,
    timeout_observer: Option<&'a Arc<dyn LivelinessObserver>>,
) -> BoxStream<'a, ConfiguredBuildEvent> {
    let providers_label = Arc::new(providers_label);
    build_configured_label_inner(
        ctx,
        materialization_and_upload,
        providers_label.dupe(),
        providers_to_build,
        opts,
        timeout_observer,
    )
    .await
    .unwrap_or_else(|e| {
        futures::stream::once(futures::future::ready(ConfiguredBuildEvent {
            label: providers_label,
            variant: ConfiguredBuildEventVariant::Error { err: e.into() },
        }))
        .boxed()
    })
}

async fn build_configured_label_inner<'a>(
    ctx: &'a LinearRecomputeDiceComputations<'_>,
    materialization_and_upload: &'a MaterializationAndUploadContext,
    providers_label: Arc<ConfiguredProvidersLabel>,
    providers_to_build: &ProvidersToBuild,
    opts: BuildConfiguredLabelOptions,
    timeout_observer: Option<&'a Arc<dyn LivelinessObserver>>,
) -> buck2_error::Result<BoxStream<'a, ConfiguredBuildEvent>> {
    let artifact_fs = ctx.get().get_artifact_fs().await?;

    let (outputs, run_args, target_rule_type_name) = {
        // A couple of these objects aren't Send and so scope them here so async transform doesn't get concerned.
        let providers = match ctx.get().get_providers(providers_label.as_ref()).await? {
            MaybeCompatible::Incompatible(reason) => {
                return if opts.skippable {
                    console_message(reason.skipping_message(providers_label.target()));
                    Ok(
                        futures::stream::once(futures::future::ready(ConfiguredBuildEvent {
                            label: providers_label.dupe(),
                            variant: ConfiguredBuildEventVariant::SkippedIncompatible,
                        }))
                        .boxed(),
                    )
                } else {
                    Err(reason.to_err().into())
                };
            }
            MaybeCompatible::Compatible(v) => v,
        };

        // Important we use an ordered collections, so the order matches the order the rule
        // author wrote.
        let mut outputs = Vec::new();
        // Providers that produced each output, in the order of outputs above. We use a separate collection
        // otherwise we'd build the same output twice when it's both in DefaultInfo and RunInfo
        let collection = providers.provider_collection();

        let mut run_args: Option<Vec<String>> = None;

        if providers_to_build.default {
            collection
                .default_info()?
                .for_each_default_output_artifact_only(&mut |o| {
                    outputs.push((ArtifactGroup::Artifact(o), BuildProviderType::Default))
                })?;
        }
        if providers_to_build.default_other {
            collection
                .default_info()?
                .for_each_default_output_other_artifacts_only(&mut |o| {
                    outputs.push((o, BuildProviderType::DefaultOther))
                })?;
            collection.default_info()?.for_each_other_output(&mut |o| {
                outputs.push((o, BuildProviderType::DefaultOther))
            })?;
        }
        if providers_to_build.run {
            if let Some(runinfo) = providers
                .provider_collection()
                .builtin_provider::<FrozenRunInfo>()
            {
                let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
                runinfo.visit_artifacts(&mut artifact_visitor)?;
                for input in artifact_visitor.inputs {
                    outputs.push((input, BuildProviderType::Run));
                }
                // Produce arguments to run on a local machine.
                let path_separator = if cfg!(windows) {
                    PathSeparatorKind::Windows
                } else {
                    PathSeparatorKind::Unix
                };
                let executor_fs = ExecutorFs::new(&artifact_fs, path_separator);
                let mut cli = Vec::<String>::new();
                let mut ctx = AbsCommandLineContext::new(&executor_fs);
                runinfo.add_to_command_line(&mut cli, &mut ctx)?;
                run_args = Some(cli);
            }
        }
        if providers_to_build.tests {
            if let Some(test_provider) = <dyn TestProvider>::from_collection(collection) {
                let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
                test_provider.visit_artifacts(&mut artifact_visitor)?;
                for input in artifact_visitor.inputs {
                    outputs.push((input, BuildProviderType::Test));
                }
            }
        }

        let target_rule_type_name =
            get_target_rule_type_name(&mut ctx.get(), providers_label.target()).await?;

        (outputs, run_args, target_rule_type_name)
    };

    if let Some(signals) = ctx
        .get()
        .per_transaction_data()
        .get_build_signals()
        .cloned()
    {
        let resolved_artifacts: Vec<_> =
            tokio::task::unconstrained(KeepGoing::try_compute_join_all(
                &mut ctx.get(),
                outputs.iter(),
                |ctx, (output, _type)| async move { output.resolved_artifact(ctx).await }.boxed(),
            ))
            .await?;
        let node_keys = resolved_artifacts
            .iter()
            .filter_map(|resolved| match resolved.dupe() {
                ResolvedArtifactGroup::Artifact(artifact) => artifact
                    .action_key()
                    .duped()
                    .map(BuildKey)
                    .map(ResolvedArtifactGroupBuildSignalsKey::BuildKey),
                ResolvedArtifactGroup::TransitiveSetProjection(key) => Some(
                    ResolvedArtifactGroupBuildSignalsKey::EnsureTransitiveSetProjectionKey(
                        EnsureTransitiveSetProjectionKey(key.dupe().dupe()),
                    ),
                ),
            })
            .collect();

        signals.top_level_target(providers_label.target().dupe(), node_keys);
    }

    if !opts.skippable && outputs.is_empty() {
        let docs = "https://buck2.build/docs/users/faq/common_issues/#why-does-my-target-not-have-any-outputs"; // @oss-enable
        // @oss-disable: let docs = "https://www.internalfb.com/intern/staticdocs/buck2/docs/users/faq/common_issues/#why-does-my-target-not-have-any-outputs";
        console_message(format!(
            "Target {} does not have any outputs. This means the rule did not define any outputs. See {} for more information",
            providers_label.target(),
            docs,
        ));
    }

    let mut outputs = outputs
        .into_iter()
        .enumerate()
        .map({
            |(index, (output, provider_type))| {
                let materialization_and_upload = materialization_and_upload.dupe();
                Either::Left(async move {
                    let res = match materialize_and_upload_artifact_group(
                        &mut ctx.get(),
                        &output,
                        &materialization_and_upload,
                    )
                    .await
                    {
                        Ok(values) => Ok(ProviderArtifacts {
                            values,
                            provider_type,
                        }),
                        Err(e) => Err(buck2_error::Error::from(e)),
                    };
                    ConfiguredBuildEventExecutionVariant::BuildOutput { index, output: res }
                })
            }
        })
        .collect::<Vec<_>>();

    let validation_result = {
        let validation_impl = VALIDATION_IMPL.get()?;
        let mut ctx = ctx.get();
        let target = providers_label.target().dupe();
        Either::Right(async move {
            let result = validation_impl
                .validate_target_node_transitively(&mut ctx, target)
                .await;
            ConfiguredBuildEventExecutionVariant::Validation { result }
        })
    };

    outputs.push(validation_result);

    let outputs = outputs
        .into_iter()
        .map(|build| {
            let providers_label = providers_label.dupe();
            async move {
                let build = build.map(ConfiguredBuildEventVariant::Execution);

                let variant = match timeout_observer {
                    Some(timeout_observer) => {
                        let alive = timeout_observer
                            .while_alive()
                            .map(|()| ConfiguredBuildEventVariant::Timeout);
                        futures::pin_mut!(alive);
                        futures::pin_mut!(build);
                        futures::future::select(alive, build)
                            .map(|r| r.factor_first().0)
                            .await
                    }
                    None => build.await,
                };

                ConfiguredBuildEvent {
                    label: providers_label.dupe(),
                    variant,
                }
            }
        })
        .collect::<FuturesUnordered<_>>();

    let stream = futures::stream::once(futures::future::ready(ConfiguredBuildEvent {
        label: providers_label.dupe(),
        variant: ConfiguredBuildEventVariant::Prepared {
            run_args,
            target_rule_type_name,
        },
    }))
    .chain(outputs);

    if opts.want_configured_graph_size {
        let stream = stream.chain(futures::stream::once(async move {
            let configured_graph_size =
                graph_size::get_configured_graph_size(&mut ctx.get(), providers_label.target())
                    .await
                    .map_err(|e| e.into());

            ConfiguredBuildEvent {
                label: providers_label,
                variant: ConfiguredBuildEventVariant::GraphSize {
                    configured_graph_size,
                },
            }
        }));

        Ok(stream.boxed())
    } else {
        Ok(stream.boxed())
    }
}

#[derive(Clone, Allocative)]
pub struct ProviderArtifacts {
    pub values: ArtifactGroupValues,
    pub provider_type: BuildProviderType,
}

// what type of artifacts to build based on the provider it came from
#[derive(Default, Clone)]
pub struct ProvidersToBuild {
    pub default: bool,
    pub default_other: bool,
    pub run: bool,
    pub tests: bool,
}

impl Debug for ProviderArtifacts {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ProviderArtifacts")
            .field("values", &self.values.iter().collect::<Vec<_>>())
            .field("provider_type", &self.provider_type)
            .finish()
    }
}

pub trait HasCreateUnhashedSymlinkLock {
    fn set_create_unhashed_symlink_lock(&mut self, lock: Arc<Mutex<()>>);

    fn get_create_unhashed_symlink_lock(&self) -> Arc<Mutex<()>>;
}

impl HasCreateUnhashedSymlinkLock for UserComputationData {
    fn set_create_unhashed_symlink_lock(&mut self, lock: Arc<Mutex<()>>) {
        self.data.set(lock);
    }

    fn get_create_unhashed_symlink_lock(&self) -> Arc<Mutex<()>> {
        self.data
            .get::<Arc<Mutex<()>>>()
            .expect("Lock for creating unhashed symlinks should be set")
            .dupe()
    }
}
