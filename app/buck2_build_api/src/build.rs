/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::future::Future;
use std::pin::pin;
use std::sync::Arc;

use allocative::Allocative;
use buck2_common::liveliness_observer::LivelinessObserver;
use buck2_core::configuration::compatibility::IncompatiblePlatformReason;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::pattern::pattern::Modifiers;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::console_message;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use dice::LinearRecomputeDiceComputations;
use dice::UserComputationData;
use dupe::Dupe;
use dupe::IterDupedExt;
use dupe::OptionDupedExt;
use futures::FutureExt;
use futures::future::Either;
use futures::stream::FuturesUnordered;
use futures::stream::StreamExt;
use itertools::Itertools;
use starlark::collections::SmallSet;
use tokio::sync::Mutex;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;

use crate::actions::calculation::BuildKey;
use crate::actions::calculation::get_target_rule_type_name;
use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::artifact_groups::ArtifactGroupValues;
use crate::artifact_groups::ResolvedArtifactGroup;
use crate::artifact_groups::ResolvedArtifactGroupBuildSignalsKey;
use crate::artifact_groups::calculation::EnsureTransitiveSetProjectionKey;
use crate::build::detailed_aggregated_metrics::dice::HasDetailedAggregatedMetrics;
use crate::build::detailed_aggregated_metrics::types::TopLevelTargetSpec;
use crate::build::graph_properties::GraphPropertiesOptions;
use crate::build::graph_properties::GraphPropertiesValues;
use crate::build::outputs::get_outputs_for_top_level_target;
use crate::build_signals::HasBuildSignals;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use crate::keep_going::KeepGoing;
use crate::materialize::MaterializationAndUploadContext;
use crate::materialize::materialize_and_upload_artifact_group;
use crate::validation::validation_impl::VALIDATION_IMPL;

mod action_error;
pub mod build_report;
pub mod detailed_aggregated_metrics;
pub mod graph_properties;
pub mod outputs;

/// The types of provider to build on the configured providers label
#[derive(Debug, Clone, Dupe, Copy, Allocative, PartialEq)]
pub enum BuildProviderType {
    Default,
    DefaultOther,
    Run,
    Test,
}

#[derive(Clone, Debug, Allocative)]
pub struct ConfiguredBuildTargetResultGen<T> {
    pub outputs: Vec<T>,
    pub provider_collection: Option<FrozenProviderCollectionValue>,
    pub target_rule_type_name: Option<String>,
    pub graph_properties: Option<buck2_error::Result<MaybeCompatible<GraphPropertiesValues>>>,
    pub errors: Vec<buck2_error::Error>,
}

pub type ConfiguredBuildTargetResult =
    ConfiguredBuildTargetResultGen<buck2_error::Result<ProviderArtifacts>>;

pub enum FailFastState {
    Continue,
    Breakpoint,
}

pub struct AsyncBuildTargetResultBuilder {
    event_rx: UnboundedReceiver<BuildEvent>,
    builder: BuildTargetResultBuilder,
}

impl AsyncBuildTargetResultBuilder {
    pub fn new(
        mut streaming_build_result_tx: Option<UnboundedSender<BuildTargetResult>>,
    ) -> (Self, impl BuildEventConsumer + Clone) {
        let (event_tx, event_rx) = tokio::sync::mpsc::unbounded_channel();
        #[derive(Clone)]
        struct EventConsumer {
            event_tx: UnboundedSender<BuildEvent>,
        }

        impl BuildEventConsumer for EventConsumer {
            fn consume(&self, ev: BuildEvent) {
                let _ignored = self.event_tx.send(ev);
            }
        }

        (
            Self {
                event_rx,
                builder: BuildTargetResultBuilder::new(streaming_build_result_tx.take()),
            },
            EventConsumer { event_tx },
        )
    }

    pub async fn wait_for(
        mut self,
        fail_fast: bool,
        fut: impl Future<Output = ()>,
    ) -> buck2_error::Result<BuildTargetResult> {
        let mut fut = pin!(fut);
        loop {
            tokio::select! {
                event = self.event_rx.recv() => {
                    match event {
                        Some(event) => {
                            if let FailFastState::Breakpoint = self.builder.event(event)? {
                                if fail_fast {
                                    break;
                                }
                            }
                        }
                        None => {
                            // Intentionally don't break early in this case.
                            // The None indicates that the event sender has been dropped, but a caller is going to expect
                            // the future to be driven to completion except for in the fail_fast case.
                        }
                    }
                }
                _ = &mut fut => {
                    // The future is done, but make sure to drain the queue of events.
                    // Unlike poll_recv, try_recv never spuriously returns empty.
                    while let Ok(event) = self.event_rx.try_recv() {
                        self.builder.event(event)?;
                    }
                    break;
                }
            }
        }

        Ok(self.builder.build())
    }
}

pub struct BuildTargetResultBuilder {
    res: HashMap<
        ConfiguredProvidersLabel,
        Option<ConfiguredBuildTargetResultGen<(usize, buck2_error::Result<ProviderArtifacts>)>>,
    >,
    configured_to_pattern_modifiers: HashMap<ConfiguredProvidersLabel, Vec<Modifiers>>,
    other_errors: BTreeMap<Option<ProvidersLabel>, Vec<buck2_error::Error>>,
    build_failed: bool,
    incompatible_targets: SmallSet<ConfiguredTargetLabel>,
    streaming_build_result_tx: Option<UnboundedSender<BuildTargetResult>>,
}

impl BuildTargetResultBuilder {
    pub fn new(mut streaming_build_result_tx: Option<UnboundedSender<BuildTargetResult>>) -> Self {
        Self {
            res: HashMap::new(),
            configured_to_pattern_modifiers: HashMap::new(),
            other_errors: BTreeMap::new(),
            incompatible_targets: SmallSet::new(),
            build_failed: false,
            streaming_build_result_tx: streaming_build_result_tx.take(),
        }
    }

    pub fn event(&mut self, event: BuildEvent) -> buck2_error::Result<FailFastState> {
        let ConfiguredBuildEvent { variant, label } = match event {
            BuildEvent::Configured(variant) => variant,
            BuildEvent::OtherError { label: target, err } => {
                self.other_errors.entry(target).or_default().push(err);
                self.build_failed = true;
                // TODO(cjhopman): Why don't we break here?
                return Ok(FailFastState::Continue);
            }
        };
        match variant {
            ConfiguredBuildEventVariant::SkippedIncompatible => {
                self.incompatible_targets.insert(label.target().dupe());
                self.res.entry(label.dupe()).or_insert(None);
            }
            ConfiguredBuildEventVariant::MapModifiers { modifiers } => {
                self.configured_to_pattern_modifiers
                    .entry(label.dupe())
                    .or_default()
                    .push(modifiers);
            }
            ConfiguredBuildEventVariant::Prepared {
                provider_collection,
                target_rule_type_name,
            } => {
                self.res
                    .entry(label.dupe())
                    .or_insert(Some(ConfiguredBuildTargetResultGen {
                        outputs: Vec::new(),
                        provider_collection,
                        target_rule_type_name: Some(target_rule_type_name),
                        graph_properties: None,
                        errors: Vec::new(),
                    }));
            }
            ConfiguredBuildEventVariant::Execution(execution_variant) => {
                let is_err = {
                    let results = self.res.get_mut(&label)
                        .with_internal_error(|| format!("ConfiguredBuildEventVariant::Execution before ConfiguredBuildEventVariant::Prepared for {label}"))?
                        .as_mut()
                        .with_internal_error(|| format!("ConfiguredBuildEventVariant::Execution for a skipped target: `{label}`"))?;
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
                            // update the streaming build result
                            if let Some(tx) = &self.streaming_build_result_tx.clone() {
                                let result = self.build();
                                let _ignored = tx.send(result);
                            }

                            is_err
                        }
                    }
                };
                if is_err {
                    self.build_failed = true;
                    return Ok(FailFastState::Breakpoint);
                }
            }
            ConfiguredBuildEventVariant::GraphProperties { graph_properties } => {
                self.res.get_mut(&label)
                     .with_internal_error(|| format!("ConfiguredBuildEventVariant::GraphProperties before ConfiguredBuildEventVariant::Prepared for {label}"))?
                     .as_mut()
                     .with_internal_error(|| format!("ConfiguredBuildEventVariant::GraphProperties for a skipped target: `{label}`"))?
                     .graph_properties = Some(graph_properties);
            }
            ConfiguredBuildEventVariant::Timeout => {
                self.res.get_mut(&label)
                     .with_internal_error(|| format!("ConfiguredBuildEventVariant::Timeout before ConfiguredBuildEventVariant::Prepared for {label}"))?
                     .as_mut()
                     .with_internal_error(|| format!("ConfiguredBuildEventVariant::Timeout for a skipped target: `{label}`"))?
                     .errors.push(buck2_error::Error::from(BuildDeadlineExpired));
                // TODO(cjhopman): Why don't we break here?
                self.build_failed = true;
            }
            ConfiguredBuildEventVariant::Error { err } => {
                self.build_failed = true;
                self.res
                    .entry(label.dupe())
                    .or_insert(Some(ConfiguredBuildTargetResultGen {
                        outputs: Vec::new(),
                        provider_collection: None,
                        target_rule_type_name: None,
                        graph_properties: None,
                        errors: Vec::new(),
                    }))
                    .as_mut()
                    .unwrap()
                    .errors
                    .push(err);
                return Ok(FailFastState::Breakpoint);
            }
        }
        Ok(FailFastState::Continue)
    }

    pub fn build(&self) -> BuildTargetResult {
        // This function can be called several times during a build in order to produce
        // intermediary/streaming build reports as well as the final build report.
        // It intentionally does not consume self and copies the arrays in the return object.

        if !self.incompatible_targets.is_empty() {
            // TODO(cjhopman): Probably better to return this in the result and let the caller decide what to do with it.
            console_message(IncompatiblePlatformReason::skipping_message_for_multiple(
                &self.incompatible_targets,
            ));
        }

        // Sort our outputs within each individual BuildTargetResult, then return those.
        // Also, turn our HashMap into a BTreeMap.
        let res = self
            .res
            .iter()
            .map(|(label, result)| {
                let result = result.as_ref().map(|result| {
                    let ConfiguredBuildTargetResultGen {
                        outputs,
                        provider_collection,
                        target_rule_type_name,
                        graph_properties,
                        errors,
                    } = result;

                    // No need for a stable sort: the indices are unique (see below).
                    let mut cloned_outputs = outputs.clone();
                    cloned_outputs.sort_unstable_by_key(|(index, _outputs)| *index);

                    // TODO: This whole building thing needs quite a bit of refactoring. We might
                    // request the same targets multiple times here, but since we know that
                    // ConfiguredTargetLabel -> Output is going to be deterministic, we just dedupe
                    // them using the index.
                    ConfiguredBuildTargetResult {
                        outputs: cloned_outputs
                            .into_iter()
                            .unique_by(|(index, _outputs)| *index)
                            .map(|(_index, outputs)| outputs)
                            .collect(),
                        provider_collection: provider_collection.clone(),
                        target_rule_type_name: target_rule_type_name.clone(),
                        graph_properties: graph_properties.clone(),
                        errors: errors.clone(),
                    }
                });

                (label.clone(), result)
            })
            .collect();

        let configured_to_pattern_modifiers = self
            .configured_to_pattern_modifiers
            .iter()
            .map(|(label, modifiers)| {
                (
                    label.clone(),
                    BTreeSet::from_iter(modifiers.iter().cloned()),
                )
            })
            .collect();

        BuildTargetResult {
            configured: res,
            configured_to_pattern_modifiers,
            other_errors: self.other_errors.clone(),
            build_failed: self.build_failed,
        }
    }
}

pub struct BuildTargetResult {
    pub configured: BTreeMap<ConfiguredProvidersLabel, Option<ConfiguredBuildTargetResult>>,
    pub configured_to_pattern_modifiers: HashMap<ConfiguredProvidersLabel, BTreeSet<Modifiers>>,
    /// Errors that could not be associated with a specific configured target. These errors may be
    /// associated with a providers label, or might not be associated with any target at all.
    pub other_errors: BTreeMap<Option<ProvidersLabel>, Vec<buck2_error::Error>>,
    pub build_failed: bool,
}

impl BuildTargetResult {
    pub fn new() -> Self {
        Self {
            configured: BTreeMap::new(),
            configured_to_pattern_modifiers: HashMap::new(),
            other_errors: BTreeMap::new(),
            build_failed: false,
        }
    }

    pub fn extend(&mut self, other: BuildTargetResult) {
        self.configured.extend(other.configured);
        self.other_errors.extend(other.other_errors);

        for (label, modifiers_set) in other.configured_to_pattern_modifiers {
            self.configured_to_pattern_modifiers
                .entry(label)
                .or_default()
                .extend(modifiers_set);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.configured.is_empty() && self.other_errors.is_empty()
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
    MapModifiers {
        modifiers: Modifiers,
    },
    Prepared {
        provider_collection: Option<FrozenProviderCollectionValue>,
        target_rule_type_name: String,
    },
    Execution(ConfiguredBuildEventExecutionVariant),
    GraphProperties {
        graph_properties: buck2_error::Result<MaybeCompatible<GraphPropertiesValues>>,
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
    label: ConfiguredProvidersLabel,
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
        Self::Configured(ConfiguredBuildEvent { label, variant })
    }
}

pub trait BuildEventConsumer: Sync {
    fn consume(&self, ev: BuildEvent);
    fn consume_configured(&self, ev: ConfiguredBuildEvent) {
        self.consume(BuildEvent::Configured(ev))
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = BuildDeadlineExpired)]
#[error("Build timed out")]
struct BuildDeadlineExpired;

#[derive(Copy, Clone, Dupe, Debug)]
pub struct BuildConfiguredLabelOptions {
    pub skippable: bool,
    pub graph_properties: GraphPropertiesOptions,
}

pub async fn build_configured_label(
    event_consumer: &dyn BuildEventConsumer,
    ctx: &LinearRecomputeDiceComputations<'_>,
    materialization_and_upload: &MaterializationAndUploadContext,
    providers_label: ConfiguredProvidersLabel,
    providers_to_build: &ProvidersToBuild,
    opts: BuildConfiguredLabelOptions,
    timeout_observer: Option<&Arc<dyn LivelinessObserver>>,
) {
    if let Err(e) = build_configured_label_inner(
        event_consumer,
        ctx,
        materialization_and_upload,
        providers_label.dupe(),
        providers_to_build,
        opts,
        timeout_observer,
    )
    .await
    {
        event_consumer.consume_configured(ConfiguredBuildEvent {
            label: providers_label,
            variant: ConfiguredBuildEventVariant::Error { err: e.into() },
        });
    }
}

async fn build_configured_label_inner<'a>(
    event_consumer: &dyn BuildEventConsumer,
    ctx: &'a LinearRecomputeDiceComputations<'_>,
    materialization_and_upload: &'a MaterializationAndUploadContext,
    providers_label: ConfiguredProvidersLabel,
    providers_to_build: &ProvidersToBuild,
    opts: BuildConfiguredLabelOptions,
    timeout_observer: Option<&'a Arc<dyn LivelinessObserver>>,
) -> buck2_error::Result<()> {
    let outputs = match get_outputs_for_top_level_target(
        &mut ctx.get(),
        &providers_label,
        providers_to_build,
    )
    .await?
    {
        MaybeCompatible::Incompatible(reason) => {
            if opts.skippable {
                tracing::debug!("{}", reason.skipping_message(providers_label.target()));
                event_consumer.consume_configured(ConfiguredBuildEvent {
                    label: providers_label.dupe(),
                    variant: ConfiguredBuildEventVariant::SkippedIncompatible,
                });
                return Ok(());
            } else {
                return Err(reason.to_err().into());
            };
        }
        MaybeCompatible::Compatible(v) => v,
    };

    let node = ctx
        .get()
        .get_configured_target_node(providers_label.target())
        .await?
        .require_compatible()?;

    ctx.get().top_level_target(TopLevelTargetSpec {
        label: providers_label.dupe(),
        target: node,
        outputs: outputs.dupe(),
    })?;

    let target_rule_type_name =
        get_target_rule_type_name(&mut ctx.get(), providers_label.target()).await?;

    let provider_collection = if providers_to_build.run {
        let providers = ctx
            .get()
            .get_providers(&providers_label)
            .await?
            .require_compatible()?;
        Some(providers)
    } else {
        None
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

    let mut outputs: Vec<_> = outputs
        .iter()
        .duped()
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
        .collect();

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

    event_consumer.consume_configured(ConfiguredBuildEvent {
        label: providers_label.dupe(),
        variant: ConfiguredBuildEventVariant::Prepared {
            provider_collection,
            target_rule_type_name,
        },
    });

    let mut outputs: FuturesUnordered<_> = outputs
        .into_iter()
        .map(|build| async move {
            let build = build.map(ConfiguredBuildEventVariant::Execution);
            match timeout_observer {
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
            }
        })
        .collect();

    while let Some(variant) = outputs.next().await {
        event_consumer.consume_configured(ConfiguredBuildEvent {
            label: providers_label.dupe(),
            variant,
        })
    }

    if !opts.graph_properties.is_empty() {
        let graph_properties = graph_properties::get_graph_properties(
            &mut ctx.get(),
            providers_label.target(),
            opts.graph_properties
                .should_compute_configured_graph_sketch(),
            opts.graph_properties
                .should_compute_per_configuration_sketch(),
            opts.graph_properties.retained_analysis_memory_sketch,
        )
        .await
        .map_err(|e| e.into());

        event_consumer.consume_configured(ConfiguredBuildEvent {
            label: providers_label,
            variant: ConfiguredBuildEventVariant::GraphProperties { graph_properties },
        });
    }

    Ok(())
}

#[derive(Clone, Allocative)]
pub struct ProviderArtifacts {
    pub values: ArtifactGroupValues,
    pub provider_type: BuildProviderType,
}

// what type of artifacts to build based on the provider it came from
#[derive(Default, Allocative, Debug, Clone, Dupe, Eq, PartialEq, Hash)]
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
