/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_analysis::analysis::calculation::AnalysisKey;
use buck2_analysis::analysis::calculation::AnalysisKeyActivationData;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::calculation::BuildKey;
use buck2_build_api::actions::calculation::BuildKeyActivationData;
use buck2_build_api::actions::RegisteredAction;
use buck2_build_api::artifact_groups::calculation::EnsureProjectedArtifactKey;
use buck2_build_api::artifact_groups::calculation::EnsureTransitiveSetProjectionKey;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::ResolvedArtifactGroup;
use buck2_build_api::build_signals::BuildSignals;
use buck2_build_api::build_signals::BuildSignalsInstaller;
use buck2_build_api::build_signals::CREATE_BUILD_SIGNALS;
use buck2_build_api::deferred::calculation::DeferredCompute;
use buck2_build_api::deferred::calculation::DeferredResolve;
use buck2_build_signals::CriticalPathBackendName;
use buck2_build_signals::DeferredBuildSignals;
use buck2_build_signals::FinishBuildSignals;
use buck2_build_signals::NodeDuration;
use buck2_configured::nodes::calculation::ConfiguredTargetNodeKey;
use buck2_core::package::PackageLabel;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_data::ToProtoMessage;
use buck2_events::dispatch::instant_event;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::metadata;
use buck2_events::span::SpanId;
use buck2_interpreter_for_build::interpreter::calculation::IntepreterResultsKeyActivationData;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterResultsKey;
use buck2_node::nodes::eval_result::EvaluationResult;
use derive_more::From;
use dice::ActivationData;
use dice::ActivationTracker;
use dupe::Dupe;
use dupe::OptionDupedExt;
use itertools::Itertools;
use smallvec::SmallVec;
use static_assertions::assert_eq_size;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;
use tokio::task::JoinHandle;
use tokio_stream::wrappers::UnboundedReceiverStream;
use tokio_stream::StreamExt;

use crate::backend::backend::BuildListenerBackend;
use crate::backend::default::DefaultBackend;
use crate::backend::longest_path_graph::LongestPathGraphBackend;

mod backend;

/// A node in our critical path graph.
#[derive(Hash, Eq, PartialEq, Clone, Dupe, Debug, From)]
enum NodeKey {
    // Those are DICE keys.
    BuildKey(BuildKey),
    AnalysisKey(AnalysisKey),
    EnsureProjectedArtifactKey(EnsureProjectedArtifactKey),
    EnsureTransitiveSetProjectionKey(EnsureTransitiveSetProjectionKey),
    DeferredCompute(DeferredCompute),
    DeferredResolve(DeferredResolve),
    ConfiguredTargetNodeKey(ConfiguredTargetNodeKey),
    InterpreterResultsKey(InterpreterResultsKey),

    // This one is not a DICE key.
    Materialization(BuildArtifact),
}

// Explain the sizeof this struct (and avoid regressing it since we store it in the longest path
// graph implementation).

assert_eq_size!(BuildKey, [usize; 4]);
assert_eq_size!(AnalysisKey, [usize; 2]);
assert_eq_size!(EnsureTransitiveSetProjectionKey, [usize; 5]);
assert_eq_size!(EnsureProjectedArtifactKey, [usize; 7]);
assert_eq_size!(DeferredCompute, [usize; 4]);
assert_eq_size!(DeferredResolve, [usize; 4]);
assert_eq_size!(ConfiguredTargetNodeKey, [usize; 2]);
assert_eq_size!(InterpreterResultsKey, [usize; 1]);
assert_eq_size!(BuildArtifact, [usize; 6]);
assert_eq_size!(NodeKey, [usize; 7]);

impl NodeKey {
    fn from_any(key: &dyn Any) -> Option<Self> {
        let key = if let Some(key) = key.downcast_ref::<BuildKey>() {
            Self::BuildKey(key.dupe())
        } else if let Some(key) = key.downcast_ref::<AnalysisKey>() {
            Self::AnalysisKey(key.dupe())
        } else if let Some(key) = key.downcast_ref::<EnsureProjectedArtifactKey>() {
            Self::EnsureProjectedArtifactKey(key.dupe())
        } else if let Some(key) = key.downcast_ref::<EnsureTransitiveSetProjectionKey>() {
            Self::EnsureTransitiveSetProjectionKey(key.dupe())
        } else if let Some(key) = key.downcast_ref::<DeferredCompute>() {
            Self::DeferredCompute(key.dupe())
        } else if let Some(key) = key.downcast_ref::<DeferredResolve>() {
            Self::DeferredResolve(key.dupe())
        } else if let Some(key) = key.downcast_ref::<ConfiguredTargetNodeKey>() {
            Self::ConfiguredTargetNodeKey(key.dupe())
        } else if let Some(key) = key.downcast_ref::<InterpreterResultsKey>() {
            Self::InterpreterResultsKey(key.dupe())
        } else {
            return None;
        };

        Some(key)
    }
}

impl fmt::Display for NodeKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BuildKey(k) => write!(f, "BuildKey({})", k),
            Self::AnalysisKey(k) => write!(f, "AnalysisKey({})", k),
            Self::EnsureProjectedArtifactKey(k) => write!(f, "EnsureProjectedArtifactKey({})", k),
            Self::EnsureTransitiveSetProjectionKey(k) => {
                write!(f, "EnsureTransitiveSetProjectionKey({})", k)
            }
            Self::DeferredCompute(k) => write!(f, "DeferredCompute({})", k),
            Self::DeferredResolve(k) => write!(f, "DeferredResolve({})", k),
            Self::ConfiguredTargetNodeKey(k) => write!(f, "ConfiguredTargetNodeKey({})", k),
            Self::InterpreterResultsKey(k) => write!(f, "InterpreterResultsKey({})", k),
            Self::Materialization(k) => write!(f, "Materialization({})", k),
        }
    }
}

struct TopLevelTargetSignal {
    pub label: ConfiguredTargetLabel,
    pub artifacts: Vec<ArtifactGroup>,
}

struct FinalMaterializationSignal {
    pub artifact: BuildArtifact,
    pub duration: NodeDuration,
    pub span_id: Option<SpanId>,
}

/* These signals are distinct from the main Buck event bus because some
 * analysis needs access to the entire build graph, and serializing the
 * entire build graph isn't feasible - therefore, we have these signals
 * with an unserializable but lightweight handle on a RegisteredAction.
 */
#[derive(From)]
enum BuildSignal {
    Evaluation(Evaluation),
    TopLevelTarget(TopLevelTargetSignal),
    FinalMaterialization(FinalMaterializationSignal),
    BuildFinished,
}

/// Data for a BuildSignal that is the result of a DICE key evaluation.
pub struct Evaluation {
    /// The key we evaluated.
    key: NodeKey,
    /// The duration. By default this'll be zero, unless activation data says otherwise.
    duration: NodeDuration,
    /// The dependencies.
    dep_keys: Vec<NodeKey>,
    /// Spans that correspond to this key. We use this when producing a chrome trace.
    spans: SmallVec<[SpanId; 1]>,

    // NOTE: The fields below aren't usually going to be both set, but it doesn't really hurt (for
    // now) to have them not tied to the right variant.
    /// The RegisteredAction that corresponds to this Evaluation (this will only be present for
    /// NodeKey::BuildKey).
    action: Option<Arc<RegisteredAction>>,

    /// The Load result that corresponds to this Evaluation (this will only be pesent for
    /// InterpreterResultsKey).
    load_result: Option<Arc<EvaluationResult>>,
}

pub struct BuildSignalSender {
    sender: UnboundedSender<BuildSignal>,
}

impl BuildSignals for BuildSignalSender {
    fn top_level_target(&self, label: ConfiguredTargetLabel, artifacts: Vec<ArtifactGroup>) {
        let _ignored = self
            .sender
            .send(TopLevelTargetSignal { label, artifacts }.into());
    }

    fn final_materialization(
        &self,
        artifact: BuildArtifact,
        duration: NodeDuration,
        span_id: Option<SpanId>,
    ) {
        let _ignored = self.sender.send(
            FinalMaterializationSignal {
                artifact,
                duration,
                span_id,
            }
            .into(),
        );
    }
}

impl ActivationTracker for BuildSignalSender {
    /// We received a DICE key. Check if it's one of the keys we care about (i.e. can we downcast
    /// it to NodeKey?), and then if that's the case, extract its dependencies and activation data
    /// (if any).
    fn key_activated(
        &self,
        key: &dyn Any,
        deps: &mut dyn Iterator<Item = &dyn Any>,
        activation_data: ActivationData,
    ) {
        let key = match NodeKey::from_any(key) {
            Some(key) => key,
            None => return,
        };

        let mut signal = Evaluation {
            key,
            action: None,
            duration: NodeDuration::zero(),
            dep_keys: deps.into_iter().filter_map(NodeKey::from_any).collect(),
            spans: Default::default(),
            load_result: None,
        };

        /// Given an Option containing an Any, take it if and only if it contains a T.
        fn downcast_and_take<T: 'static>(
            data: &mut Option<Box<dyn Any + Send + Sync + 'static>>,
        ) -> Option<T> {
            if data.as_ref().map(|d| d.is::<T>()) != Some(true) {
                return None;
            }

            // Unwrap safety: we just checked that the option is occupied and the type matches
            Some(*data.take().unwrap().downcast().ok().unwrap())
        }

        if let ActivationData::Evaluated(mut activation_data) = activation_data {
            if let Some(BuildKeyActivationData {
                action,
                duration,
                spans,
            }) = downcast_and_take(&mut activation_data)
            {
                signal.action = Some(action);
                signal.duration = duration;
                signal.spans = spans;
            } else if let Some(AnalysisKeyActivationData { duration, spans }) =
                downcast_and_take(&mut activation_data)
            {
                signal.duration = NodeDuration {
                    user: duration,
                    total: duration,
                };
                signal.spans = spans;
            } else if let Some(IntepreterResultsKeyActivationData {
                duration,
                result,
                spans,
            }) = downcast_and_take(&mut activation_data)
            {
                signal.duration = NodeDuration {
                    user: duration,
                    total: duration,
                };

                signal.load_result = result.ok();
                signal.spans = spans;
            }
        }

        let _ignored = self.sender.send(signal.into());
    }
}

pub struct DeferredBuildSignalsImpl {
    sender: Arc<BuildSignalSender>,
    receiver: UnboundedReceiver<BuildSignal>,
}

impl DeferredBuildSignals for DeferredBuildSignalsImpl {
    fn start(
        self: Box<Self>,
        events: EventDispatcher,
        backend: CriticalPathBackendName,
    ) -> Box<dyn FinishBuildSignals> {
        let handle = match backend {
            CriticalPathBackendName::LongestPathGraph => {
                start_backend(events, self.receiver, LongestPathGraphBackend::new())
            }
            CriticalPathBackendName::Default => {
                start_backend(events, self.receiver, DefaultBackend::new())
            }
        };

        Box::new(FinishBuildSignalsImpl {
            sender: self.sender,
            handle,
        }) as _
    }
}

pub struct FinishBuildSignalsImpl {
    sender: Arc<BuildSignalSender>,
    handle: JoinHandle<anyhow::Result<()>>,
}

#[async_trait]
impl FinishBuildSignals for FinishBuildSignalsImpl {
    async fn finish(self: Box<Self>) -> anyhow::Result<()> {
        let _ignored = self.sender.sender.send(BuildSignal::BuildFinished);

        self.handle
            .await
            .context("Error joining critical path task")?
    }
}

fn start_backend(
    events: EventDispatcher,
    receiver: UnboundedReceiver<BuildSignal>,
    backend: impl BuildListenerBackend + Send + 'static,
) -> JoinHandle<anyhow::Result<()>> {
    let listener = BuildSignalReceiver::new(receiver, backend);
    tokio::spawn(with_dispatcher_async(events.dupe(), async move {
        listener.run_and_log().await
    }))
}

struct BuildSignalReceiver<T> {
    receiver: UnboundedReceiverStream<BuildSignal>,
    // Maps a PackageLabel to the first PackageLabel that had an edge to it. When that PackageLabel
    // shows up, we'll give it a dependency on said first PackageLabel that had an edge to it, which
    // is how we discovered its existence.
    first_edge_to_load: HashMap<PackageLabel, PackageLabel>,
    backend: T,
}

impl<T> BuildSignalReceiver<T>
where
    T: BuildListenerBackend,
{
    fn new(receiver: UnboundedReceiver<BuildSignal>, backend: T) -> Self {
        Self {
            receiver: UnboundedReceiverStream::new(receiver),
            backend,
            first_edge_to_load: HashMap::new(),
        }
    }

    pub async fn run_and_log(mut self) -> anyhow::Result<()> {
        while let Some(event) = self.receiver.next().await {
            match event {
                BuildSignal::Evaluation(eval) => self.process_evaluation(eval),
                BuildSignal::TopLevelTarget(top_level) => {
                    self.process_top_level_target(top_level)?
                }
                BuildSignal::FinalMaterialization(final_materialization) => {
                    self.process_final_materialization(final_materialization)?
                }
                BuildSignal::BuildFinished => break,
            }
        }

        let now = Instant::now();

        let BuildInfo {
            critical_path,
            num_nodes,
            num_edges,
        } = self.backend.finish()?;

        let compute_elapsed = now.elapsed();

        let meta_entry_data = NodeData {
            action: None,
            duration: NodeDuration {
                user: Duration::ZERO,
                total: compute_elapsed,
            },
            span_ids: Default::default(),
        };

        let meta_entry = (
            buck2_data::critical_path_entry2::ComputeCriticalPath {}.into(),
            &meta_entry_data,
            &Some(compute_elapsed),
        );

        let critical_path2 = critical_path
            .iter()
            .filter_map(|(key, data, potential_improvement)| {
                let entry: buck2_data::critical_path_entry2::Entry = match key {
                    NodeKey::BuildKey(key) => {
                        let owner = key.0.owner().to_proto().into();

                        // If we have a NodeKey that's an ActionKey we'd expect to have an `action`
                        // in our data (unless we didn't actually run it because of e.g. early
                        // cutoff, in which case omitting it is what we want).
                        let action = data.action.as_ref()?;

                        buck2_data::critical_path_entry2::ActionExecution {
                            owner: Some(owner),
                            name: Some(buck2_data::ActionName {
                                category: action.category().as_str().to_owned(),
                                identifier: action.identifier().unwrap_or("").to_owned(),
                            }),
                        }
                        .into()
                    }
                    NodeKey::AnalysisKey(key) => buck2_data::critical_path_entry2::Analysis {
                        target: Some(key.0.as_proto().into()),
                    }
                    .into(),
                    NodeKey::Materialization(key) => {
                        let owner = key.key().owner().to_proto().into();

                        buck2_data::critical_path_entry2::Materialization {
                            owner: Some(owner),
                            path: key.get_path().path().to_string(),
                        }
                        .into()
                    }
                    NodeKey::InterpreterResultsKey(key) => buck2_data::critical_path_entry2::Load {
                        package: key.0.to_string(),
                    }
                    .into(),
                    NodeKey::EnsureProjectedArtifactKey(..) => return None,
                    NodeKey::EnsureTransitiveSetProjectionKey(..) => return None,
                    NodeKey::DeferredCompute(..) => return None,
                    NodeKey::DeferredResolve(..) => return None,
                    NodeKey::ConfiguredTargetNodeKey(..) => return None,
                };

                Some((entry, data, potential_improvement))
            })
            .chain(std::iter::once(meta_entry))
            .map(|(entry, data, potential_improvement)| {
                anyhow::Ok(buck2_data::CriticalPathEntry2 {
                    span_ids: data
                        .span_ids
                        .iter()
                        .map(|span_id| (*span_id).into())
                        .collect(),
                    duration: Some(data.duration.critical_path_duration().try_into()?),
                    user_duration: Some(data.duration.user.try_into()?),
                    total_duration: Some(data.duration.total.try_into()?),
                    potential_improvement_duration: potential_improvement
                        .map(|p| p.try_into())
                        .transpose()?,
                    entry: Some(entry),
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        instant_event(buck2_data::BuildGraphExecutionInfo {
            critical_path: Vec::new(),
            critical_path2,
            metadata: metadata::collect(),
            num_nodes,
            num_edges,
            uses_total_duration: true,
            backend_name: Some(T::name().to_string()),
        });
        Ok(())
    }

    /// Receive an Evaluation. Do a little enrichment if it's a load, then pass through to the
    /// underying backend.
    fn process_evaluation(&mut self, mut evaluation: Evaluation) {
        self.enrich_load(&mut evaluation);

        self.backend.process_node(
            evaluation.key,
            evaluation.action,
            evaluation.duration,
            evaluation.dep_keys.into_iter(),
            evaluation.spans,
        );
    }

    /// If the evaluation is a load (InterpreterResultsKey) and carries a load_result, then inject
    /// some extra edges that indicate which packages have now become visibile as a result of this
    /// load.
    fn enrich_load(&mut self, evaluation: &mut Evaluation) {
        let pkg = match &evaluation.key {
            NodeKey::InterpreterResultsKey(InterpreterResultsKey(pkg)) => pkg,
            _ => return,
        };

        if let Some(load_result) = &evaluation.load_result {
            let deps_pkg = load_result
                .targets()
                .values()
                .flat_map(|target| target.deps().map(|t| t.pkg()))
                .unique()
                .map(|pkg| pkg.dupe());

            for dep_pkg in deps_pkg {
                if dep_pkg == *pkg {
                    continue;
                }

                self.first_edge_to_load
                    .entry(dep_pkg)
                    .or_insert_with(|| pkg.dupe());
            }
        }

        let first_edge = self.first_edge_to_load.get(pkg);

        if let Some(first_edge) = first_edge {
            evaluation
                .dep_keys
                .push(NodeKey::InterpreterResultsKey(InterpreterResultsKey(
                    first_edge.dupe(),
                )));
        }
    }

    // TODO: We would need something similar with anon targets.
    fn process_top_level_target(
        &mut self,
        top_level: TopLevelTargetSignal,
    ) -> Result<(), anyhow::Error> {
        let artifact_keys =
            top_level
                .artifacts
                .into_iter()
                .filter_map(|dep| match dep.assert_resolved() {
                    ResolvedArtifactGroup::Artifact(artifact) => artifact
                        .action_key()
                        .duped()
                        .map(BuildKey)
                        .map(NodeKey::BuildKey),
                    ResolvedArtifactGroup::TransitiveSetProjection(key) => {
                        Some(NodeKey::EnsureTransitiveSetProjectionKey(
                            EnsureTransitiveSetProjectionKey(key.dupe()),
                        ))
                    }
                });

        self.backend.process_top_level_target(
            NodeKey::AnalysisKey(AnalysisKey(top_level.label)),
            artifact_keys,
        );

        Ok(())
    }

    fn process_final_materialization(
        &mut self,
        materialization: FinalMaterializationSignal,
    ) -> Result<(), anyhow::Error> {
        let dep = NodeKey::BuildKey(BuildKey(materialization.artifact.key().dupe()));

        self.backend.process_node(
            NodeKey::Materialization(materialization.artifact),
            None,
            materialization.duration,
            std::iter::once(dep),
            materialization.span_id.into_iter().collect(),
        );

        Ok(())
    }
}

pub struct BuildInfo {
    // Node, its data, and its potential for improvement
    critical_path: Vec<(NodeKey, NodeData, Option<Duration>)>,
    num_nodes: u64,
    num_edges: u64,
}

#[derive(Clone)]
struct NodeData {
    action: Option<Arc<RegisteredAction>>,
    duration: NodeDuration,
    span_ids: SmallVec<[SpanId; 1]>,
}

assert_eq_size!(NodeData, [usize; 8]);

fn create_build_signals() -> (BuildSignalsInstaller, Box<dyn DeferredBuildSignals>) {
    let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();

    let sender = Arc::new(BuildSignalSender { sender });
    let installer = BuildSignalsInstaller {
        build_signals: sender.dupe() as _,
        activation_tracker: sender.dupe() as _,
    };

    let deferred = Box::new(DeferredBuildSignalsImpl { receiver, sender });

    (installer, deferred as _)
}

pub fn init_late_bindings() {
    CREATE_BUILD_SIGNALS.init(create_build_signals)
}
