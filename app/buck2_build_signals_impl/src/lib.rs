/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![feature(error_generic_member_access)]

use std::any::Any;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use async_trait::async_trait;
use buck2_analysis::analysis::calculation::AnalysisKey;
use buck2_analysis::analysis::calculation::AnalysisKeyActivationData;
use buck2_analysis::analysis::calculation::AnalysisWithExtraData;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::RegisteredAction;
use buck2_build_api::actions::calculation::ActionExtraData;
use buck2_build_api::actions::calculation::ActionWithExtraData;
use buck2_build_api::actions::calculation::BuildKey;
use buck2_build_api::actions::calculation::BuildKeyActivationData;
use buck2_build_api::artifact_groups::ResolvedArtifactGroupBuildSignalsKey;
use buck2_build_api::artifact_groups::calculation::EnsureProjectedArtifactKey;
use buck2_build_api::artifact_groups::calculation::EnsureTransitiveSetProjectionKey;
use buck2_build_api::build_signals::BuildSignals;
use buck2_build_api::build_signals::BuildSignalsInstaller;
use buck2_build_api::build_signals::CREATE_BUILD_SIGNALS;
use buck2_build_signals::env::BuildSignalsContext;
use buck2_build_signals::env::CriticalPathBackendName;
use buck2_build_signals::env::DeferredBuildSignals;
use buck2_build_signals::env::EarlyCommandTiming;
use buck2_build_signals::env::FinishBuildSignals;
use buck2_build_signals::env::NodeDuration;
use buck2_build_signals::env::WaitingData;
use buck2_build_signals::error::CriticalPathError;
use buck2_build_signals::node_key::BuildSignalsNodeKey;
use buck2_common::package_listing::dice::PackageListingKey;
use buck2_common::package_listing::dice::PackageListingKeyActivationData;
use buck2_core::package::PackageLabel;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_data::ToProtoMessage;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::dispatch::instant_event;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_events::span::SpanId;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterResultsKey;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterResultsKeyActivationData;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_util::time_span::TimeSpan;
use dice::ActivationData;
use dice::ActivationTracker;
use dice::DynKey;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use gazebo::variants::VariantName;
use itertools::Itertools;
use smallvec::SmallVec;
use static_assertions::assert_eq_size;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;
use tokio::task::JoinHandle;
use tokio_stream::StreamExt;
use tokio_stream::wrappers::UnboundedReceiverStream;

use crate::backend::backend::BuildListenerBackend;
use crate::backend::logging::LoggingBackend;
use crate::backend::longest_path_graph::LongestPathGraphBackend;
use crate::enhancement::CriticalPathProtoEnhancer;
use crate::test_signals::TestExecutionBuildSignalKey;
use crate::test_signals::TestExecutionSignal;
use crate::test_signals::TestListingBuildSignalKey;
use crate::test_signals::TestListingSignal;

mod backend;
mod enhancement;
mod error;
mod test_signals;

/// A node in our critical path graph.
#[derive(Hash, Eq, PartialEq, Clone, Dupe, Debug, VariantName)]
enum NodeKey {
    // Those are DICE keys.
    BuildKey(BuildKey),
    AnalysisKey(AnalysisKey),
    EnsureProjectedArtifactKey(EnsureProjectedArtifactKey),
    EnsureTransitiveSetProjectionKey(EnsureTransitiveSetProjectionKey),
    InterpreterResultsKey(InterpreterResultsKey),
    PackageListingKey(PackageListingKey),

    // This one is not a DICE key.
    FinalMaterialization(BuildArtifact),

    // Keys for test not on DICE.
    TestExecution(TestExecutionBuildSignalKey),
    TestListing(TestListingBuildSignalKey),

    // Dynamically-typed.
    Dyn(&'static str, BuildSignalsNodeKey),
}

// Explain the sizeof this struct (and avoid regressing it since we store it in the longest path
// graph implementation).

assert_eq_size!(BuildKey, [usize; 4]);
assert_eq_size!(AnalysisKey, [usize; 2]);
assert_eq_size!(EnsureTransitiveSetProjectionKey, [usize; 5]);
assert_eq_size!(EnsureProjectedArtifactKey, [usize; 7]);
assert_eq_size!(InterpreterResultsKey, [usize; 1]);
assert_eq_size!(PackageListingKey, [usize; 1]);
assert_eq_size!(BuildArtifact, [usize; 6]);
assert_eq_size!(NodeKey, [usize; 7]);

impl NodeKey {
    fn from_dyn_key(key: &DynKey) -> Option<Self> {
        let key = if let Some(key) = key.downcast_ref::<BuildKey>() {
            Self::BuildKey(key.dupe())
        } else if let Some(key) = key.downcast_ref::<AnalysisKey>() {
            Self::AnalysisKey(key.dupe())
        } else if let Some(key) = key.downcast_ref::<EnsureProjectedArtifactKey>() {
            Self::EnsureProjectedArtifactKey(key.dupe())
        } else if let Some(key) = key.downcast_ref::<EnsureTransitiveSetProjectionKey>() {
            Self::EnsureTransitiveSetProjectionKey(key.dupe())
        } else if let Some(key) = key.downcast_ref::<InterpreterResultsKey>() {
            Self::InterpreterResultsKey(key.dupe())
        } else if let Some(key) = key.downcast_ref::<PackageListingKey>() {
            Self::PackageListingKey(key.dupe())
        } else if let Some(node_key) = key.request_value::<BuildSignalsNodeKey>() {
            Self::Dyn(key.key_type_name(), node_key)
        } else {
            return None;
        };

        Some(key)
    }

    fn into_critical_path_entry_data(
        self,
        extra_data: &NodeExtraData,
    ) -> buck2_data::critical_path_entry2::Entry {
        match self {
            NodeKey::BuildKey(ref key) => {
                let owner = key.0.owner().to_proto().into();

                // If we have a NodeKey that's an ActionKey we'd expect to have `action`
                // extra data (unless we didn't actually run it because of e.g. early
                // cutoff, in which case omitting it is what we want).
                match extra_data {
                    NodeExtraData::Action(ActionNodeData {
                        action,
                        execution_kind,
                        target_rule_type_name,
                        action_digest,
                        invalidation_info,
                    }) => buck2_data::critical_path_entry2::ActionExecution {
                        owner: Some(owner),
                        name: Some(buck2_data::ActionName {
                            category: action.category().as_str().to_owned(),
                            identifier: action.identifier().unwrap_or("").to_owned(),
                        }),
                        execution_kind: (*execution_kind).into(),
                        target_rule_type_name: target_rule_type_name.to_owned(),
                        action_digest: action_digest.to_owned(),
                        invalidation_info: invalidation_info.to_owned(),
                    }
                    .into(),
                    _ => self.into_generic_entry(),
                }
            }
            NodeKey::AnalysisKey(key) => buck2_data::critical_path_entry2::Analysis {
                target: Some(key.0.as_proto().into()),
                target_rule_type_name: match &extra_data {
                    NodeExtraData::Analysis(node_data) => node_data.target_rule_type_name.clone(),
                    _ => None,
                },
            }
            .into(),

            NodeKey::FinalMaterialization(key) => {
                let owner = key.key().owner().to_proto().into();

                buck2_data::critical_path_entry2::FinalMaterialization {
                    owner: Some(owner),
                    path: key.get_path().path().to_string(),
                }
                .into()
            }
            NodeKey::InterpreterResultsKey(key) => buck2_data::critical_path_entry2::Load {
                package: key.0.to_string(),
            }
            .into(),

            NodeKey::PackageListingKey(key) => buck2_data::critical_path_entry2::Listing {
                package: key.0.to_string(),
            }
            .into(),

            NodeKey::EnsureProjectedArtifactKey(..) => self.into_generic_entry(),
            NodeKey::EnsureTransitiveSetProjectionKey(..) => self.into_generic_entry(),
            NodeKey::Dyn(_, ref d) => match d.critical_path_entry_proto() {
                Some(entry) => entry,
                None => self.into_generic_entry(),
            },
            NodeKey::TestExecution(t) => buck2_data::critical_path_entry2::TestExecution {
                target_label: Some(t.target.as_proto()),
                suite: t.suite.to_string(),
                testcases: t.testcases.to_vec(),
                variant: t.variant.map(|v| v.to_string()),
            }
            .into(),

            NodeKey::TestListing(t) => buck2_data::critical_path_entry2::TestListing {
                target_label: Some(t.target.as_proto()),
                suite: t.suite.to_string(),
            }
            .into(),
        }
    }

    fn into_generic_entry(self) -> buck2_data::critical_path_entry2::Entry {
        buck2_data::critical_path_entry2::GenericEntry {
            kind: match self {
                NodeKey::Dyn(_, d) => d.kind(),
                _ => self.variant_name_lowercase(),
            }
            .to_owned(),
        }
        .into()
    }
}

impl fmt::Display for NodeKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BuildKey(k) => write!(f, "BuildKey({k})"),
            Self::AnalysisKey(k) => write!(f, "AnalysisKey({k})"),
            Self::EnsureProjectedArtifactKey(k) => write!(f, "EnsureProjectedArtifactKey({k})"),
            Self::EnsureTransitiveSetProjectionKey(k) => {
                write!(f, "EnsureTransitiveSetProjectionKey({k})")
            }
            Self::InterpreterResultsKey(k) => write!(f, "InterpreterResultsKey({k})"),
            Self::PackageListingKey(k) => write!(f, "PackageListingKey({k})"),
            Self::FinalMaterialization(k) => write!(f, "FinalMaterialization({k})"),
            Self::TestExecution(k) => write!(f, "TestExecution({k:?})"),
            Self::TestListing(k) => write!(f, "TestListing({k:?})"),
            Self::Dyn(name, k) => write!(f, "{name}({k})"),
        }
    }
}

struct TopLevelTargetSignal {
    pub(crate) label: ConfiguredTargetLabel,
    pub(crate) artifacts: Vec<ResolvedArtifactGroupBuildSignalsKey>,
}

struct FinalMaterializationSignal {
    pub(crate) artifact: BuildArtifact,
    pub(crate) duration: NodeDuration,
    pub(crate) span_id: Option<SpanId>,
    pub(crate) waiting_data: WaitingData,
}

/* These signals are distinct from the main Buck event bus because some
 * analysis needs access to the entire build graph, and serializing the
 * entire build graph isn't feasible - therefore, we have these signals
 * with an unserializable but lightweight handle on a RegisteredAction.
 */
enum BuildSignal {
    Evaluation(Evaluation),
    TopLevelTarget(TopLevelTargetSignal),
    FinalMaterialization(FinalMaterializationSignal),
    TestExecution(TestExecutionSignal),
    TestListing(TestListingSignal),
    BuildFinished,
}

/// Data for a BuildSignal that is the result of a DICE key evaluation.
pub(crate) struct Evaluation {
    /// The key we evaluated.
    key: NodeKey,
    /// The duration. By default this'll be zero, unless activation data says otherwise.
    duration: NodeDuration,
    /// The dependencies.
    dep_keys: Vec<NodeKey>,
    /// Spans that correspond to this key. We use this when producing a chrome trace.
    spans: SmallVec<[SpanId; 1]>,

    /// Data about time spent waiting (not on critical path) during this evaluation.
    waiting_data: WaitingData,

    /// Node-type-specific extra data (action data for BuildKey, load result for InterpreterResultsKey, etc.).
    extra_data: NodeExtraData,
}

#[derive(Clone)]
pub(crate) struct BuildSignalSender {
    sender: UnboundedSender<BuildSignal>,
}

impl BuildSignals for BuildSignalSender {
    fn top_level_target(
        &self,
        label: ConfiguredTargetLabel,
        artifacts: Vec<ResolvedArtifactGroupBuildSignalsKey>,
    ) {
        let _ignored = self
            .sender
            .send(BuildSignal::TopLevelTarget(TopLevelTargetSignal {
                label,
                artifacts,
            }));
    }

    fn final_materialization(
        &self,
        artifact: BuildArtifact,
        duration: NodeDuration,
        span_id: Option<SpanId>,
        waiting_data: WaitingData,
    ) {
        let _ignored = self.sender.send(BuildSignal::FinalMaterialization(
            FinalMaterializationSignal {
                artifact,
                duration,
                span_id,
                waiting_data,
            },
        ));
    }

    fn test_listing(
        &self,
        target: ConfiguredTargetLabel,
        suite: String,
        duration: NodeDuration,
        deps: &[ActionKey],
    ) {
        let _ignored = self
            .sender
            .send(BuildSignal::TestListing(TestListingSignal {
                target,
                suite,
                deps: deps.to_vec(),
                duration,
            }));
    }

    fn test_execution(
        &self,
        target: ConfiguredTargetLabel,
        suite: String,
        testcases: &[String],
        variant: Option<String>,
        duration: NodeDuration,
        deps: &[ActionKey],
    ) {
        let _ignored = self
            .sender
            .send(BuildSignal::TestExecution(TestExecutionSignal {
                target,
                suite,
                testcases: testcases.to_vec(),
                variant,
                deps: deps.to_vec(),
                duration,
            }));
    }
}

impl ActivationTracker for BuildSignalSender {
    /// We received a DICE key. Check if it's one of the keys we care about (i.e. can we downcast
    /// it to NodeKey?), and then if that's the case, extract its dependencies and activation data
    /// (if any).
    fn key_activated(
        &self,
        key: &DynKey,
        deps: &mut dyn Iterator<Item = &DynKey>,
        activation_data: ActivationData,
    ) {
        let key = match NodeKey::from_dyn_key(key) {
            Some(key) => key,
            None => return,
        };

        let mut signal = Evaluation {
            key,
            extra_data: NodeExtraData::None,
            duration: NodeDuration::zero(),
            dep_keys: deps.into_iter().filter_map(NodeKey::from_dyn_key).collect(),
            spans: Default::default(),
            waiting_data: WaitingData::new(),
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
                action_with_extra_data,
                duration,
                spans,
                waiting_data,
            }) = downcast_and_take(&mut activation_data)
            {
                signal.extra_data =
                    NodeExtraData::Action(ActionNodeData::from_extra_data(action_with_extra_data));
                signal.duration = duration;
                signal.spans = spans;
                signal.waiting_data = waiting_data;
            } else if let Some(AnalysisKeyActivationData {
                time_span,
                spans,
                analysis_with_extra_data,
                waiting_data,
            }) = downcast_and_take(&mut activation_data)
            {
                signal.duration = NodeDuration {
                    user: time_span.duration(),
                    total: time_span,
                    queue: None,
                };
                signal.spans = spans;
                signal.extra_data = NodeExtraData::Analysis(AnalysisNodeData::from_extra_data(
                    analysis_with_extra_data,
                ));
                signal.waiting_data = waiting_data;
            } else if let Some(InterpreterResultsKeyActivationData {
                time_span,
                result,
                spans,
            }) = downcast_and_take(&mut activation_data)
            {
                signal.duration = NodeDuration {
                    user: time_span.duration(),
                    total: time_span,
                    queue: None,
                };

                signal.extra_data = NodeExtraData::Load(result.ok());
                signal.spans = spans;
            } else if let Some(PackageListingKeyActivationData { time_span, spans }) =
                downcast_and_take(&mut activation_data)
            {
                signal.duration = NodeDuration {
                    user: time_span.duration(),
                    total: time_span,
                    queue: None,
                };
                signal.spans = spans;
            }
        }

        let _ignored = self.sender.send(BuildSignal::Evaluation(signal));
    }
}

pub(crate) struct DeferredBuildSignalsImpl {
    sender: Arc<BuildSignalSender>,
    receiver: UnboundedReceiver<BuildSignal>,
}

impl DeferredBuildSignals for DeferredBuildSignalsImpl {
    fn start(
        self: Box<Self>,
        events: EventDispatcher,
        backend: CriticalPathBackendName,
        ctx: BuildSignalsContext,
    ) -> Box<dyn FinishBuildSignals> {
        let handle = match backend {
            CriticalPathBackendName::LongestPathGraph => {
                start_backend(events, self.receiver, LongestPathGraphBackend::new(), ctx)
            }
            CriticalPathBackendName::Logging => start_backend(
                events.dupe(),
                self.receiver,
                LoggingBackend::new(events),
                ctx,
            ),
        };

        Box::new(FinishBuildSignalsImpl {
            sender: self.sender,
            handle,
        }) as _
    }
}

pub(crate) struct FinishBuildSignalsImpl {
    sender: Arc<BuildSignalSender>,
    handle: JoinHandle<Result<(), CriticalPathError>>,
}

#[async_trait]
impl FinishBuildSignals for FinishBuildSignalsImpl {
    async fn finish(self: Box<Self>) -> Result<(), CriticalPathError> {
        let _ignored = self.sender.sender.send(BuildSignal::BuildFinished);

        self.handle.await.expect("Error joining critical path task")
    }
}

fn start_backend(
    events: EventDispatcher,
    receiver: UnboundedReceiver<BuildSignal>,
    backend: impl BuildListenerBackend + Send + 'static,
    ctx: BuildSignalsContext,
) -> JoinHandle<Result<(), CriticalPathError>> {
    let listener = BuildSignalReceiver::new(receiver, backend);
    tokio::spawn(with_dispatcher_async(events.dupe(), async move {
        listener.run_and_log(ctx).await
    }))
}

struct BuildSignalReceiver<T> {
    receiver: UnboundedReceiverStream<BuildSignal>,
    // Maps a PackageLabel to the first PackageLabel that had an edge to it. When that PackageLabel
    // shows up, we'll give it a dependency on said first PackageLabel that had an edge to it, which
    // is how we discovered its existence.
    first_edge_to_load: HashMap<PackageLabel, PackageLabel>,
    backend: T,

    // TODO(rajneeshl): When Test listing and execution are on DICE, we can remove this and use
    // DICE keys instead.
    test_listing_keys: HashMap<String, NodeKey>,
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
            test_listing_keys: HashMap::new(),
        }
    }

    pub(crate) async fn run_and_log(
        mut self,
        ctx: BuildSignalsContext,
    ) -> Result<(), CriticalPathError> {
        while let Some(event) = self.receiver.next().await {
            match event {
                BuildSignal::Evaluation(eval) => self.process_evaluation(eval),
                BuildSignal::TopLevelTarget(top_level) => self.process_top_level_target(top_level),
                BuildSignal::FinalMaterialization(final_materialization) => {
                    self.process_final_materialization(final_materialization)
                }
                BuildSignal::TestExecution(test_execution) => {
                    self.process_test_execution(test_execution)
                }
                BuildSignal::TestListing(test_listing) => self.process_test_listing(test_listing),
                BuildSignal::BuildFinished => break,
            }
        }

        let now = Instant::now();

        let BuildInfo {
            critical_path,
            slowest_path,
            num_nodes,
            num_edges,
            top_level_targets,
        } = self.backend.finish()?;

        let critical_path2 = critical_path.into_critical_path_proto(&ctx.early_command_timing, now);

        let slowest_path = slowest_path.into_critical_path_proto(&ctx.early_command_timing, now);

        let top_level_targets =
            top_level_targets.map(|(key, duration)| buck2_data::TopLevelTargetCriticalPath {
                target: Some(key.as_proto()),
                duration: Some((*duration).try_into().unwrap_or(prost_types::Duration {
                    seconds: i64::MAX,
                    nanos: 0,
                })),
            });

        instant_event(buck2_data::BuildGraphExecutionInfo {
            critical_path2,
            slowest_path,
            metadata: ctx.metadata,
            command_name: Some(ctx.command_name),
            isolation_dir: Some(ctx.isolation_prefix.into_inner().into()),
            num_nodes,
            num_edges,
            backend_name: Some(T::name().to_string()),
            top_level_targets,
        });
        Ok(())
    }

    /// Receive an Evaluation. Do a little enrichment if it's a load, then pass through to the
    /// underlying backend.
    fn process_evaluation(&mut self, mut evaluation: Evaluation) {
        self.enrich_load(&mut evaluation);

        self.backend.process_node(
            evaluation.key,
            evaluation.extra_data,
            evaluation.duration,
            evaluation.dep_keys,
            evaluation.spans,
            evaluation.waiting_data,
        );
    }

    /// If the evaluation is a load (InterpreterResultsKey) and carries a load_result, then inject
    /// some extra edges that indicate which packages have now become visible as a result of this
    /// load.
    fn enrich_load(&mut self, evaluation: &mut Evaluation) {
        let pkg = match &evaluation.key {
            NodeKey::InterpreterResultsKey(InterpreterResultsKey(pkg)) => pkg,
            _ => return,
        };

        if let NodeExtraData::Load(Some(load_result)) = &evaluation.extra_data {
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
    fn process_top_level_target(&mut self, top_level: TopLevelTargetSignal) {
        self.backend.process_top_level_target(
            top_level.label,
            top_level.artifacts.map(|k| match k {
                ResolvedArtifactGroupBuildSignalsKey::BuildKey(b) => NodeKey::BuildKey(b.clone()),
                ResolvedArtifactGroupBuildSignalsKey::EnsureTransitiveSetProjectionKey(e) => {
                    NodeKey::EnsureTransitiveSetProjectionKey(e.clone())
                }
            }),
        );
    }

    fn process_final_materialization(&mut self, materialization: FinalMaterializationSignal) {
        let dep = NodeKey::BuildKey(BuildKey(materialization.artifact.key().dupe()));

        self.backend.process_node(
            NodeKey::FinalMaterialization(materialization.artifact),
            NodeExtraData::None,
            materialization.duration,
            std::iter::once(dep),
            materialization.span_id.into_iter().collect(),
            materialization.waiting_data,
        );
    }

    fn process_test_execution(&mut self, signal: TestExecutionSignal) {
        let key = TestExecutionBuildSignalKey {
            target: signal.target.dupe(),
            suite: Arc::new(signal.suite.to_owned()),
            testcases: Arc::new(signal.testcases),
            variant: signal.variant.map(Arc::new),
        };

        let deps = signal
            .deps
            .into_iter()
            .map(|d| NodeKey::BuildKey(BuildKey(d)));

        let listing_key = self
            .test_listing_keys
            .get(&signal.suite)
            .map(|k| k.dupe())
            .into_iter();

        self.backend.process_node(
            NodeKey::TestExecution(key),
            NodeExtraData::None,
            signal.duration,
            deps.chain(listing_key),
            Default::default(),
            WaitingData::new(),
        );
    }

    fn process_test_listing(&mut self, signal: TestListingSignal) {
        let key = TestListingBuildSignalKey {
            target: signal.target,
            suite: Arc::new(signal.suite.to_owned()),
        };

        let node_key = NodeKey::TestListing(key);

        // Since the TestListing and TestExecution keys are only created here, use this hashmap to
        // create dependencies between them.
        self.test_listing_keys.insert(signal.suite, node_key.dupe());

        let deps = signal
            .deps
            .into_iter()
            .map(|d| NodeKey::BuildKey(BuildKey(d)));

        self.backend.process_node(
            node_key,
            NodeExtraData::None,
            signal.duration,
            deps,
            Default::default(),
            WaitingData::new(),
        );
    }
}

pub(crate) struct BuildInfo {
    /// Node, its data, and its potential for improvement
    critical_path: DetailedCriticalPath,
    /// Path where each node's predecessor is the dependency that finished last.
    /// Unlike critical path, waiting time is directly attributable to the immediate predecessor.
    slowest_path: DetailedCriticalPath,
    num_nodes: u64,
    num_edges: u64,
    /// Critical path for top level targets
    top_level_targets: Vec<(ConfiguredTargetLabel, Duration)>,
}

/// Entry in a detailed critical path, including metadata about timing and dependencies.
pub(crate) struct DetailedCriticalPathEntry {
    pub(crate) key: NodeKey,
    pub(crate) data: NodeData,
    /// The potential improvement if this node's duration was reduced to zero.
    pub(crate) potential_improvement: Option<Duration>,
    /// The time when all dependencies finished executing (if known).
    pub(crate) deps_finished_time: Option<Instant>,
}

pub(crate) struct DetailedCriticalPath {
    entries: Vec<DetailedCriticalPathEntry>,
}

impl DetailedCriticalPath {
    fn empty() -> DetailedCriticalPath {
        Self {
            entries: Vec::new(),
        }
    }

    fn new(entries: Vec<DetailedCriticalPathEntry>) -> Self {
        Self { entries }
    }

    fn create_proto_entries_for_early_timings(
        enhancer: &mut CriticalPathProtoEnhancer,
        early_command_timing: &EarlyCommandTiming,
    ) {
        let generic_entry = |kind: &str| -> buck2_data::critical_path_entry2::Entry {
            buck2_data::critical_path_entry2::GenericEntry {
                kind: kind.to_owned(),
            }
            .into()
        };

        let mut current_kind = "buckd_command_init";
        let mut current_start = early_command_timing.command_start;
        for (span_start, kind) in &early_command_timing.early_spans {
            let span_start = span_start.max(&current_start);

            enhancer.add_simple_entry(
                None,
                generic_entry(current_kind),
                TimeSpan::new_saturating(current_start, *span_start),
                true,
            );
            current_kind = kind;
            current_start = *span_start;
        }
        enhancer.add_simple_entry(
            None,
            generic_entry(current_kind),
            TimeSpan::new_saturating(current_start, early_command_timing.early_command_end),
            true,
        );
    }

    fn into_critical_path_proto(
        self,
        early_command_timing: &EarlyCommandTiming,
        critical_path_compute_start: Instant,
    ) -> Vec<buck2_data::CriticalPathEntry2> {
        let mut enhancer = CriticalPathProtoEnhancer::new(
            early_command_timing.command_start,
            1 + early_command_timing.early_spans.len() + self.entries.len() + 1,
        );

        Self::create_proto_entries_for_early_timings(&mut enhancer, early_command_timing);

        for entry in self.entries {
            enhancer.add_entry(entry);
        }

        enhancer.add_simple_entry(
            Some("unknown_final_work"),
            buck2_data::critical_path_entry2::ComputeCriticalPath {}.into(),
            TimeSpan::new_saturating(critical_path_compute_start, Instant::now()),
            true,
        );
        enhancer.into_entries()
    }
}

/// Struct to hold data about a build graph node for critical path analysis.
#[derive(Clone)]
struct NodeData {
    /// Node-type-specific extra data (action data, load result, or none).
    extra_data: NodeExtraData,
    duration: NodeDuration,
    /// Data about time spent waiting (not on critical path) during this node's execution.
    waiting_data: WaitingData,
    span_ids: SmallVec<[SpanId; 1]>,
}

/// Type-safe enum for extra data associated with different node types in the build graph.
#[derive(Clone)]
enum NodeExtraData {
    /// The data that corresponds to a `NodeKey::BuildKey` Evaluation.
    Action(ActionNodeData),
    Analysis(AnalysisNodeData),
    /// The Load result that corresponds to a `NodeKey::InterpreterResultsKey` Evaluation if evaluation was successful.
    Load(Option<Arc<EvaluationResult>>),
    /// No extra data (used for other node types or when data is not available).
    None,
}

/// Extra data specific to action nodes.
///
/// Contains action execution metadata including the registered action, execution kind,
/// rule type, digest, and invalidation information.
#[derive(Clone)]
struct ActionNodeData {
    action: Arc<RegisteredAction>,
    execution_kind: buck2_data::ActionExecutionKind,
    target_rule_type_name: Option<String>,
    action_digest: Option<String>,
    invalidation_info: Option<buck2_data::CommandInvalidationInfo>,
}

impl ActionNodeData {
    fn from_extra_data(data: ActionWithExtraData) -> Self {
        let ActionWithExtraData {
            action,
            extra_data:
                ActionExtraData {
                    execution_kind,
                    target_rule_type_name,
                    action_digest,
                    invalidation_info,
                    ..
                },
        } = data;
        Self {
            action,
            execution_kind,
            target_rule_type_name,
            action_digest,
            invalidation_info,
        }
    }
}

#[derive(Clone)]
struct AnalysisNodeData {
    target_rule_type_name: Option<String>,
}

impl AnalysisNodeData {
    fn from_extra_data(data: AnalysisWithExtraData) -> Self {
        Self {
            target_rule_type_name: data.target_rule_type_name,
        }
    }
}

assert_eq_size!(NodeData, [usize; 20]);

fn create_build_signals() -> (BuildSignalsInstaller, Box<dyn DeferredBuildSignals>) {
    let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();

    let sender = Arc::new(BuildSignalSender { sender });
    let installer = BuildSignalsInstaller {
        build_signals: sender.dupe() as _,
        activation_tracker: sender.dupe() as _,
    };

    let deferred = Box::new(DeferredBuildSignalsImpl { sender, receiver });

    (installer, deferred as _)
}

pub fn init_late_bindings() {
    CREATE_BUILD_SIGNALS.init(create_build_signals)
}

pub(crate) fn duration_to_proto_saturating(duration: Duration) -> prost_types::Duration {
    duration.try_into().unwrap_or(prost_types::Duration {
        seconds: i64::MAX,
        nanos: i32::MAX,
    })
}
