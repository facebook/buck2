/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    collections::{HashMap, HashSet},
    future::Future,
    hash::Hash,
    sync::Arc,
    time::Duration,
};

use buck2_data::{BuildGraphExecutionInfo, CriticalPathEntry, InstantEvent};
use derive_more::From;
use dice::UserComputationData;
use events::dispatch::EventDispatcher;
use gazebo::prelude::*;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tokio_stream::{wrappers::UnboundedReceiverStream, StreamExt};

use crate::{
    actions::{artifact::ArtifactKind, BuildArtifact, RegisteredAction},
    artifact_groups::{ArtifactGroup, TransitiveSetProjectionKey},
    events::proto::ToProtoMessage,
};

pub struct ActionExecutionSignal {
    pub action: Arc<RegisteredAction>,
    pub duration: Duration,
}

pub struct TransitiveSetComputationSignal {
    pub key: TransitiveSetProjectionKey,
    pub artifacts: HashSet<BuildArtifact>,
    pub set_deps: HashSet<TransitiveSetProjectionKey>,
}

/* These signals are distinct from the main Buck event bus because some
 * analysis needs access to the entire build graph, and serializing the
 * entire build graph isn't feasible - therefore, we have these signals
 * with an unserializable but lightweight handle on a RegisteredAction.
 */
#[derive(From)]
pub enum BuildSignal {
    ActionExecution(ActionExecutionSignal),
    TransitiveSetComputation(TransitiveSetComputationSignal),
    BuildFinished,
}

#[derive(Clone, Dupe)]
pub struct BuildSignalSender {
    sender: Arc<UnboundedSender<BuildSignal>>,
}

impl BuildSignalSender {
    pub fn signal(&self, signal: impl Into<BuildSignal>) {
        self.sender.send(signal.into()).ok();
    }
}

#[derive(Clone, Dupe)]
struct CriticalPathNode<TKey: Eq, TValue> {
    /// The aggregated duration of this critical path.
    pub duration: Duration,
    /// The value of this node. If None, this node just won't be included when displaying.
    pub value: Option<TValue>,
    pub prev: Option<TKey>,
}

#[derive(Hash, Eq, PartialEq, Clone, Dupe)]
enum NodeKey {
    BuildArtifact(BuildArtifact),
    TransitiveSetProjection(TransitiveSetProjectionKey),
}

pub struct BuildSignalReceiver {
    receiver: UnboundedReceiverStream<BuildSignal>,
    predecessors: HashMap<NodeKey, CriticalPathNode<NodeKey, Arc<RegisteredAction>>>,
}

fn extract_critical_path<TKey: Hash + Eq, TValue>(
    predecessors: &HashMap<TKey, CriticalPathNode<TKey, TValue>>,
) -> Vec<(&TKey, &Option<TValue>, Duration)> {
    let terminal = predecessors
        .iter()
        .max_by_key(|(_key, data)| data.duration)
        .map(|q| q.0);
    let mut path = itertools::unfold(terminal, |maybe_key| {
        if maybe_key.is_none() {
            return None;
        }
        let key = maybe_key.unwrap();
        let next = predecessors.get(key);
        *maybe_key = next.and_then(|q| (&q.prev).as_ref());
        next.map(|x| (key, &x.value, x.duration))
    })
    .collect::<Vec<_>>();
    // Take differences of adjacent elements to recover action time from cumulative sum.
    path.reverse();
    for i in (1..path.len()).rev() {
        path[i].2 = path[i].2.saturating_sub(path[i - 1].2);
    }

    path
}

impl BuildSignalReceiver {
    fn new(receiver: UnboundedReceiver<BuildSignal>) -> Self {
        Self {
            receiver: UnboundedReceiverStream::new(receiver),
            predecessors: HashMap::new(),
        }
    }

    pub async fn run_and_log(&mut self, events: EventDispatcher) -> anyhow::Result<()> {
        while let Some(event) = self.receiver.next().await {
            match event {
                BuildSignal::ActionExecution(execution) => self.process_action(execution)?,
                BuildSignal::TransitiveSetComputation(tset) => {
                    self.process_transitive_set_computation(tset)?
                }
                BuildSignal::BuildFinished => break,
            }
        }

        let build_info_payload: buck2_data::instant_event::Data = BuildGraphExecutionInfo {
            critical_path: self
                .extract_critical_path()
                .into_map(|(name, duration, action)| CriticalPathEntry {
                    action_name: name,
                    action_key: Some(action.key().as_proto()),
                    duration: Some(duration.into()),
                }),
        }
        .into();
        events.event(InstantEvent {
            data: Some(build_info_payload),
        });
        Ok(())
    }

    fn process_action(&mut self, execution: ActionExecutionSignal) -> Result<(), anyhow::Error> {
        // Identify most costly predecessor.
        let inputs = execution.action.inputs()?;

        let dep_keys = inputs.iter().filter_map(|dep| match dep {
            ArtifactGroup::Artifact(artifact) => match artifact.0.as_ref() {
                ArtifactKind::Build(build_artifact) => {
                    Some(NodeKey::BuildArtifact(build_artifact.dupe()))
                }
                _ => None,
            },
            ArtifactGroup::TransitiveSetProjection(key) => {
                Some(NodeKey::TransitiveSetProjection(key.dupe()))
            }
        });

        self.process_node(
            execution
                .action
                .outputs()?
                .iter()
                .map(|a| NodeKey::BuildArtifact(a.dupe())),
            Some(execution.action.dupe()),
            execution.duration,
            dep_keys,
        );

        Ok(())
    }

    fn process_transitive_set_computation(
        &mut self,
        set: TransitiveSetComputationSignal,
    ) -> anyhow::Result<()> {
        let artifacts = set.artifacts.into_iter().map(NodeKey::BuildArtifact);
        let sets = set
            .set_deps
            .into_iter()
            .map(NodeKey::TransitiveSetProjection);

        self.process_node(
            std::iter::once(NodeKey::TransitiveSetProjection(set.key)),
            None,
            Duration::from_secs(0), // Those nodes don't carry a duration.
            artifacts.chain(sets),
        );

        Ok(())
    }

    fn process_node(
        &mut self,
        keys: impl Iterator<Item = NodeKey>,
        value: Option<Arc<RegisteredAction>>,
        duration: Duration,
        dep_keys: impl Iterator<Item = NodeKey>,
    ) {
        let longest_ancestor = dep_keys
            .filter_map(|node_key| {
                let node_data = self.predecessors.get(&node_key)?;
                Some((node_key, node_data))
            })
            .max_by_key(|d| d.1.duration);

        let node = match longest_ancestor {
            Some((key, data)) => CriticalPathNode {
                prev: Some(key.dupe()),
                value,
                duration: data.duration + duration,
            },
            None => CriticalPathNode {
                prev: None,
                value,
                duration,
            },
        };

        for key in keys {
            self.predecessors.insert(key, node.dupe());
        }
    }

    pub fn extract_critical_path(&self) -> Vec<(String, Duration, &Arc<RegisteredAction>)> {
        extract_critical_path(&self.predecessors)
            .into_iter()
            .filter_map(|(_key, maybe_action, duration)| {
                let action = maybe_action.as_ref()?;
                if duration == Duration::ZERO {
                    return None;
                }
                let name = format!(
                    "{} {}{}",
                    action.owner(),
                    action.category(),
                    action
                        .identifier()
                        .map_or_else(|| "".to_owned(), |v| format!("[{}]", v))
                );
                Some((name, duration, action))
            })
            .collect()
    }
}

pub trait SetBuildSignals {
    fn set_build_signals(&mut self, sender: BuildSignalSender);
}

impl SetBuildSignals for UserComputationData {
    fn set_build_signals(&mut self, sender: BuildSignalSender) {
        self.data.set(sender);
    }
}

pub trait HasBuildSignals {
    fn get_build_signals(&self) -> Option<&BuildSignalSender>;
}

impl HasBuildSignals for UserComputationData {
    fn get_build_signals(&self) -> Option<&BuildSignalSender> {
        self.data.get::<BuildSignalSender>().ok()
    }
}

fn create_matched_pair() -> (BuildSignalSender, BuildSignalReceiver) {
    let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
    (
        BuildSignalSender {
            sender: Arc::new(sender),
        },
        BuildSignalReceiver::new(receiver),
    )
}

/// Creates a Build Listener signal pair and invokes the given asynchronous function with the send-end of the signal
/// sender.
///
/// Build listeners in this module operate by creating a matched pair of signal senders and signal receivers. Senders
/// are Dupe and allow for arbitrarily many writeres. Receivers are not Dupe and are expected to be driven by a single
/// thread. This implies that, in order for the receiver to function correctly and dispatch to build listeners, it must
/// be run in a backround task that is periodically polled.
///
/// This function arranges for a background task to be spawned that drives the receiver, while invoking the called
/// function with a live BuildSignalSender that can be used to send events to the listening receiver. Upon return of
/// `scope`, the sender terminates the receiver by sending a `BuildFinished` signal and joins the receiver task.
pub async fn scope<F, R, Fut>(events: EventDispatcher, func: F) -> anyhow::Result<R>
where
    F: FnOnce(BuildSignalSender) -> Fut,
    Fut: Future<Output = anyhow::Result<R>>,
{
    let (sender, mut receiver) = create_matched_pair();
    let receiver_task_handle = tokio::spawn(async move { receiver.run_and_log(events).await });
    let result = func(sender.dupe()).await;
    sender.signal(BuildSignal::BuildFinished);
    receiver_task_handle.await??;
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    type CriticalPathMap = HashMap<i32, CriticalPathNode<i32, i32>>;

    fn cp_insert(
        predecessors: &mut CriticalPathMap,
        key: i32,
        prev: Option<i32>,
        duration: Duration,
    ) {
        predecessors.insert(
            key,
            CriticalPathNode {
                duration,
                value: Some(key),
                prev,
            },
        );
    }
    #[test]
    fn empty_path() {
        let predecessors = CriticalPathMap::new();
        assert_eq!(extract_critical_path(&predecessors), vec![]);
    }

    #[test]
    fn unit_path() {
        let mut predecessors = CriticalPathMap::new();
        cp_insert(&mut predecessors, 1, None, Duration::from_secs(3));
        assert_eq!(
            extract_critical_path(&predecessors),
            vec![(&1, &Some(1), Duration::from_secs(3))],
        );
    }

    #[test]
    fn long_path() {
        let mut predecessors = HashMap::new();
        /*   -> 1 -> 2 -> 3
         *   5s   6s   7s
         *
         *      1 -> 4
         *        9s
         */
        cp_insert(&mut predecessors, 1, None, Duration::from_secs(5));
        cp_insert(&mut predecessors, 2, Some(1), Duration::from_secs(11));
        cp_insert(&mut predecessors, 3, Some(2), Duration::from_secs(18));
        cp_insert(&mut predecessors, 4, Some(1), Duration::from_secs(14));
        assert_eq!(
            extract_critical_path(&predecessors),
            vec![
                (&1, &Some(1), Duration::from_secs(5)),
                (&2, &Some(2), Duration::from_secs(6)),
                (&3, &Some(3), Duration::from_secs(7)),
            ],
        );
    }
}
