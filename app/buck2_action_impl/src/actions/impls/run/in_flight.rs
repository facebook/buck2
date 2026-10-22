/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Merges identical `run` actions that are in flight at the same time
//! (`buck2.dedupe_in_flight_actions`).
//!
//! With content-based paths, the same command in two configurations has one action digest. The
//! first action to arrive with a given key is the leader: it looks up the caches and executes as
//! usual. An action that arrives while the leader runs is a follower: it waits, and takes the
//! leader's result instead of looking up the action cache or executing.
//!
//! - A leader that succeeds publishes its output values, whether it executed or hit a cache
//!   (the action cache, or a dep file cache after the action cache missed). Content-based
//!   outputs of leader and follower resolve to the same paths, so the follower has nothing to
//!   materialize that the leader has not already declared.
//! - A leader whose command exited with a code from 1 to 127 publishes that exit code and its
//!   output streams, and each follower fails with them. A flaky failure is shared rather than
//!   retried, as it is when the same command fails in sibling configurations without this key.
//! - A leader that is cancelled, times out, or fails for any other reason publishes nothing: an
//!   executor error, a worker failure, a missing input, or an exit code of 128 or more, which is
//!   how a command killed by a signal (an OOM kill included) is reported. Those say more about
//!   the machine than about the command. Its followers then race to become the
//!   next leader, and each runs as if it had arrived first.
//!
//! The registry lives for the daemon, across concurrent commands, and holds an entry only while
//! its leader runs.

use std::sync::Arc;
use std::sync::LazyLock;

use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_core::content_hash::ContentBasedPathHash;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::executor_stage_async;
use buck2_execute::execute::output::CommandStdStreams;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::execute::result::CommandExecutionStatus;
use buck2_hash::BuckMutInsertionMap;
use buck2_hash::BuckMutMap;
use dupe::Dupe;
use parking_lot::Mutex;
use tokio::sync::watch;

/// What makes two executions interchangeable. The action digest covers the command, inputs,
/// environment, output paths and RE platform; the rest is what changes a result, or how it is
/// produced, outside the digest.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct InFlightKey {
    pub(crate) digest: ActionDigest,
    /// Hash of the executor configuration: local, remote or hybrid, and its options.
    pub(crate) executor: u64,
    /// The action's `ExecutorPreference`, as displayed.
    pub(crate) executor_preference: String,
    /// Whether this action uploads its result to the action cache.
    pub(crate) allow_cache_upload: bool,
}

/// How a leader's command ended, when its followers can share it.
pub(crate) enum LeaderOutcome {
    Succeeded {
        /// Output values by the placeholder path the command wrote each one to. That path is part
        /// of the action digest, so a follower's outputs resolve to the same paths.
        outputs: BuckMutMap<ProjectRelativePathBuf, ArtifactValue>,
    },
    /// The command exited with a code from 1 to 127.
    Failed {
        exit_code: Option<i32>,
        std_streams: CommandStdStreams,
    },
}

/// A leader's result that its followers take as their own.
pub(crate) struct LeaderResult {
    pub(crate) outcome: LeaderOutcome,
    pub(crate) leader: buck2_data::ActionKey,
    pub(crate) leader_kind: buck2_data::ActionExecutionKind,
}

impl LeaderResult {
    /// The result to publish, or `None` when the leader's followers should run themselves.
    pub(crate) fn new(
        result: &CommandExecutionResult,
        fs: &ArtifactFs,
        leader: buck2_data::ActionKey,
    ) -> buck2_error::Result<Option<Self>> {
        let outcome = match &result.report.status {
            CommandExecutionStatus::Success { .. } => {
                let mut outputs = BuckMutMap::default();
                for output in result.resolve_outputs(fs) {
                    let (resolved, _content_path, value) = output?;
                    outputs.insert(resolved.into_path(), value.dupe());
                }
                LeaderOutcome::Succeeded { outputs }
            }
            CommandExecutionStatus::Failure {
                missing_materialized_inputs: false,
                ..
            } if matches!(result.report.exit_code, Some(1..=127)) => LeaderOutcome::Failed {
                exit_code: result.report.exit_code,
                std_streams: result.report.std_streams.clone(),
            },
            _ => return Ok(None),
        };
        let leader_kind = result
            .report
            .status
            .execution_kind()
            .map_or(buck2_data::ActionExecutionKind::NotSet, |k| k.as_enum());
        Ok(Some(Self {
            outcome,
            leader,
            leader_kind,
        }))
    }

    /// The result of a leader served by the local dep file cache.
    pub(crate) fn from_dep_file_hit(
        outputs: &ActionOutputs,
        fs: &ArtifactFs,
        leader: buck2_data::ActionKey,
    ) -> buck2_error::Result<Self> {
        let mut by_path = BuckMutMap::default();
        for (path, value) in outputs.iter() {
            by_path.insert(
                fs.resolve_build(path, Some(&ContentBasedPathHash::OutputArtifact))?,
                value.dupe(),
            );
        }
        Ok(Self {
            outcome: LeaderOutcome::Succeeded { outputs: by_path },
            leader,
            leader_kind: buck2_data::ActionExecutionKind::LocalDepFile,
        })
    }

    /// A succeeded leader's values keyed by `request`'s outputs, or `None` if one of them has no
    /// value.
    pub(crate) fn outputs_for(
        outputs: &BuckMutMap<ProjectRelativePathBuf, ArtifactValue>,
        request: &CommandExecutionRequest,
        fs: &ArtifactFs,
    ) -> buck2_error::Result<Option<BuckMutInsertionMap<CommandExecutionOutput, ArtifactValue>>>
    {
        let mut by_output = BuckMutInsertionMap::default();
        for output in request.outputs() {
            let path = output
                .resolve(fs, Some(&ContentBasedPathHash::OutputArtifact))?
                .into_path();
            let Some(value) = outputs.get(&path) else {
                return Ok(None);
            };
            by_output.insert(output.cloned(), value.dupe());
        }
        Ok(Some(by_output))
    }
}

type Slot = Option<Arc<LeaderResult>>;

/// A running leader: who it is, and where its result will appear.
struct Entry {
    leader: buck2_data::ActionKey,
    rx: watch::Receiver<Slot>,
}

static IN_FLIGHT: LazyLock<Mutex<BuckMutMap<InFlightKey, Entry>>> =
    LazyLock::new(|| Mutex::new(BuckMutMap::default()));

pub(crate) enum Gate {
    /// No identical action is running: run this one, and publish its result if it succeeds.
    Leader(LeaderGuard),
    /// An identical action ran and succeeded while this one waited.
    Follower(Arc<LeaderResult>),
}

/// Waits until this action, `action`, may run as a leader or can take a leader's result.
pub(crate) async fn enter(key: InFlightKey, action: buck2_data::ActionKey) -> Gate {
    loop {
        let (leader, mut rx) = {
            let mut in_flight = IN_FLIGHT.lock();
            match in_flight.get(&key) {
                Some(entry) => (entry.leader.clone(), entry.rx.clone()),
                None => {
                    let (tx, rx) = watch::channel(None);
                    in_flight.insert(key.clone(), Entry { leader: action, rx });
                    return Gate::Leader(LeaderGuard { key, tx });
                }
            }
        };
        // The channel closes without a value when the leader fails or is cancelled. Go round
        // again: one of the waiters becomes the next leader.
        let stage = buck2_data::InFlightWait {
            leader_action_key: Some(leader),
        };
        let published = executor_stage_async(stage, async {
            rx.wait_for(Option::is_some)
                .await
                .ok()
                .and_then(|slot| slot.as_ref().map(Dupe::dupe))
        })
        .await;
        if let Some(result) = published {
            return Gate::Follower(result);
        }
    }
}

/// Held by a leader while it runs. Dropping it removes the leader's entry and closes the channel,
/// whether or not a result was published.
pub(crate) struct LeaderGuard {
    key: InFlightKey,
    tx: watch::Sender<Slot>,
}

impl LeaderGuard {
    pub(crate) fn publish(self, result: LeaderResult) {
        // No receivers just means nobody waited.
        let _ignored = self.tx.send(Some(Arc::new(result)));
    }
}

impl Drop for LeaderGuard {
    fn drop(&mut self) {
        let ours = self.tx.subscribe();
        let mut in_flight = IN_FLIGHT.lock();
        if in_flight
            .get(&self.key)
            .is_some_and(|entry| entry.rx.same_channel(&ours))
        {
            in_flight.remove(&self.key);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use super::*;

    fn key(n: u8) -> InFlightKey {
        InFlightKey {
            digest: ActionDigest::new_sha1([n; 20], 1),
            executor: 0,
            executor_preference: "default".to_owned(),
            allow_cache_upload: false,
        }
    }

    fn result(kind: buck2_data::ActionExecutionKind) -> LeaderResult {
        LeaderResult {
            outcome: LeaderOutcome::Succeeded {
                outputs: BuckMutMap::default(),
            },
            leader: buck2_data::ActionKey::default(),
            leader_kind: kind,
        }
    }

    fn leader(gate: Gate) -> LeaderGuard {
        match gate {
            Gate::Leader(guard) => guard,
            Gate::Follower(_) => panic!("expected to lead"),
        }
    }

    #[tokio::test]
    async fn follower_takes_published_result() {
        let guard = leader(enter(key(1), buck2_data::ActionKey::default()).await);
        let follower = tokio::spawn(enter(key(1), buck2_data::ActionKey::default()));
        tokio::time::sleep(Duration::from_millis(50)).await;
        guard.publish(result(buck2_data::ActionExecutionKind::Remote));
        match follower.await.unwrap() {
            Gate::Follower(r) => assert_eq!(r.leader_kind, buck2_data::ActionExecutionKind::Remote),
            Gate::Leader(_) => panic!("expected to follow"),
        }
    }

    #[tokio::test]
    async fn follower_leads_when_leader_gives_up() {
        let guard = leader(enter(key(2), buck2_data::ActionKey::default()).await);
        let follower = tokio::spawn(enter(key(2), buck2_data::ActionKey::default()));
        tokio::time::sleep(Duration::from_millis(50)).await;
        drop(guard);
        let next = leader(follower.await.unwrap());
        // The new leader's entry is the one in the registry now.
        let late = tokio::spawn(enter(key(2), buck2_data::ActionKey::default()));
        tokio::time::sleep(Duration::from_millis(50)).await;
        next.publish(result(buck2_data::ActionExecutionKind::Local));
        assert!(matches!(late.await.unwrap(), Gate::Follower(_)));
    }

    #[tokio::test]
    async fn entry_ends_with_its_leader() {
        leader(enter(key(3), buck2_data::ActionKey::default()).await)
            .publish(result(buck2_data::ActionExecutionKind::Local));
        leader(enter(key(3), buck2_data::ActionKey::default()).await);
        assert!(!IN_FLIGHT.lock().contains_key(&key(3)));
    }
}
