/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use derive_more::Display;
use remote_execution as RE;
use sorted_vector_map::SortedVectorMap;

use crate::execute::action_digest::ActionDigest;

#[derive(Debug, Display, Clone)]
pub enum CommandExecutionKind {
    /// This action was executed locally.
    #[display(fmt = "local")]
    Local {
        // Even though this did not run on RE, we still produced this, so we might as well report
        // it.
        digest: ActionDigest,
        command: Vec<String>,
        env: SortedVectorMap<String, String>,
    },
    /// This action was executed via a remote executor.
    #[display(fmt = "remote")]
    Remote {
        details: RemoteCommandExecutionDetails,

        /// How long this command queued in RE. This value excludes execution time, i.e. for action cache hit,
        /// this value represents how long a request has to wait for server to handle.
        queue_time: Duration,
    },
    /// This action was served by the action cache and not executed.
    #[display(fmt = "action_cache")]
    ActionCache {
        details: RemoteCommandExecutionDetails,
    },
    /// This action would have executed via a local worker but failed during worker initialization.
    #[display(fmt = "worker_init")]
    LocalWorkerInit {
        command: Vec<String>,
        env: SortedVectorMap<String, String>,
    },
    /// This action was executed via a local worker.
    #[display(fmt = "worker")]
    LocalWorker {
        digest: ActionDigest,
        command: Vec<String>,
        env: SortedVectorMap<String, String>,
        fallback_exe: Vec<String>,
    },
}

impl CommandExecutionKind {
    pub fn as_enum(&self) -> buck2_data::ActionExecutionKind {
        match self {
            Self::Local { .. } => buck2_data::ActionExecutionKind::Local,
            Self::LocalWorker { .. } | Self::LocalWorkerInit { .. } => {
                buck2_data::ActionExecutionKind::LocalWorker
            }
            Self::Remote { .. } => buck2_data::ActionExecutionKind::Remote,
            Self::ActionCache { .. } => buck2_data::ActionExecutionKind::ActionCache,
        }
    }

    pub fn to_proto<T>(&self, omit_details: bool) -> T
    where
        T: From<buck2_data::LocalCommand>
            + From<buck2_data::RemoteCommand>
            + From<buck2_data::OmittedLocalCommand>
            + From<buck2_data::WorkerInitCommand>
            + From<buck2_data::WorkerCommand>,
    {
        match self {
            Self::Local {
                command,
                env,
                digest,
            } => {
                if omit_details {
                    buck2_data::OmittedLocalCommand {
                        action_digest: digest.to_string(),
                    }
                    .into()
                } else {
                    buck2_data::LocalCommand {
                        action_digest: digest.to_string(),
                        argv: command.to_owned(),
                        env: env
                            .iter()
                            .map(|(key, value)| buck2_data::EnvironmentEntry {
                                key: key.clone(),
                                value: value.clone(),
                            })
                            .collect(),
                    }
                    .into()
                }
            }
            Self::Remote {
                details,
                queue_time,
            } => buck2_data::RemoteCommand {
                action_digest: details.action_digest.to_string(),
                cache_hit: false,
                queue_time: (*queue_time).try_into().ok(),
            }
            .into(),
            Self::ActionCache { details } => buck2_data::RemoteCommand {
                action_digest: details.action_digest.to_string(),
                cache_hit: true,
                queue_time: None,
            }
            .into(),
            Self::LocalWorkerInit { command, env } => buck2_data::WorkerInitCommand {
                argv: command.to_owned(),
                env: env
                    .iter()
                    .map(|(key, value)| buck2_data::EnvironmentEntry {
                        key: key.clone(),
                        value: value.clone(),
                    })
                    .collect(),
            }
            .into(),
            Self::LocalWorker {
                command,
                env,
                digest,
                fallback_exe,
            } => buck2_data::WorkerCommand {
                action_digest: digest.to_string(),
                argv: command.to_owned(),
                env: env
                    .iter()
                    .map(|(key, value)| buck2_data::EnvironmentEntry {
                        key: key.clone(),
                        value: value.clone(),
                    })
                    .collect(),
                fallback_exe: fallback_exe.to_owned(),
            }
            .into(),
        }
    }
}

/// Structured data for a RE request.
#[derive(Debug, Clone)]
pub struct RemoteCommandExecutionDetails {
    pub action_digest: ActionDigest,
    pub session_id: Option<String>,
    pub use_case: RemoteExecutorUseCase,
    pub platform: RE::Platform,
}
