/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use allocative::Allocative;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_data::RePlatform;
use derive_more::Display;
use gazebo::prelude::SliceExt;
use remote_execution as RE;
use sorted_vector_map::SortedVectorMap;

use crate::execute::action_digest::ActionDigest;
use crate::execute::dep_file_digest::DepFileDigest;
use crate::re::convert::platform_to_proto;

#[derive(Debug, Display, Clone, Allocative)]
pub enum CommandExecutionKind {
    /// This action was executed locally.
    #[display("local")]
    Local {
        // Even though this did not run on RE, we still produced this, so we might as well report
        // it.
        digest: ActionDigest,
        command: Vec<String>,
        env: SortedVectorMap<String, String>,
    },
    /// This action was executed via a remote executor.
    #[display("remote")]
    Remote {
        details: RemoteCommandExecutionDetails,

        /// How long this command queued in RE. This value excludes execution time, i.e. for action cache hit,
        /// this value represents how long a request has to wait for server to handle.
        queue_time: Duration,
        /// Local paths to the materialized inputs for failed actions, if `--materialize-failed-inputs`
        /// was passed to build options
        materialized_inputs_for_failed: Option<Vec<ProjectRelativePathBuf>>,
        /// Local paths to the materialized outputs for failed actions, if `--unstable-materialize-failed-action-outputs`
        /// was passed to build options
        materialized_outputs_for_failed_actions: Option<Vec<ProjectRelativePathBuf>>,
    },
    /// This action was served by the action cache and not executed.
    #[display("action_cache")]
    ActionCache {
        details: RemoteCommandExecutionDetails,
    },
    /// This action was served by the action cache (remote dep file) and not executed.
    #[display("remote_dep_file_cache")]
    RemoteDepFileCache {
        details: RemoteCommandExecutionDetails,
    },
    /// This action would have executed via a local worker but failed during worker initialization.
    #[display("worker_init")]
    LocalWorkerInit {
        command: Vec<String>,
        env: SortedVectorMap<String, String>,
    },
    /// This action was executed via a local worker.
    #[display("worker")]
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
            Self::RemoteDepFileCache { .. } => buck2_data::ActionExecutionKind::RemoteDepFileCache,
        }
    }

    pub fn to_proto(&self, omit_details: bool) -> buck2_data::CommandExecutionKind {
        use buck2_data::command_execution_kind::Command;

        let command = Some(match self {
            Self::Local {
                command,
                env,
                digest,
            } => {
                if omit_details {
                    Command::OmittedLocalCommand(buck2_data::OmittedLocalCommand {
                        action_digest: digest.to_string(),
                    })
                } else {
                    Command::LocalCommand(buck2_data::LocalCommand {
                        action_digest: digest.to_string(),
                        argv: command.to_owned(),
                        env: env
                            .iter()
                            .map(|(key, value)| buck2_data::EnvironmentEntry {
                                key: key.clone(),
                                value: value.clone(),
                            })
                            .collect(),
                    })
                }
            }
            Self::Remote {
                details,
                queue_time,
                materialized_inputs_for_failed,
                materialized_outputs_for_failed_actions,
            } => Command::RemoteCommand(buck2_data::RemoteCommand {
                action_digest: details.action_digest.to_string(),
                cache_hit: false,
                cache_hit_type: buck2_data::CacheHitType::Executed.into(),
                remote_dep_file_key: None,
                queue_time: (*queue_time).try_into().ok(),
                details: details.to_proto(omit_details),
                materialized_inputs_for_failed: materialized_inputs_for_failed
                    .as_ref()
                    .map(|paths| paths.clone().map(|p| format!("{}", p)))
                    .unwrap_or_default(),
                materialized_outputs_for_failed_actions: materialized_outputs_for_failed_actions
                    .as_ref()
                    .map(|paths| paths.clone().map(|p| format!("{}", p)))
                    .unwrap_or_default(),
            }),

            Self::ActionCache { details } => Command::RemoteCommand(buck2_data::RemoteCommand {
                action_digest: details.action_digest.to_string(),
                cache_hit: true,
                cache_hit_type: buck2_data::CacheHitType::ActionCache.into(),
                queue_time: None,
                details: details.to_proto(omit_details),
                remote_dep_file_key: None,
                materialized_inputs_for_failed: Vec::new(),
                materialized_outputs_for_failed_actions: Vec::new(),
            }),

            Self::RemoteDepFileCache { details } => {
                Command::RemoteCommand(buck2_data::RemoteCommand {
                    action_digest: details.action_digest.to_string(),
                    cache_hit: true,
                    cache_hit_type: buck2_data::CacheHitType::RemoteDepFileCache.into(),
                    queue_time: None,
                    details: details.to_proto(omit_details),
                    remote_dep_file_key: details
                        .remote_dep_file_key
                        .as_ref()
                        .map(|k| k.to_string()),
                    materialized_inputs_for_failed: Vec::new(),
                    materialized_outputs_for_failed_actions: Vec::new(),
                })
            }

            Self::LocalWorkerInit { command, env } => {
                Command::WorkerInitCommand(buck2_data::WorkerInitCommand {
                    argv: command.to_owned(),
                    env: env
                        .iter()
                        .map(|(key, value)| buck2_data::EnvironmentEntry {
                            key: key.clone(),
                            value: value.clone(),
                        })
                        .collect(),
                })
            }

            Self::LocalWorker {
                command,
                env,
                digest,
                fallback_exe,
            } => Command::WorkerCommand(buck2_data::WorkerCommand {
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
            }),
        });

        buck2_data::CommandExecutionKind { command }
    }
}

/// Structured data for a RE request.
#[derive(Debug, Clone, Allocative)]
pub struct RemoteCommandExecutionDetails {
    pub action_digest: ActionDigest,
    pub remote_dep_file_key: Option<DepFileDigest>,
    pub session_id: Option<String>,
    pub use_case: RemoteExecutorUseCase,
    pub platform: RePlatform,
}

impl RemoteCommandExecutionDetails {
    pub fn new(
        action_digest: ActionDigest,
        remote_dep_file_key: Option<DepFileDigest>,
        session_id: Option<String>,
        use_case: RemoteExecutorUseCase,
        platform: &RE::Platform,
    ) -> Self {
        Self {
            action_digest,
            remote_dep_file_key,
            session_id,
            use_case,
            platform: platform_to_proto(platform),
        }
    }

    fn to_proto(&self, omit_details: bool) -> Option<buck2_data::RemoteCommandDetails> {
        if omit_details {
            return None;
        }

        Some(buck2_data::RemoteCommandDetails {
            session_id: self.session_id.clone(),
            use_case: self.use_case.to_string(),
            platform: Some(self.platform.clone()),
        })
    }
}
