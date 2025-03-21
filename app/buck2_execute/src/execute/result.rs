/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::Infallible;
use std::fmt::Display;
use std::ops::ControlFlow;
use std::ops::FromResidual;
use std::time::Duration;
use std::time::SystemTime;

use allocative::Allocative;
use buck2_action_metadata_proto::RemoteDepFile;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use derivative::Derivative;
use dupe::Dupe;
use indexmap::IndexMap;
use remote_execution::TActionResult2;

use crate::artifact_value::ArtifactValue;
use crate::execute::claim::Claim;
use crate::execute::dep_file_digest::DepFileDigest;
use crate::execute::kind::CommandExecutionKind;
use crate::execute::output::CommandStdStreams;
use crate::execute::request::CommandExecutionOutput;
use crate::execute::request::ResolvedCommandExecutionOutput;
use crate::output_size::OutputSize;

#[derive(Debug)]
pub enum CommandExecutionErrorType {
    StorageResourceExhausted,
    Other,
}

#[derive(Debug)]
pub enum CommandCancellationReason {
    NotSpecified,
    ReQueueTimeout,
}

/// "Status" of an action execution indicating how it finished. E.g. "built_remotely", "local_fallback", "action_cache".
#[derive(Debug)]
pub enum CommandExecutionStatus {
    Success {
        execution_kind: CommandExecutionKind,
    },
    Failure {
        execution_kind: CommandExecutionKind,
    },
    WorkerFailure {
        execution_kind: CommandExecutionKind,
    },
    Error {
        stage: &'static str,
        error: buck2_error::Error,
        execution_kind: Option<CommandExecutionKind>,
        typ: CommandExecutionErrorType,
    },
    TimedOut {
        execution_kind: CommandExecutionKind,
        duration: Duration,
    },
    // TODO: We should rename this.
    Cancelled {
        reason: Option<CommandCancellationReason>,
    },
}

impl CommandExecutionStatus {
    pub fn execution_kind(&self) -> Option<&CommandExecutionKind> {
        match self {
            CommandExecutionStatus::Success { execution_kind, .. } => Some(execution_kind),
            CommandExecutionStatus::Failure { execution_kind } => Some(execution_kind),
            CommandExecutionStatus::WorkerFailure { execution_kind } => Some(execution_kind),
            CommandExecutionStatus::Error { execution_kind, .. } => execution_kind.as_ref(),
            CommandExecutionStatus::TimedOut { execution_kind, .. } => Some(execution_kind),
            CommandExecutionStatus::Cancelled { reason: _ } => None,
        }
    }
}

impl Display for CommandExecutionStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandExecutionStatus::Success { execution_kind, .. } => {
                write!(f, "success {}", execution_kind,)
            }
            CommandExecutionStatus::WorkerFailure { execution_kind } => {
                write!(f, "worker failure {}", execution_kind,)
            }
            CommandExecutionStatus::Failure { execution_kind } => {
                write!(f, "failure {}", execution_kind,)
            }
            CommandExecutionStatus::Error {
                stage,
                error,
                execution_kind: Some(execution_kind),
                ..
            } => {
                write!(f, "error {}:{}\n{:#}", execution_kind, stage, error)
            }
            CommandExecutionStatus::Error {
                stage,
                error,
                execution_kind: None,
                ..
            } => {
                write!(f, "error:{}\n{:#}", stage, error)
            }
            CommandExecutionStatus::TimedOut { duration, .. } => {
                write!(f, "timed out after {:.3}s", duration.as_secs_f64())
            }
            CommandExecutionStatus::Cancelled { reason } => {
                if let Some(reason) = reason {
                    write!(f, "Cancelled due to {:?}", reason)
                } else {
                    write!(f, "Cancelled")
                }
            }
        }
    }
}

/// Unlike action where we only really have just 1 time, commands can have slightly richer timing
/// data.
#[derive(Debug, Copy, Clone, Dupe, Allocative)]
pub struct CommandExecutionMetadata {
    /// How long this build actually waited for this action to complete
    pub wall_time: Duration,

    /// How long this command actually took to execute. This can be different from the wall_time if
    /// this was e.g. an action cache hit, in which case this field would reflect how long the
    /// command took to actually execute but not how we had to wait for it.
    pub execution_time: Duration,

    /// When execution started.
    pub start_time: SystemTime,

    /// Additional stats.
    pub execution_stats: Option<buck2_data::CommandExecutionStats>,

    /// How long it took to materialize the action's inputs.
    pub input_materialization_duration: Duration,

    /// How long we spent hashing the action's inputs.
    pub hashing_duration: Duration,

    /// How many artifacts we hashed
    pub hashed_artifacts_count: u64,

    /// How long this command spent waiting to run
    pub queue_duration: Option<Duration>,
}

impl CommandExecutionMetadata {
    pub fn end_time(&self) -> SystemTime {
        self.start_time + self.wall_time
    }

    pub fn to_proto(&self) -> buck2_data::CommandExecutionMetadata {
        let metadata = self.dupe();
        buck2_data::CommandExecutionMetadata {
            wall_time: metadata.wall_time.try_into().ok(),
            execution_time: metadata.execution_time.try_into().ok(),
            start_time: Some(metadata.start_time.into()),
            input_materialization_duration: metadata.input_materialization_duration.try_into().ok(),
            execution_stats: metadata.execution_stats,
            hashing_duration: metadata.hashing_duration.try_into().ok(),
            hashed_artifacts_count: metadata.hashed_artifacts_count.try_into().ok().unwrap_or(0),
            queue_duration: metadata.queue_duration.and_then(|d| d.try_into().ok()),
        }
    }
}

impl Default for CommandExecutionMetadata {
    fn default() -> Self {
        Self {
            wall_time: Duration::default(),
            execution_time: Duration::default(),
            start_time: SystemTime::now(),
            execution_stats: None,
            input_materialization_duration: Duration::default(),
            hashing_duration: Duration::default(),
            hashed_artifacts_count: 0,
            queue_duration: None,
        }
    }
}

/// CommandExecutionResult is the result of an executor executing a command.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct CommandExecutionResult {
    /// The outputs produced by this command
    pub outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
    /// How it executed.
    pub report: CommandExecutionReport,
    /// A previously rejected execution of this command.
    pub rejected_execution: Option<CommandExecutionReport>,
    /// Whether this was uploaded to cache, by Buck2.
    pub did_cache_upload: bool,
    /// Whether dep file information for this action was uploaded to cache, by Buck2.
    pub did_dep_file_cache_upload: bool,
    // Remote dep file key, if we did upload a dep file entry
    pub dep_file_key: Option<DepFileDigest>,
    /// Whether this command was eligible for hybrid execution.
    pub eligible_for_full_hybrid: bool,
    /// Execution metadata used for remote dep file lookups.
    /// This is picked up from the action result's auxiliary metadata and
    /// is used to verify the dep file cache lookup result
    pub dep_file_metadata: Option<RemoteDepFile>,
    /// If the action executed on RE, the original action result
    /// to be re-used when uploading the remote dep file.
    #[derivative(Debug = "ignore")]
    pub action_result: Option<TActionResult2>,
}

impl CommandExecutionResult {
    /// Total size of all outputs in bytes.
    pub fn calc_output_size_bytes(&self) -> u64 {
        self.outputs
            .values()
            .map(|v| v.calc_output_count_and_bytes().bytes)
            .sum()
    }

    pub fn was_success(&self) -> bool {
        match self.report.status {
            CommandExecutionStatus::Success { .. } => true,
            _ => false,
        }
    }

    pub fn was_served_by_remote_dep_file_cache(&self) -> bool {
        match self.report.status {
            CommandExecutionStatus::Success {
                execution_kind: CommandExecutionKind::RemoteDepFileCache { .. },
            } => true,
            _ => false,
        }
    }

    pub fn was_remotely_executed(&self) -> bool {
        match self.report.status {
            CommandExecutionStatus::Success {
                execution_kind: CommandExecutionKind::Remote { .. },
            } => true,
            _ => false,
        }
    }

    pub fn was_locally_executed(&self) -> bool {
        match self.report.status {
            CommandExecutionStatus::Success {
                execution_kind: CommandExecutionKind::Local { .. },
            } => true,
            CommandExecutionStatus::Success {
                execution_kind: CommandExecutionKind::LocalWorker { .. },
            } => true,
            _ => false,
        }
    }

    pub fn was_action_cache_hit(&self) -> bool {
        match self.report.status {
            CommandExecutionStatus::Success {
                execution_kind: CommandExecutionKind::ActionCache { .. },
            } => true,
            _ => false,
        }
    }

    pub fn resolve_outputs<'a>(
        &'a self,
        fs: &'a ArtifactFs,
    ) -> impl Iterator<Item = (ResolvedCommandExecutionOutput, &'a ArtifactValue)> + 'a {
        self.outputs
            .iter()
            .map(|(output, value)| (output.as_ref().resolve(fs), value))
    }
}

/// Describes how a command executed.
#[derive(Debug)]
pub struct CommandExecutionReport {
    pub claim: Option<Box<dyn Claim>>,
    pub status: CommandExecutionStatus,
    pub timing: CommandExecutionMetadata,
    pub std_streams: CommandStdStreams,
    /// No exit_code means the command did not finish executing. Signals get mapped into this as
    /// 128 + SIGNUM, which is the convention shells follow.
    pub exit_code: Option<i32>,
    /// Any additional message that a command's executor wants to be user vissible in case of a
    /// failure. Provided by non-Meta RE server.
    pub additional_message: Option<String>,
}

impl CommandExecutionReport {
    pub async fn to_command_execution_proto(
        &self,
        omit_stdout: bool,
        omit_stderr: bool,
        omit_command_details: bool,
    ) -> buck2_data::CommandExecution {
        let details = self
            .to_command_execution_details_proto(omit_stdout, omit_stderr, omit_command_details)
            .await;

        let status = match &self.status {
            CommandExecutionStatus::Success { .. } => {
                buck2_data::command_execution::Success {}.into()
            }
            CommandExecutionStatus::Cancelled { .. } => {
                buck2_data::command_execution::Cancelled {}.into()
            }
            CommandExecutionStatus::Failure { .. } => {
                buck2_data::command_execution::Failure {}.into()
            }
            CommandExecutionStatus::WorkerFailure { .. } => {
                buck2_data::command_execution::WorkerFailure {}.into()
            }
            CommandExecutionStatus::TimedOut { duration, .. } => {
                buck2_data::command_execution::Timeout {
                    duration: (*duration).try_into().ok(),
                }
                .into()
            }
            CommandExecutionStatus::Error { stage, error, .. } => {
                buck2_data::command_execution::Error {
                    stage: (*stage).to_owned(),
                    error: format!("{:#}", error),
                }
                .into()
            }
        };

        buck2_data::CommandExecution {
            details: Some(details),
            status: Some(status),
        }
    }

    async fn to_command_execution_details_proto(
        &self,
        omit_stdout: bool,
        omit_stderr: bool,
        omit_command_details: bool,
    ) -> buck2_data::CommandExecutionDetails {
        // If the top-level command failed then we don't want to omit any details. If it succeeded and
        // so did this command (it could succeed while not having a success here if we have rejected
        // executions), then we'll strip non-relevant stuff.
        let omit_stdout =
            omit_stdout && matches!(self.status, CommandExecutionStatus::Success { .. });

        let signed_exit_code = self.exit_code;

        let std_pair = self.std_streams.to_lossy().await;
        let mut stdout = std_pair.stdout;
        let mut stderr = std_pair.stderr;

        if omit_stdout {
            stdout = "".to_owned();
        }

        if omit_stderr {
            stderr = "".to_owned();
        }

        let command_kind = self
            .status
            .execution_kind()
            .map(|k| k.to_proto(omit_command_details));

        buck2_data::CommandExecutionDetails {
            stdout,
            stderr,
            command_kind,
            signed_exit_code,
            metadata: Some(self.timing.to_proto()),
            additional_message: self.additional_message.clone(),
        }
    }
}

/// Implement FromResidual so that it's easier to refactor functions returning a CommandExecutionResult
/// (it allows to easily factor out early returns into another function and then propagate them with `?`).
impl FromResidual<ControlFlow<Self, Infallible>> for CommandExecutionResult {
    fn from_residual(residual: ControlFlow<Self, Infallible>) -> Self {
        match residual {
            ControlFlow::Break(v) => v,
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_common::cas_digest::CasDigest;
    use sorted_vector_map::SortedVectorMap;

    use super::*;

    fn make_simple_report() -> CommandExecutionReport {
        // The field values correspond to what `make_simple_proto()` builds.
        let status = CommandExecutionStatus::Success {
            execution_kind: CommandExecutionKind::Local {
                digest: CasDigest::new_blake3([0].repeat(32).as_slice().try_into().unwrap(), 123),
                command: vec!["fake_buck2".to_owned()],
                env: {
                    let mut map = SortedVectorMap::new();
                    map.insert("FAKE_ENV_VAR".to_owned(), "1".to_owned());
                    map
                },
            },
        };
        let timing = CommandExecutionMetadata {
            wall_time: Duration::from_secs(2),
            execution_time: Duration::from_secs(3),
            start_time: SystemTime::UNIX_EPOCH,
            execution_stats: Some(buck2_data::CommandExecutionStats {
                cpu_instructions_user: Some(4),
                cpu_instructions_kernel: Some(5),
                userspace_events: Some(buck2_data::CpuCounter {
                    count: 4,
                    time_enabled: 100,
                    time_running: 100,
                }),
                kernel_events: Some(buck2_data::CpuCounter {
                    count: 10,
                    time_enabled: 50,
                    time_running: 100,
                }),
                memory_peak: None,
            }),
            input_materialization_duration: Duration::from_secs(6),
            hashing_duration: Duration::from_secs(7),
            hashed_artifacts_count: 8,
            queue_duration: Some(Duration::from_secs(9)),
        };
        let std_streams = CommandStdStreams::Local {
            stdout: [65, 66, 67].to_vec(), // ABC
            stderr: [68, 69, 70].to_vec(), // DEF
        };

        CommandExecutionReport {
            claim: None,
            status,
            timing,
            std_streams,
            exit_code: Some(456),
            additional_message: None,
        }
    }

    fn make_simple_proto() -> buck2_data::CommandExecution {
        // The field values correspond to what `make_simple_report()` builds.
        use prost_types::Duration;
        use prost_types::Timestamp;

        let command_execution_kind = buck2_data::CommandExecutionKind {
            command: Some(buck2_data::command_execution_kind::Command::LocalCommand(
                buck2_data::LocalCommand {
                    argv: vec!["fake_buck2".to_owned()],
                    env: vec![buck2_data::EnvironmentEntry {
                        key: "FAKE_ENV_VAR".to_owned(),
                        value: "1".to_owned(),
                    }],
                    action_digest: format!("{}:{}", "0".repeat(64), "123"),
                },
            )),
        };
        let command_execution_stats = buck2_data::CommandExecutionStats {
            cpu_instructions_user: Some(4),
            cpu_instructions_kernel: Some(5),
            userspace_events: Some(buck2_data::CpuCounter {
                count: 4,
                time_enabled: 100,
                time_running: 100,
            }),
            kernel_events: Some(buck2_data::CpuCounter {
                count: 10,
                time_enabled: 50,
                time_running: 100,
            }),
            memory_peak: None,
        };
        let command_execution_metadata = buck2_data::CommandExecutionMetadata {
            wall_time: Some(Duration {
                seconds: 2,
                nanos: 0,
            }),
            execution_time: Some(Duration {
                seconds: 3,
                nanos: 0,
            }),
            start_time: Some(Timestamp {
                seconds: 0, // UNIX_EPOCH
                nanos: 0,
            }),
            input_materialization_duration: Some(Duration {
                seconds: 6,
                nanos: 0,
            }),
            execution_stats: Some(command_execution_stats),
            hashing_duration: Some(Duration {
                seconds: 7,
                nanos: 0,
            }),
            hashed_artifacts_count: 8,
            queue_duration: Some(Duration {
                seconds: 9,
                nanos: 0,
            }),
        };
        let command_execution_details = buck2_data::CommandExecutionDetails {
            signed_exit_code: Some(456),
            stdout: "ABC".to_owned(),
            stderr: "DEF".to_owned(),
            command_kind: Some(command_execution_kind),
            metadata: Some(command_execution_metadata),
            additional_message: None,
        };

        buck2_data::CommandExecution {
            details: Some(command_execution_details),
            status: Some(buck2_data::command_execution::Status::Success(
                buck2_data::command_execution::Success {},
            )),
        }
    }

    #[tokio::test]
    async fn test_to_command_execution_proto() {
        let report = make_simple_report();
        let proto = report.to_command_execution_proto(false, false, false).await;
        let expected_proto = make_simple_proto();

        assert_eq!(proto, expected_proto);
    }

    #[tokio::test]
    async fn test_to_command_execution_proto_omit_stdout() {
        let report = make_simple_report();
        let proto = report.to_command_execution_proto(true, false, false).await;
        let mut expected_proto = make_simple_proto();

        expected_proto.details.as_mut().unwrap().stdout = "".to_owned();

        assert_eq!(proto, expected_proto);
    }

    #[tokio::test]
    async fn test_to_command_execution_proto_omit_stderr() {
        let report = make_simple_report();
        let proto = report.to_command_execution_proto(false, true, false).await;
        let mut expected_proto = make_simple_proto();

        expected_proto.details.as_mut().unwrap().stderr = "".to_owned();

        assert_eq!(proto, expected_proto);
    }

    #[tokio::test]
    async fn test_to_command_execution_proto_omit_command_details() {
        let report = make_simple_report();
        let proto = report.to_command_execution_proto(false, false, true).await;
        let mut expected_proto = make_simple_proto();

        expected_proto
            .details
            .as_mut()
            .unwrap()
            .command_kind
            .as_mut()
            .unwrap()
            .command = Some(
            buck2_data::command_execution_kind::Command::OmittedLocalCommand(
                buck2_data::OmittedLocalCommand {
                    action_digest: format!("{}:{}", "0".repeat(64), "123"),
                },
            ),
        );

        assert_eq!(proto, expected_proto);
    }
}
