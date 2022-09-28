use std::convert::Infallible;
use std::fmt::Display;
use std::ops::ControlFlow;
use std::ops::FromResidual;
use std::time::Duration;
use std::time::SystemTime;

use gazebo::dupe::Dupe;
use indexmap::IndexMap;

use crate::artifact_value::ArtifactValue;
use crate::execute::claim::Claim;
use crate::execute::kind::CommandExecutionKind;
use crate::execute::name::ExecutorName;
use crate::execute::output::CommandStdStreams;
use crate::execute::request::CommandExecutionOutput;

/// "Status" of an action execution indicating how it finished. E.g. "built_remotely", "local_fallback", "action_cache".
#[derive(Debug)]
pub enum CommandExecutionStatus {
    Success {
        execution_kind: CommandExecutionKind,
    },
    Failure {
        execution_kind: CommandExecutionKind,
    },
    Error {
        stage: String,
        error: anyhow::Error,
    },
    TimedOut {
        execution_kind: CommandExecutionKind,
        duration: Duration,
    },
    ClaimRejected,
    ClaimCancelled,
}

impl CommandExecutionStatus {
    pub fn execution_kind(&self) -> Option<&CommandExecutionKind> {
        match self {
            CommandExecutionStatus::Success { execution_kind, .. } => Some(execution_kind),
            CommandExecutionStatus::Failure { execution_kind } => Some(execution_kind),
            CommandExecutionStatus::Error { .. } => None,
            CommandExecutionStatus::TimedOut { execution_kind, .. } => Some(execution_kind),
            CommandExecutionStatus::ClaimRejected => None,
            CommandExecutionStatus::ClaimCancelled => None,
        }
    }
}

impl Display for CommandExecutionStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandExecutionStatus::Success { execution_kind, .. } => {
                write!(f, "success {}", execution_kind,)
            }
            CommandExecutionStatus::Failure { execution_kind } => {
                write!(f, "failure {}", execution_kind,)
            }
            CommandExecutionStatus::Error { stage, error } => {
                write!(f, "error:{}\n{:#}", stage, error)
            }
            CommandExecutionStatus::TimedOut { duration, .. } => {
                write!(f, "timed out after {:.3}s", duration.as_secs_f64())
            }
            CommandExecutionStatus::ClaimRejected => write!(f, "ClaimRejected"),
            CommandExecutionStatus::ClaimCancelled => write!(f, "ClaimCancelled"),
        }
    }
}

/// Unlike action where we only really have just 1 time, commands can have slightly richer timing
/// data.
#[derive(Debug, Copy, Clone, Dupe)]
pub struct CommandExecutionTimingData {
    /// How long this build actually waited for this action to complete
    pub wall_time: Duration,

    // How long this command queued in RE. This value excludes execution time, i.e. for action cache hit,
    // this value represents how long a request has to wait for server to handle.
    pub re_queue_time: Option<Duration>,

    /// How long this command actually took to execute. This can be different from the wall_time if
    /// this was e.g. an action cache hit, in which case this field would reflect how long the
    /// command took to actually execute but not how we had to wait for it.
    pub execution_time: Duration,

    /// When execution started.
    pub start_time: SystemTime,
}

impl Default for CommandExecutionTimingData {
    fn default() -> Self {
        Self {
            wall_time: Duration::default(),
            re_queue_time: None,
            execution_time: Duration::default(),
            start_time: SystemTime::now(),
        }
    }
}

/// CommandExecutionResult is the result of an executor executing a command.
#[derive(Debug)]
pub struct CommandExecutionResult {
    /// The outputs produced by this command
    pub outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
    /// How it executed.
    pub report: CommandExecutionReport,
    /// A previously rejected execution of this command.
    pub rejected_execution: Option<CommandExecutionReport>,
    /// Whether this was uploaded to cache, by Buck2.
    pub did_cache_upload: bool,
}

/// Describes how a command executed.
#[derive(Debug)]
pub struct CommandExecutionReport {
    pub claim: Option<Box<dyn Claim>>,
    pub status: CommandExecutionStatus,
    pub executor: ExecutorName,
    pub timing: CommandExecutionTimingData,
    pub std_streams: CommandStdStreams,
    pub exit_code: Option<i32>,
}

/// Implement FromResidual so that it's easier to refactor functions returning a CommandExecutionResult
/// (it allows to easily factor out early returns into another function and then propagate them with `?`).
impl FromResidual<ControlFlow<Self, Infallible>> for CommandExecutionResult {
    fn from_residual(residual: ControlFlow<Self, Infallible>) -> Self {
        match residual {
            ControlFlow::Break(v) => v,
            ControlFlow::Continue(_) => unreachable!(),
        }
    }
}
