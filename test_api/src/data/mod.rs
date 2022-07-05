//! Core data objects used in the protocol

mod convert;

use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::time::Duration;
use std::time::SystemTime;

use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use derive_more::From;
use gazebo::prelude::*;
use host_sharing::HostSharingRequirements;

/// A handle generated by the TestOrchestrator. It can be used by the TestExecutor to access this
/// target.
#[derive(Debug, Copy, Clone, Dupe, From, Hash, Eq, PartialEq)]
pub struct ConfiguredTargetHandle(u64);

/// The Target of a test rule
#[derive(Debug, Clone, PartialEq)]
pub struct ConfiguredTarget {
    pub handle: ConfiguredTargetHandle,
    /// Legacy name
    pub name: String,
    /// Structured data
    pub cell: String,
    pub package: String,
    pub target: String,
    pub configuration: String,
}

/// Metadata about the execution to display
#[derive(Debug, Clone, PartialEq)]
pub enum DisplayMetadata {
    // Listing the test binary to discover tests. The String is the name of the suite at the binary
    Listing(String),
    // the name of the test(s) that we are running for the suite of a target
    Testing {
        suite: String,
        testcases: Vec<String>,
    },
}

#[derive(Clone, PartialEq)]
pub enum ExecutionStream {
    Inline(Vec<u8>),
}

impl Debug for ExecutionStream {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Inline(d) => {
                write!(f, "{}", String::from_utf8_lossy(d))
            }
        }
    }
}

#[derive(Clone, Debug, Dupe, PartialEq)]
pub enum ExecutionStatus {
    Finished { exitcode: i32 },
    TimedOut { duration: Duration },
}

/// The result of running a test
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TestResult {
    // the target the test came from
    pub target: ConfiguredTargetHandle,
    // the name of the test
    pub name: String,
    // the status of running the test
    pub status: TestStatus,
    // additional optional messages
    pub msg: Option<String>,
    // the duration of the test run
    // TODO(skcd) should this be optional? why doesn't everything have duration
    pub duration: Option<Duration>,
    // the output of the test execution (combining stdout and stderr)
    pub details: String,
}

/// different possible test results
#[derive(PartialEq, Eq, Debug, Clone, Dupe)]
#[allow(non_camel_case_types)]
pub enum TestStatus {
    PASS,
    FAIL,
    SKIP,
    OMITTED,
    FATAL,
    TIMEOUT,
    // There is something called unknown, adding it here for now,
    // we can change it later on.
    UNKNOWN,
    // We also have re-runs
    RERUN,
    LISTING_SUCCESS,
    LISTING_FAILED,
}

/// The set of information about a test rule that is passed to the test executor
#[derive(Clone, Debug, PartialEq)]
pub struct ExternalRunnerSpec {
    /// Target the spec belongs to
    pub target: ConfiguredTarget,
    /// Type of test spec
    pub test_type: String,
    /// Base command used for further processing. A mix of verbatim arguments and
    /// opaque handles for more complex arguments.
    pub command: Vec<ExternalRunnerSpecValue>,
    /// Environment variables a specified by the rule. A mapping from keys to
    /// verbatim values or opaque handles for more complex values.
    pub env: HashMap<String, ExternalRunnerSpecValue>,
    /// Labels defined on the rule.
    pub labels: Vec<String>,
    /// Contacts defined on the rule.
    pub contacts: Vec<String>,
}

/// Command line argument or environment variable value
///
/// It is either a verbatim string, or a reference to a more complex value that's opaque to the
/// test run coordinator.
#[derive(Clone, Debug, PartialEq)]
pub enum ExternalRunnerSpecValue {
    Verbatim(String),
    ArgHandle(ArgHandle),
    EnvHandle(EnvHandle),
}

/// Handle referring to a complex argument defined on the test rule
#[derive(Clone, Debug, Dupe, PartialEq, From)]
pub struct ArgHandle(pub usize);

impl TryFrom<i64> for ArgHandle {
    type Error = anyhow::Error;

    fn try_from(i: i64) -> Result<Self, Self::Error> {
        Ok(ArgHandle(i.try_into()?))
    }
}

/// Handle referring to a complex environment value defined on the test rule
#[derive(Clone, Debug, PartialEq, From)]
pub struct EnvHandle(pub String);

#[derive(Clone, Debug, PartialEq)]
pub struct ArgValue {
    pub content: ArgValueContent,
    pub format: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArgValueContent {
    ExternalRunnerSpecValue(ExternalRunnerSpecValue),
    DeclaredOutput(DeclaredOutput),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DeclaredOutput {
    pub name: ForwardRelativePathBuf,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ExecutorConfigOverride {
    pub name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExecuteRequest2 {
    pub test_executable: TestExecutable,
    pub timeout: Duration,
    pub host_sharing_requirements: HostSharingRequirements,
    pub executor_override: Option<ExecutorConfigOverride>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Output {
    LocalPath(AbsPathBuf),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExecutionResult2 {
    pub status: ExecutionStatus,
    pub stdout: ExecutionStream,
    pub stderr: ExecutionStream,
    pub outputs: HashMap<DeclaredOutput, Output>,
    pub start_time: SystemTime,
    pub execution_time: Duration,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TestExecutable {
    pub ui_prints: DisplayMetadata,
    pub target: ConfiguredTargetHandle,
    pub cmd: Vec<ArgValue>,
    pub env: HashMap<String, ArgValue>,
    pub pre_create_dirs: Vec<DeclaredOutput>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrepareForLocalExecutionResult {
    pub cmd: Vec<String>,
    pub env: HashMap<String, String>,
    pub cwd: AbsPathBuf,
}

pub mod testing {
    use crate::data::ConfiguredTargetHandle;

    pub trait ConfiguredTargetHandleExt {
        fn testing_new(id: u64) -> Self;
    }

    impl ConfiguredTargetHandleExt for ConfiguredTargetHandle {
        fn testing_new(id: u64) -> Self {
            Self(id)
        }
    }
}
