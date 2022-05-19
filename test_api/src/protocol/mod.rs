//! The traits that defines the protocol between buck and a test executor.
//!
//! Test executors are expected to implement the trait `TestExecutor`. Buck will need to implement
//! the trait `Buck` for the test executor to interact against.

pub mod convert;

use std::{collections::HashMap, time::Duration};

use gazebo::prelude::*;
use host_sharing::HostSharingRequirements;

use crate::data::{
    ArgValue, ConfiguredTargetHandle, DeclaredOutput, DisplayMetadata, ExecutionResult2,
    ExternalRunnerSpec, TestResult,
};

/// available to buck to interact with the test executor
#[async_trait::async_trait]
pub trait TestExecutor: Send + Sync {
    /// sends an external runner spec to the test executor
    async fn external_runner_spec(&self, s: ExternalRunnerSpec) -> anyhow::Result<()>;

    // report that there are no more test specs to send
    async fn end_of_test_requests(&self) -> anyhow::Result<()>;
}

// available to the test executor to interact with the orchestrator
#[async_trait::async_trait]
pub trait TestOrchestrator: Send + Sync {
    /// executes the given command and returns the result of the execution
    async fn execute2(
        &self,
        // information about this execute request for Buck's UX
        ui_prints: DisplayMetadata,
        // the label of the rule being tested
        target: ConfiguredTargetHandle,
        // the command to run
        cmd: Vec<ArgValue>,
        // environment variables to set at runtime
        env: HashMap<String, ArgValue>,
        // timeout for command
        timeout: Duration,
        // parameters used to effectively share the executor host for this command.
        host_sharing_requirements: HostSharingRequirements,
        // outputs that need to be pre created as directories
        pre_create_dirs: Vec<DeclaredOutput>,
    ) -> anyhow::Result<ExecutionResult2>;

    /// reports a test is done
    async fn report_test_result(&self, r: TestResult) -> anyhow::Result<()>;

    async fn report_tests_discovered(
        &self,
        target: ConfiguredTargetHandle,
        suite: String,
        name: Vec<String>,
    ) -> anyhow::Result<()>;

    /// report a summary about the current test executor
    async fn report_test_session(
        &self,
        session_id: String,
        session_info: String,
    ) -> anyhow::Result<()>;

    /// report that all tests are done and provide the exit code that this test executor wants to
    /// return for the test command, no more executions
    async fn end_of_test_results(&self, exit_code: i32) -> anyhow::Result<()>;
}

// TODO need to figure out what this is. we can go without it for now
#[derive(Debug, Default, Clone, Dupe)]
pub struct ExecPlatformRefinement;

/// the template name for external test executor to fill with the test execution location
pub const OUTPUT_DIR: &str = "output_dir";

/// the template name for buck to fill with the commands outputs
pub const OUTPUTS_TEMPLATE: &str = "outputs";

/// the template name for buck to fill with the run cmd based on the test's run information
/// this should be multi-arity
pub const TEST_RUN_CMD: &str = "test_run_cmd";

/// the template name for buck to fill with the run environment based on the test's run information
/// this should be multi-arity
pub const TEST_RUN_ENV_KEYS: &str = "test_run_env_keys";

/// the template name for buck to fill with the run environment based on the test's run information
/// this should be multi-arity
pub const TEST_RUN_ENV_VALUES: &str = "test_run_env_values";

/// the template name for the external test runner to fill with test filters
pub const TEST_FILTER_TEMPLATE: &str = "test_filter";
