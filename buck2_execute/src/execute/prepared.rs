use async_trait::async_trait;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_node::execute::config::RemoteExecutorUseCase;
use remote_execution as RE;

use crate::directory::ActionImmutableDirectory;
use crate::execute::action_digest::ActionDigest;
use crate::execute::blobs::ActionBlobs;
use crate::execute::manager::CommandExecutionManager;
use crate::execute::name::ExecutorName;
use crate::execute::request::CommandExecutionRequest;
use crate::execute::result::CommandExecutionResult;
use crate::execute::target::CommandExecutionTarget;

pub struct ActionPaths {
    pub inputs: ActionImmutableDirectory,
    pub outputs: Vec<ProjectRelativePathBuf>,

    /// Total size of input files.
    pub input_files_bytes: u64,
}

pub struct PreparedAction {
    pub action: ActionDigest,
    // The encoded action and other messages referenced from it by digest (e.g. RE::Command).
    // Does not include the files referenced in inputs.
    pub blobs: ActionBlobs,
}

pub struct PreparedCommand<'a, 'b> {
    pub request: &'a CommandExecutionRequest,
    pub target: CommandExecutionTarget<'b>,
    pub action_paths: ActionPaths,
    pub prepared_action: PreparedAction,
}

#[async_trait]
pub trait PreparedCommandExecutor: Send + Sync {
    /// Execute a command.
    ///
    /// This intentionally does not return a Result since we want to capture information about the
    /// execution even if there are errors. Any errors can be propagated by converting them
    /// to a result with CommandExecutionManager::error.
    async fn exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
    ) -> CommandExecutionResult;

    fn re_platform(&self) -> Option<&RE::Platform>;

    fn re_use_case(&self) -> RemoteExecutorUseCase;

    fn name(&self) -> ExecutorName;
}
