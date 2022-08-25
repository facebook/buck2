use buck2_node::execute::config::RemoteExecutorUseCase;
use remote_execution::RemoteExecutionMetadata;

pub trait RemoteExecutionMetadataExt {
    fn metadata(&self) -> RemoteExecutionMetadata;
}

impl RemoteExecutionMetadataExt for RemoteExecutorUseCase {
    fn metadata(&self) -> RemoteExecutionMetadata {
        RemoteExecutionMetadata {
            use_case_id: self.as_str().to_owned(),
            ..Default::default()
        }
    }
}
