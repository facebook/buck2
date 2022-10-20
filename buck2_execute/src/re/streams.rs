/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::executor_config::RemoteExecutorUseCase;
use derivative::Derivative;
use futures::future;
use gazebo::dupe::Dupe;
use remote_execution::TActionResult2;

use crate::execute::output::ReStdStream;
use crate::re::manager::ManagedRemoteExecutionClient;

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct RemoteCommandStdStreams {
    #[derivative(Debug = "ignore")]
    client: ManagedRemoteExecutionClient,
    #[derivative(Debug = "ignore")]
    use_case: RemoteExecutorUseCase,
    stdout: ReStdStream,
    stderr: ReStdStream,
}

impl RemoteCommandStdStreams {
    pub fn new(
        action_result: &TActionResult2,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
    ) -> Self {
        let stdout = ReStdStream::new(
            action_result.stdout_raw.clone(),
            action_result.stdout_digest.clone(),
        );
        let stderr = ReStdStream::new(
            action_result.stderr_raw.clone(),
            action_result.stderr_digest.clone(),
        );

        Self {
            client: client.dupe(),
            use_case,
            stdout,
            stderr,
        }
    }

    pub async fn prefetch_lossy_stderr(mut self) -> Self {
        self.stderr
            .prefetch_lossy(&self.client, self.use_case)
            .await;
        self
    }

    pub(crate) async fn to_lossy_stdout(&self) -> String {
        self.stdout.to_lossy(&self.client, self.use_case).await
    }

    pub(crate) async fn to_lossy_stderr(&self) -> String {
        self.stderr.to_lossy(&self.client, self.use_case).await
    }

    pub(crate) async fn into_stdout_stderr_bytes(self) -> anyhow::Result<(Vec<u8>, Vec<u8>)> {
        future::try_join(
            self.stdout.into_bytes(&self.client, self.use_case),
            self.stderr.into_bytes(&self.client, self.use_case),
        )
        .await
    }

    pub(crate) fn use_case(&self) -> RemoteExecutorUseCase {
        self.use_case
    }

    pub(crate) fn into_stdout_stderr(self) -> (ReStdStream, ReStdStream) {
        (self.stdout, self.stderr)
    }
}
