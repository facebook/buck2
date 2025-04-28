/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use derivative::Derivative;
use dupe::Dupe;
use futures::future;
use remote_execution::TActionResult2;

use crate::digest_config::DigestConfig;
use crate::execute::output::ReStdStream;
use crate::re::manager::ManagedRemoteExecutionClient;

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct RemoteCommandStdStreams {
    #[derivative(Debug = "ignore")]
    client: ManagedRemoteExecutionClient,
    #[derivative(Debug = "ignore")]
    digest_config: DigestConfig,
    stdout: ReStdStream,
    stderr: ReStdStream,
}

impl RemoteCommandStdStreams {
    pub fn new(
        action_result: &TActionResult2,
        client: &ManagedRemoteExecutionClient,
        digest_config: DigestConfig,
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
            digest_config,
            stdout,
            stderr,
        }
    }

    pub async fn prefetch_lossy_stderr(mut self) -> Self {
        self.stderr
            .prefetch_lossy(&self.client, self.digest_config)
            .await;
        self
    }

    pub(crate) async fn to_lossy_stdout(&self) -> String {
        self.stdout.to_lossy(&self.client, self.digest_config).await
    }

    pub(crate) async fn to_lossy_stderr(&self) -> String {
        self.stderr.to_lossy(&self.client, self.digest_config).await
    }

    pub(crate) async fn into_stdout_stderr_bytes(self) -> buck2_error::Result<(Vec<u8>, Vec<u8>)> {
        future::try_join(
            self.stdout.into_bytes(&self.client, self.digest_config),
            self.stderr.into_bytes(&self.client, self.digest_config),
        )
        .await
    }

    pub(crate) fn use_case(&self) -> RemoteExecutorUseCase {
        self.client.use_case
    }

    pub(crate) fn into_stdout_stderr(self) -> (ReStdStream, ReStdStream) {
        (self.stdout, self.stderr)
    }
}
