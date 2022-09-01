use anyhow::Context as _;
use async_trait::async_trait;
use buck2_common::file_ops::FileDigest;
use buck2_execute::digest::FileDigestFromReExt;
use buck2_execute::execute::output::CommandStdStreams;
use buck2_execute::execute::output::ReStdStream;
use buck2_execute::execute::output::RemoteCommandStdStreamsDyn;
use buck2_execute::execute::output::StdStreamPair;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use buck2_node::execute::config::RemoteExecutorUseCase;
use derivative::Derivative;
use futures::future;
use gazebo::prelude::*;
use remote_execution::TActionResult2;

#[async_trait]
pub(crate) trait ReStdStreamExt {
    async fn to_lossy(
        &self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
    ) -> String;

    async fn into_bytes(
        self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<Vec<u8>>;

    async fn prefetch_lossy(
        &mut self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
    );
}

#[async_trait]
impl ReStdStreamExt for ReStdStream {
    async fn to_lossy(
        &self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
    ) -> String {
        // 4MBs seems like a reasonably large volume of output. There is no research or science behind
        // this number.
        const MAX_STREAM_DOWNLOAD_SIZE: i64 = 4 * 1024 * 1024;

        match self {
            Self::Raw(raw) => String::from_utf8_lossy(raw).into_owned(),
            Self::Digest(digest) if digest.size_in_bytes <= MAX_STREAM_DOWNLOAD_SIZE => {
                match client.download_blob(digest, use_case).await {
                    Ok(bytes) => String::from_utf8_lossy(&bytes).to_string(),
                    Err(e) => {
                        tracing::warn!("Failed to download action stderr: {:#}", e);
                        format!(
                            "Result could not be downloaded - to view type `frecli cas download-blob {}`",
                            FileDigest::from_re(digest),
                        )
                    }
                }
            }
            Self::PrefetchedLossy { data, .. } => data.clone(),
            Self::Digest(digest) => {
                format!(
                    "Result too large to display - to view type `frecli cas download-blob {}`",
                    FileDigest::from_re(digest),
                )
            }
            Self::None => String::new(),
        }
    }

    async fn into_bytes(
        self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<Vec<u8>> {
        match self {
            Self::Raw(raw) => Ok(raw),
            Self::Digest(digest) | Self::PrefetchedLossy { digest, .. } => {
                let bytes = client
                    .download_blob(&digest, use_case)
                    .await
                    .with_context(|| {
                        format!("Error downloading from {}", FileDigest::from_re(&digest))
                    })?;
                Ok(bytes)
            }
            Self::None => Ok(Vec::new()),
        }
    }

    /// Prefetch the output, if relevant.
    async fn prefetch_lossy(
        &mut self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
    ) {
        if let Self::Digest(digest) = &self {
            let data = self.to_lossy(client, use_case).await;
            *self = Self::PrefetchedLossy {
                data,
                digest: digest.clone(),
            };
        }
    }
}

#[async_trait]
pub(crate) trait CommandStdStreamsExt {
    async fn into_re(
        self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<StdStreamPair<ReStdStream>>;
}

#[async_trait]
impl CommandStdStreamsExt for CommandStdStreams {
    async fn into_re(
        self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<StdStreamPair<ReStdStream>> {
        match self {
            Self::Local { stdout, stderr } => {
                let (stdout, stderr) = future::try_join(
                    maybe_upload_to_re(client, use_case, stdout),
                    maybe_upload_to_re(client, use_case, stderr),
                )
                .await?;

                Ok(StdStreamPair { stdout, stderr })
            }
            Self::Remote(remote) => {
                // TODO (torozco): This assumes that the existing remote outputs we have have the
                // same re use case as what we passed in. Lots of things make this assumption, but
                // for the sake of being safe, check it.
                if remote.use_case() != use_case {
                    return Err(anyhow::anyhow!(
                        "Copying log outputs across RE use cases (from `{}` to `{}`) is not supported",
                        remote.use_case(),
                        use_case
                    ));
                }

                let (stdout, stderr) = remote.into_stdout_stderr();
                Ok(StdStreamPair { stdout, stderr })
            }
            Self::Empty => Ok(StdStreamPair {
                stdout: ReStdStream::None,
                stderr: ReStdStream::None,
            }),
        }
    }
}

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
}

#[async_trait]
impl RemoteCommandStdStreamsDyn for RemoteCommandStdStreams {
    fn clone(&self) -> Box<dyn RemoteCommandStdStreamsDyn> {
        box Clone::clone(self)
    }

    async fn to_lossy_stdout(&self) -> String {
        self.stdout.to_lossy(&self.client, self.use_case).await
    }

    async fn to_lossy_stderr(&self) -> String {
        self.stderr.to_lossy(&self.client, self.use_case).await
    }

    async fn into_stdout_stderr_bytes(self: Box<Self>) -> anyhow::Result<(Vec<u8>, Vec<u8>)> {
        future::try_join(
            self.stdout.into_bytes(&self.client, self.use_case),
            self.stderr.into_bytes(&self.client, self.use_case),
        )
        .await
    }

    fn use_case(&self) -> RemoteExecutorUseCase {
        self.use_case
    }

    fn into_stdout_stderr(self: Box<Self>) -> (ReStdStream, ReStdStream) {
        (self.stdout, self.stderr)
    }
}

async fn maybe_upload_to_re(
    client: &ManagedRemoteExecutionClient,
    use_case: RemoteExecutorUseCase,
    bytes: Vec<u8>,
) -> anyhow::Result<ReStdStream> {
    const MIN_STREAM_UPLOAD_SIZE: usize = 50 * 1024; // Same as RE
    if bytes.len() < MIN_STREAM_UPLOAD_SIZE {
        return Ok(ReStdStream::Raw(bytes));
    }
    let digest = client.upload_blob(bytes, use_case).await?;
    Ok(ReStdStream::Digest(digest))
}
