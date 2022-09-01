use async_trait::async_trait;
use buck2_execute::execute::output::CommandStdStreams;
use buck2_execute::execute::output::ReStdStream;
use buck2_execute::execute::output::StdStreamPair;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use buck2_node::execute::config::RemoteExecutorUseCase;
use futures::future;

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
