use std::fmt;
use std::fmt::Debug;

use anyhow::Context;
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_common::file_ops::FileDigest;
use futures::future;

use crate::digest::CasDigestFromReExt;
use crate::digest::ReDigest;
use crate::re::manager::ManagedRemoteExecutionClient;
use crate::re::streams::RemoteCommandStdStreams;

/// A pair of streams.
#[allow(clippy::manual_non_exhaustive)]
#[derive(Debug)]
pub struct StdStreamPair<T> {
    pub stdout: T,
    pub stderr: T,
}

#[derive(Clone)]
pub enum ReStdStream {
    /// Raw bytes received inline from RE.
    Raw(Vec<u8>),

    /// Output not available inline, we have a digest.
    Digest(ReDigest),

    /// This output was not available inline, but was prefetched. The prefetch might be lossy. We
    /// have a digest to access the full data if needed.
    PrefetchedLossy { data: String, digest: ReDigest },

    /// There was no output made available by RE.
    None,
}

impl ReStdStream {
    pub fn new(raw: Option<Vec<u8>>, digest: Option<ReDigest>) -> Self {
        match (raw, digest) {
            (Some(raw), _) if !raw.is_empty() => Self::Raw(raw),
            (_, Some(digest)) => Self::Digest(digest),
            (_, None) => Self::None,
        }
    }

    pub fn into_raw_or_digest(self) -> (Option<Vec<u8>>, Option<ReDigest>) {
        match self {
            Self::Raw(raw) => (Some(raw), None),
            Self::Digest(digest) | Self::PrefetchedLossy { digest, .. } => (None, Some(digest)),
            Self::None => (None, None),
        }
    }

    pub(crate) async fn to_lossy(
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

    pub(crate) async fn into_bytes(
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
    pub(crate) async fn prefetch_lossy(
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

impl fmt::Display for ReStdStream {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Raw(raw) => {
                write!(fmt, "raw = `{}`", String::from_utf8_lossy(raw))?;
            }
            Self::Digest(digest) | Self::PrefetchedLossy { digest, .. } => {
                write!(fmt, "digest = `{}`", FileDigest::from_re(digest))?;
            }
            Self::None => {
                write!(fmt, "none")?;
            }
        }

        Ok(())
    }
}

impl fmt::Debug for ReStdStream {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "ReStdStream({})", self)
    }
}

#[derive(Debug, derive_more::From, Clone)]
pub enum CommandStdStreams {
    Local { stdout: Vec<u8>, stderr: Vec<u8> },

    Remote(RemoteCommandStdStreams),

    Empty,
}

impl Default for CommandStdStreams {
    fn default() -> Self {
        Self::Empty
    }
}

impl CommandStdStreams {
    /// Access this data as lossy stdout / stderr. This is designed for human consumption.
    pub async fn to_lossy(&self) -> StdStreamPair<String> {
        match self {
            Self::Local { stdout, stderr } => StdStreamPair {
                stdout: String::from_utf8_lossy(stdout).into_owned(),
                stderr: String::from_utf8_lossy(stderr).into_owned(),
            },
            Self::Remote(remote) => {
                let (stdout, stderr) =
                    future::join(remote.to_lossy_stdout(), remote.to_lossy_stderr()).await;
                StdStreamPair { stdout, stderr }
            }
            Self::Empty => StdStreamPair {
                stdout: String::new(),
                stderr: String::new(),
            },
        }
    }

    /// Access this data lossily, but only retain stderr.
    pub async fn to_lossy_stderr(&self) -> String {
        match self {
            Self::Local { stderr, .. } => String::from_utf8_lossy(stderr).into_owned(),
            Self::Remote(remote) => remote.to_lossy_stderr().await,
            Self::Empty => String::new(),
        }
    }

    /// Access the raw data. This is suitable for machine consumption. This will fail if we can't
    /// fetch it.
    pub async fn into_bytes(self) -> anyhow::Result<StdStreamPair<Vec<u8>>> {
        match self {
            Self::Local { stdout, stderr } => Ok(StdStreamPair { stdout, stderr }),
            Self::Remote(remote) => {
                let (stdout, stderr) = remote.into_stdout_stderr_bytes().await?;
                Ok(StdStreamPair { stdout, stderr })
            }
            Self::Empty => Ok(StdStreamPair {
                stdout: Vec::new(),
                stderr: Vec::new(),
            }),
        }
    }

    pub async fn into_re(
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
