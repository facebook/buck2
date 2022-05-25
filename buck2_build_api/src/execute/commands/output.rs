use std::fmt;

use anyhow::Context as _;
use buck2_common::file_ops::FileDigest;
use derivative::Derivative;
use derive_more::From;
use futures::future;
use gazebo::prelude::*;
use remote_execution::TActionResult2;

use crate::{
    actions::digest::{FileDigestFromReExt, ReDigest},
    execute::commands::re::manager::ManagedRemoteExecutionClient,
};

/// A pair of streams.
#[allow(clippy::manual_non_exhaustive)]
#[derive(Debug)]
pub struct StdStreamPair<T> {
    pub stdout: T,
    pub stderr: T,
    _private: (),
}

#[derive(Debug, From)]
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
                _private: (),
            },
            Self::Remote(remote) => {
                let (stdout, stderr) = future::join(
                    remote.stdout.to_lossy(&remote.client),
                    remote.stderr.to_lossy(&remote.client),
                )
                .await;
                StdStreamPair {
                    stdout,
                    stderr,
                    _private: (),
                }
            }
            Self::Empty => StdStreamPair {
                stdout: String::new(),
                stderr: String::new(),
                _private: (),
            },
        }
    }

    /// Access this data lossily, but only retain stderr.
    pub async fn to_lossy_stderr(&self) -> String {
        match self {
            Self::Local { stderr, .. } => String::from_utf8_lossy(stderr).into_owned(),
            Self::Remote(remote) => remote.stderr.to_lossy(&remote.client).await,
            Self::Empty => String::new(),
        }
    }

    /// Access the raw data. This is suitable for machine consumption. This will fail if we can't
    /// fetch it.
    pub async fn into_bytes(self) -> anyhow::Result<StdStreamPair<Vec<u8>>> {
        match self {
            Self::Local { stdout, stderr } => Ok(StdStreamPair {
                stdout,
                stderr,
                _private: (),
            }),
            Self::Remote(remote) => {
                let (stdout, stderr) = future::try_join(
                    remote.stdout.into_bytes(&remote.client),
                    remote.stderr.into_bytes(&remote.client),
                )
                .await?;
                Ok(StdStreamPair {
                    stdout,
                    stderr,
                    _private: (),
                })
            }
            Self::Empty => Ok(StdStreamPair {
                stdout: Vec::new(),
                stderr: Vec::new(),
                _private: (),
            }),
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct RemoteCommandStdStreams {
    #[derivative(Debug = "ignore")]
    client: ManagedRemoteExecutionClient,
    stdout: ReStdStream,
    stderr: ReStdStream,
}

impl RemoteCommandStdStreams {
    pub fn new(action_result: &TActionResult2, client: &ManagedRemoteExecutionClient) -> Self {
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
            stdout,
            stderr,
        }
    }

    pub async fn prefetch_lossy_stderr(mut self) -> Self {
        self.stderr.prefetch_lossy(&self.client).await;
        self
    }
}

enum ReStdStream {
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
    fn new(raw: Option<Vec<u8>>, digest: Option<ReDigest>) -> Self {
        match (raw, digest) {
            (Some(raw), _) if !raw.is_empty() => Self::Raw(raw),
            (_, Some(digest)) => Self::Digest(digest),
            (_, None) => Self::None,
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

impl ReStdStream {
    async fn to_lossy(&self, client: &ManagedRemoteExecutionClient) -> String {
        // 4MBs seems like a reasonably large volume of output. There is no research or science behind
        // this number.
        const MAX_STREAM_DOWNLOAD_SIZE: i64 = 4 * 1024 * 1024;

        match self {
            Self::Raw(raw) => String::from_utf8_lossy(raw).into_owned(),
            Self::Digest(digest) if digest.size_in_bytes <= MAX_STREAM_DOWNLOAD_SIZE => {
                match client.download_blob(digest, Default::default()).await {
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

    async fn into_bytes(self, client: &ManagedRemoteExecutionClient) -> anyhow::Result<Vec<u8>> {
        match self {
            Self::Raw(raw) => Ok(raw),
            Self::Digest(digest) | Self::PrefetchedLossy { digest, .. } => {
                let bytes = client
                    .download_blob(&digest, Default::default())
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
    async fn prefetch_lossy(&mut self, client: &ManagedRemoteExecutionClient) {
        if let Self::Digest(digest) = &self {
            let data = self.to_lossy(client).await;
            *self = Self::PrefetchedLossy {
                data,
                digest: digest.clone(),
            };
        }
    }
}
