use std::fmt;
use std::fmt::Debug;

use async_trait::async_trait;
use buck2_common::file_ops::FileDigest;
use buck2_node::execute::config::RemoteExecutorUseCase;
use futures::future;

use crate::digest::FileDigestFromReExt;
use crate::digest::ReDigest;

/// A pair of streams.
#[allow(clippy::manual_non_exhaustive)]
#[derive(Debug)]
pub struct StdStreamPair<T> {
    pub stdout: T,
    pub stderr: T,
}

#[async_trait]
pub trait RemoteCommandStdStreamsDyn: Debug + Send + Sync + 'static {
    fn clone(&self) -> Box<dyn RemoteCommandStdStreamsDyn>;
    async fn to_lossy_stdout(&self) -> String;
    async fn to_lossy_stderr(&self) -> String;
    async fn into_stdout_stderr_bytes(self: Box<Self>) -> anyhow::Result<(Vec<u8>, Vec<u8>)>;
    fn use_case(&self) -> RemoteExecutorUseCase;
    fn into_stdout_stderr(self: Box<Self>) -> (ReStdStream, ReStdStream);
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

#[derive(Debug, derive_more::From)]
pub enum CommandStdStreams {
    Local { stdout: Vec<u8>, stderr: Vec<u8> },

    Remote(Box<dyn RemoteCommandStdStreamsDyn>),

    Empty,
}

impl Clone for CommandStdStreams {
    fn clone(&self) -> Self {
        match self {
            CommandStdStreams::Local { stdout, stderr } => CommandStdStreams::Local {
                stdout: stdout.clone(),
                stderr: stderr.clone(),
            },
            CommandStdStreams::Remote(remote) => {
                CommandStdStreams::Remote(RemoteCommandStdStreamsDyn::clone(&**remote))
            }
            CommandStdStreams::Empty => CommandStdStreams::Empty,
        }
    }
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
}
