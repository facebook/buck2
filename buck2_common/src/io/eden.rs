/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Eden's Thrift API does sometime want &Vec<...>.
#![allow(clippy::useless_vec)]

use std::fmt::Display;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::anyhow as fs;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::result::SharedResult;
use buck2_core::{self};
use derivative::Derivative;
use edenfs::client::EdenService;
use edenfs::errors::eden_service::ListMountsError;
use edenfs::types::Dtype;
use edenfs::types::EdenErrorType;
use edenfs::types::FileAttributeDataOrError;
use edenfs::types::FileAttributes;
use edenfs::types::FileInformationOrError;
use edenfs::types::GetAttributesFromFilesParams;
use edenfs::types::GlobParams;
use edenfs::types::MountState;
use edenfs::types::SyncBehavior;
use edenfs::types::SynchronizeWorkingCopyParams;
use fbinit::FacebookInit;
use futures::future::BoxFuture;
use futures::future::Future;
use futures::future::FutureExt;
use futures::future::Shared;
use gazebo::cmp::PartialEqAny;
use gazebo::prelude::*;
use parking_lot::Mutex;
use thiserror::Error;
use tokio::sync::Semaphore;

use crate::file_ops::FileDigest;
use crate::file_ops::FileMetadata;
use crate::file_ops::FileType;
use crate::file_ops::PathMetadata;
use crate::file_ops::SimpleDirEntry;
use crate::file_ops::TrackedFileDigest;
use crate::io::fs::FsIoProvider;
use crate::io::IoProvider;

#[derive(Debug, Error)]
#[error("Eden returned an error: {}", .0.message)]
struct EdenError(edenfs::types::EdenError);

#[derive(Debug, Error)]
#[error("Eden returned an unexpected field: {}", .0)]
struct UnknownField(i32);

#[derive(Derivative)]
#[derivative(PartialEq)]
pub struct EdenIoProvider {
    #[derivative(PartialEq = "ignore")]
    connector: EdenConnector,
    #[derivative(PartialEq = "ignore")]
    connection: Mutex<EdenConnection>,
    /// Eden has limits on concurrency and will return server overloaded (or timeout) errors if we
    /// send too many. Experimentally, even for large builds (see details in D36136516), we don't
    /// much performance improvement beyond 2K concurrent requests, regardless of whether Eden has
    /// a fast or slow connectin to source control, a warm cache or not, and a lot of CPU available
    /// to run or not.
    #[derivative(PartialEq = "ignore")]
    semaphore: Semaphore,
    fs: FsIoProvider,
}

impl EdenIoProvider {
    pub fn new(fb: FacebookInit, fs: &Arc<ProjectFilesystem>) -> anyhow::Result<Option<Self>> {
        if cfg!(not(fbcode_build)) {
            tracing::warn!("Cargo build detected: disabling Eden I/O");
            return Ok(None);
        }

        let eden_root = fs.root.join(".eden");
        if !eden_root.exists() {
            return Ok(None);
        }

        let root = fs::read_link(eden_root.join("root"))?
            .to_str()
            .context("Eden root is not UTF-8")?
            .to_owned();
        let root = Arc::new(root);

        let socket = fs::read_link(eden_root.join("socket"))?;

        let connector = EdenConnector { fb, root, socket };

        let connection = Mutex::new(EdenConnection {
            epoch: 0,
            client: connector.connect(),
        });

        static EDEN_SEMAPHORE: EnvHelper<usize> = EnvHelper::new("BUCK2_EDEN_SEMAPHORE");
        let eden_semaphore = EDEN_SEMAPHORE.get()?.unwrap_or(2048);

        Ok(Some(Self {
            connector,
            connection,
            semaphore: Semaphore::new(eden_semaphore),
            fs: FsIoProvider::new(fs.dupe()),
        }))
    }

    async fn with_eden<F, Fut, T, E>(&self, f: F) -> Result<T, ConnectAndRequestError<E>>
    where
        F: Fn(&(dyn EdenService + Send + Sync)) -> Fut + 'static,
        Fut: Future<Output = Result<T, E>>,
        E: HasErrorHandlingStrategy + Display,
    {
        const MAX_ATTEMPTS: usize = 3;

        let mut connection = (*self.connection.lock()).clone();
        let mut attempts = 0;

        let _permit = self
            .semaphore
            .acquire()
            .await
            .expect("Eden I/O semaphore is never closed");

        loop {
            attempts += 1;

            let res = async {
                let client = connection
                    .client
                    .clone()
                    .await
                    .map_err(|e| ConnectAndRequestError::ConnectionError(e.into()))?;

                f(client.as_ref())
                    .await
                    .map_err(|e| ConnectAndRequestError::RequestError(e))
            }
            .await;

            let err = match res {
                Ok(res) => break Ok(res),
                Err(e) => e,
            };

            match err.error_handling_strategy() {
                ErrorHandlingStrategy::Reconnect => {
                    // Our connection to Eden broke. This typically means Eden restarted. Just
                    // reconnect.
                    tracing::info!("Reconnecting to Eden after: {:#}", err);
                    let mut guard = self.connection.lock();
                    if guard.epoch == connection.epoch {
                        guard.client = self.connector.connect();
                        guard.epoch += 1;
                    }
                    connection = (*guard).clone();
                }
                ErrorHandlingStrategy::Retry => {
                    // Our request failed but needs retrying.
                    tracing::info!("Retrying Eden request after: {:#}", err);
                }
                ErrorHandlingStrategy::Abort => {
                    break Err(err);
                }
            };

            if attempts > MAX_ATTEMPTS {
                break Err(err);
            }
        }
    }
}

#[async_trait]
impl IoProvider for EdenIoProvider {
    async fn read_file(&self, path: ProjectRelativePathBuf) -> anyhow::Result<String> {
        self.fs.read_file(path).await
    }

    async fn read_dir(&self, path: ProjectRelativePathBuf) -> anyhow::Result<Vec<SimpleDirEntry>> {
        let params = GlobParams {
            mountPoint: self.connector.root.as_bytes().to_vec(),
            globs: vec![format!("{}/*", path)],
            includeDotfiles: true,
            wantDtype: true,
            sync: no_sync(),
            ..Default::default()
        };

        let globbed = self.with_eden(move |eden| eden.globFiles(&params)).await?;

        tracing::trace!(
            "globFiles({}/*): {} files",
            path,
            globbed.matchingFiles.len()
        );

        let ret = globbed
            .matchingFiles
            .iter()
            .enumerate()
            .map(|(idx, file_path)| {
                let file_path = std::str::from_utf8(file_path)
                    .map_err(|_| anyhow::anyhow!("Invalid filename: {:?}", file_path))?;

                let file_path = ProjectRelativePath::new(file_path).context("Invalid file path")?;
                let file_name = file_path
                    .file_name()
                    .context("Invalid file path")?
                    .to_owned();

                let dtype = globbed
                    .dtypes
                    .get(idx)
                    .context("Invalid response from Eden")?;

                let dtype = Dtype::from(i32::from(*dtype));

                let file_type = match dtype {
                    Dtype::DIR => FileType::Directory,
                    Dtype::REGULAR => FileType::File,
                    Dtype::LINK => FileType::Symlink,
                    _ => FileType::Unknown,
                };

                anyhow::Ok(SimpleDirEntry {
                    file_type,
                    file_name,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(ret)
    }

    async fn read_path_metadata_if_exists(
        &self,
        cell_root: ProjectRelativePathBuf,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<Option<PathMetadata>> {
        let requested_attributes =
            i64::from(i32::from(FileAttributes::SHA1_HASH) | i32::from(FileAttributes::FILE_SIZE));

        let params = GetAttributesFromFilesParams {
            mountPoint: self.connector.root.as_bytes().to_vec(),
            paths: vec![path.to_string().into_bytes()],
            requestedAttributes: requested_attributes,
            sync: no_sync(),
            ..Default::default()
        };

        let attrs = self
            .with_eden(move |eden| eden.getAttributesFromFiles(&params))
            .await?;

        match attrs
            .res
            .into_iter()
            .next()
            .context("Eden did not return file info")?
        {
            FileAttributeDataOrError::data(data) => {
                tracing::trace!("getAttributesFromFiles({}): ok", path,);
                let digest = FileDigest {
                    sha1: data
                        .sha1
                        .context("Eden did not return a sha1")?
                        .try_into()
                        .ok()
                        .context("Eden returned an invalid sha1")?,
                    size: data
                        .fileSize
                        .context("Eden did not return a fileSize")?
                        .try_into()
                        .context("Eden returned an invalid fileSize")?,
                };

                let digest = TrackedFileDigest::new(digest);

                let is_executable = fetch_is_executable(self, &self.connector.root, &path).await?;

                let meta = FileMetadata {
                    digest,
                    is_executable,
                };

                Ok(Some(PathMetadata::File(meta)))
            }
            FileAttributeDataOrError::error(e) => {
                tracing::trace!("getAttributesFromFiles({}): {} ()", e.errorType, e.message);

                match e.errorType {
                    EdenErrorType::POSIX_ERROR => {
                        match e.errorCode.map(nix::errno::Errno::from_i32) {
                            Some(nix::errno::Errno::EISDIR) => {
                                return Ok(Some(PathMetadata::Directory));
                            }
                            Some(nix::errno::Errno::ENOENT) => {
                                return Ok(None);
                            }
                            // If we get EINVAL it means the target wasn't a file, and since we know it
                            // existed and it wasn't a dir, then that means it must be a symlink. If we get
                            // ENOTDIR, that means we tried to traverse a path component that was a
                            // symlink. In both cases, we need to both a) handle ExternalSymlink and b)
                            // look through to the target, so we do that.
                            // TODO: It would be better to read the link then ask Eden for the SHA1.
                            Some(nix::errno::Errno::EINVAL) | Some(nix::errno::Errno::ENOTDIR) => {
                                self.fs.read_path_metadata_if_exists(cell_root, path).await
                            }
                            _ => Err(EdenError(e).into()),
                        }
                    }
                    _ => Err(EdenError(e).into()),
                }
            }
            FileAttributeDataOrError::UnknownField(f) => Err(UnknownField(f).into()),
        }
    }

    async fn settle(&self) -> anyhow::Result<()> {
        let root = (*self.connector.root).clone().into_bytes();

        self.with_eden(move |eden| {
            eden.synchronizeWorkingCopy(
                &root,
                &SynchronizeWorkingCopyParams {
                    sync: SyncBehavior {
                        syncTimeoutSeconds: None,
                        ..Default::default()
                    },
                    ..Default::default()
                },
            )
        })
        .await
        .context("Error synchronizing Eden working copy")
    }

    fn name(&self) -> &'static str {
        "eden"
    }

    fn eq_token(&self) -> PartialEqAny<'_> {
        PartialEqAny::new(self)
    }

    fn fs(&self) -> &Arc<ProjectFilesystem> {
        self.fs.fs()
    }
}

/// This is a hack. For the time being, Eden does no allow us to get file modes via FileAttributes
/// so we have to get it separately (T117809710 tracks adding this to FileAttributes in Eden).
async fn fetch_is_executable(
    provider: &EdenIoProvider,
    root: &str,
    path: &ProjectRelativePathBuf,
) -> anyhow::Result<bool> {
    let root = root.as_bytes().to_vec();
    let paths = vec![path.to_string().into_bytes()];

    let attrs = provider
        .with_eden(move |eden| eden.getFileInformation(&root, &paths, &no_sync()))
        .await?;

    let attr = attrs.into_iter().next().context("No attrs")?;

    match attr {
        FileInformationOrError::info(info) => {
            let mode = nix::sys::stat::Mode::from_bits_truncate(
                info.mode.try_into().context("invalid mode")?,
            );
            Ok(mode.intersects(nix::sys::stat::Mode::S_IXUSR))
        }
        FileInformationOrError::error(e) => Err(EdenError(e).into()),
        FileInformationOrError::UnknownField(f) => Err(UnknownField(f).into()),
    }
}

/// We don't request sync on individual calls because we make a dedicated call for this before we
/// start an operation.
fn no_sync() -> SyncBehavior {
    // NOTE/ we'd rather make this a `const` if we could, but we can't, because
    // `Default::default()` is not a const-fn.
    SyncBehavior {
        syncTimeoutSeconds: Some(0),
        ..Default::default()
    }
}

/// A (potentially pending) Eden client.
type EdenClientFuture =
    Shared<BoxFuture<'static, SharedResult<Arc<dyn EdenService + Send + Sync>>>>;

/// An Eden client and an epoch to keep track of reconnections.
#[derive(Clone)]
struct EdenConnection {
    /// This stats at zero and increments every time we reconnect. We use this to keep track of
    /// whether another client already recycled the connection when we need to reconnect.
    epoch: usize,
    client: EdenClientFuture,
}

/// A factory for Eden clients.
struct EdenConnector {
    fb: FacebookInit,
    root: Arc<String>,
    socket: PathBuf,
}

impl EdenConnector {
    fn connect(&self) -> EdenClientFuture {
        let socket = self.socket.clone();
        let fb = self.fb;
        let root = self.root.dupe();

        tokio::task::spawn(async move {
            tracing::info!("Creating a new Eden connection via `{}`", socket.display());
            let eden: anyhow::Result<Arc<dyn EdenService + Send + Sync>>;

            #[cfg(fbcode_build)]
            {
                // NOTE: This timeout is absurdly high, but bear in mind that what we're
                // "comparing" to is a FS call that has no timeouts at all.
                const EDEN_THRIFT_TIMEOUT_MS: u32 = 120_000;

                eden = ::thriftclient::ThriftChannelBuilder::from_path(fb, socket)?
                    .with_conn_timeout(EDEN_THRIFT_TIMEOUT_MS)
                    .with_recv_timeout(EDEN_THRIFT_TIMEOUT_MS)
                    .with_secure(false)
                    .build_client(::edenfs::client::make_EdenService);
            }

            #[cfg(not(fbcode_build))]
            {
                let _ignored = fb;
                let _ignored = socket;
                eden = Err(anyhow::anyhow!("Eden I/O is not available in Cargo builds"))
            }

            let eden = eden.context("Error constructing Eden client")?;

            wait_until_mount_is_ready(eden.as_ref(), &root).await?;

            Ok(eden)
        })
        .map(|r| match r {
            Ok(r) => r,
            Err(e) => Err(e.into()), // Turn the JoinError into a SharedError.
        })
        .boxed()
        .shared()
    }
}

#[derive(Error, Debug)]
#[error("Mount never became ready: `{}`", self.mount)]
struct MountNeverBecameReady {
    mount: Arc<String>,
}

/// Delay until a mount becomes ready (up to 10 seconds).
async fn wait_until_mount_is_ready(
    eden: &(dyn EdenService + Send + Sync),
    root: &Arc<String>,
) -> anyhow::Result<()> {
    let mut interval = tokio::time::interval(Duration::from_secs(1));
    interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);

    for _ in 0..10 {
        interval.tick().await;
        match is_mount_ready(eden, root).await {
            Ok(true) => return Ok(()),
            Ok(false) => {
                // Fallthrough to keep going
            }
            Err(e) if e.error_handling_strategy() == ErrorHandlingStrategy::Retry => {
                // Fallthrough to keep going
            }
            Err(e) => return Err(e.into()),
        }
    }

    Err(MountNeverBecameReady { mount: root.dupe() }.into())
}

#[derive(Error, Debug)]
pub enum IsMountReadyError {
    #[error("Mount does not exist in Eden: `{}`", .mount)]
    MountDoesNotExist { mount: Arc<String> },
    #[error(transparent)]
    RequestError(ListMountsError),
}

/// Check if a given mount is ready.
async fn is_mount_ready(
    eden: &(dyn EdenService + Send + Sync),
    root: &Arc<String>,
) -> Result<bool, IsMountReadyError> {
    let mounts = eden
        .listMounts()
        .await
        .map_err(IsMountReadyError::RequestError)?;

    for mount in mounts {
        if mount.mountPoint == root.as_bytes() {
            return Ok(mount.state == MountState::RUNNING);
        }
    }

    Err(IsMountReadyError::MountDoesNotExist { mount: root.dupe() })
}

#[derive(Error, Debug)]
enum ConnectAndRequestError<E> {
    #[error(transparent)]
    ConnectionError(anyhow::Error),

    #[error(transparent)]
    RequestError(E),
}

#[derive(Copy, Clone, Dupe, PartialEq, Eq)]
enum ErrorHandlingStrategy {
    Reconnect,
    Retry,
    Abort,
}

trait HasErrorHandlingStrategy {
    fn error_handling_strategy(&self) -> ErrorHandlingStrategy;
}

impl<E: HasErrorHandlingStrategy> HasErrorHandlingStrategy for ConnectAndRequestError<E> {
    fn error_handling_strategy(&self) -> ErrorHandlingStrategy {
        match self {
            Self::ConnectionError(..) => ErrorHandlingStrategy::Reconnect,
            Self::RequestError(e) => e.error_handling_strategy(),
        }
    }
}

impl HasErrorHandlingStrategy for IsMountReadyError {
    fn error_handling_strategy(&self) -> ErrorHandlingStrategy {
        match self {
            Self::MountDoesNotExist { .. } => ErrorHandlingStrategy::Abort,
            Self::RequestError(e) => e.error_handling_strategy(),
        }
    }
}

macro_rules! impl_has_error_disposition {
    ($err: ident) => {
        impl HasErrorHandlingStrategy for ::edenfs::errors::eden_service::$err {
            fn error_handling_strategy(&self) -> ErrorHandlingStrategy {
                match self {
                    Self::ThriftError(..) => ErrorHandlingStrategy::Reconnect,
                    Self::ApplicationException(..) => ErrorHandlingStrategy::Retry,
                    Self::ex(..) => ErrorHandlingStrategy::Abort,
                }
            }
        }
    };
}

impl_has_error_disposition!(GetAttributesFromFilesError);
impl_has_error_disposition!(GetFileInformationError);
impl_has_error_disposition!(GlobFilesError);
impl_has_error_disposition!(ListMountsError);
impl_has_error_disposition!(SynchronizeWorkingCopyError);
