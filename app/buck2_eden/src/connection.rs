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

use std::collections::BTreeMap;
use std::fmt::Display;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use allocative::Allocative;
use buck2_certs::validate::validate_certs;
use buck2_core;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::soft_error;
use buck2_error::buck2_error;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use dupe::Dupe;
use edenfs::BinaryHash;
use edenfs::EdenErrorType;
use edenfs::FileAttributeData;
use edenfs::FileAttributeDataOrErrorV2;
use edenfs::FileAttributeDataV2;
use edenfs::MountState;
use edenfs::PathString;
use edenfs::SourceControlType;
use edenfs_clients::errors::ListMountsError;
use edenfs_clients::EdenService;
use fb303_core_clients::BaseService;
use fbinit::FacebookInit;
use futures::future::BoxFuture;
use futures::future::Future;
use futures::future::FutureExt;
use futures::future::Shared;
use parking_lot::Mutex;
use serde::Deserialize;
use sorted_vector_map::SortedVectorMap;
use tokio::sync::Semaphore;

use crate::semaphore;

#[derive(Allocative)]
pub struct EdenConnectionManager {
    connector: EdenConnector,
    connection: Mutex<EdenConnection>,
    /// Eden has limits on concurrency and will return server overloaded (or timeout) errors if we
    /// send too many. Experimentally, even for large builds (see details in D36136516), we don't
    /// get much performance improvement beyond 2K concurrent requests, regardless of whether Eden
    /// has a fast or slow connection to source control, a warm cache or not, and a lot of CPU
    /// available to run or not.
    #[allocative(skip)]
    semaphore: Semaphore,
    /// The project root, relative to the eden mount point
    project_root: ForwardRelativePathBuf,
}

#[derive(Deserialize, Debug)]
struct Config {
    root: String,
    socket: String,
}

#[derive(Deserialize, Debug)]
struct EdenConfig {
    #[serde(rename = "Config")]
    config: Config,
}

#[derive(Allocative)]
struct EdenMountPoint(AbsPathBuf);

impl EdenConnectionManager {
    pub fn new(
        fb: FacebookInit,
        project_root: &ProjectRoot,
        semaphore: Option<Semaphore>,
    ) -> buck2_error::Result<Option<Self>> {
        let dot_eden_dir = project_root.root().as_abs_path().join(".eden");
        if !dot_eden_dir.exists() {
            return Ok(None);
        }
        let connector = Self::get_eden_connector(fb, &dot_eden_dir)?;

        let canon_project_root = fs_util::canonicalize(project_root.root())?;
        let canon_eden_mount = fs_util::canonicalize(&connector.mount.0)?;

        let rel_project_root = canon_project_root
            .strip_prefix(&canon_eden_mount)
            .with_buck_error_context(|| {
                format!(
                    "Eden root {} was not a prefix of the project root {}",
                    canon_eden_mount, canon_project_root
                )
            })?;

        let connection = Mutex::new(EdenConnection {
            epoch: 0,
            client: connector.connect(),
        });

        let semaphore = semaphore.unwrap_or(semaphore::default());

        Ok(Some(Self {
            connector,
            connection,
            semaphore,
            project_root: rel_project_root.into_owned(),
        }))
    }

    fn get_eden_connector(
        fb: FacebookInit,
        dot_eden_dir: &AbsPath,
    ) -> buck2_error::Result<EdenConnector> {
        // Based off of how watchman picks up the config: fbcode/watchman/watcher/eden.cpp:138
        if cfg!(windows) {
            let config_path = dot_eden_dir.join("config");
            let config_contents = fs_util::read_to_string(config_path)?;
            let config: EdenConfig = toml::from_str(&config_contents)
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
            let mount = Arc::new(EdenMountPoint(AbsPathBuf::new(config.config.root)?));
            let socket = AbsPathBuf::new(PathBuf::from(config.config.socket))?;
            Ok(EdenConnector { fb, mount, socket })
        } else {
            let mount = fs_util::read_link(dot_eden_dir.join("root"))?;
            let mount = Arc::new(EdenMountPoint(AbsPathBuf::new(mount)?));
            let socket = AbsPathBuf::new(fs_util::read_link(dot_eden_dir.join("socket"))?)?;
            Ok(EdenConnector { fb, mount, socket })
        }
    }

    pub fn get_mount_point(&self) -> Vec<u8> {
        self.connector
            .mount
            .0
            .as_path()
            .as_os_str()
            .as_encoded_bytes()
            .to_vec()
    }

    /// Converts project relative paths to values that are suitable for passing to Eden requests
    pub fn project_paths_as_eden_paths<'a>(
        &self,
        paths: impl IntoIterator<Item = &'a ProjectRelativePath>,
    ) -> Vec<Vec<u8>> {
        paths
            .into_iter()
            .map(|p| self.project_root.join(p).to_string().into_bytes())
            .collect()
    }

    /// Returns a string like "20220102-030405", assuming this is a release version. This is
    /// pattern-matched off of what the Eden CLI does.
    pub async fn get_eden_version(&self) -> buck2_error::Result<Option<String>> {
        let fb303 = self.connector.connect_fb303()?;
        let values = fb303
            .getRegexExportedValues("^build_.*")
            .await
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;

        fn join_version(values: &BTreeMap<String, String>) -> Option<String> {
            let version = values.get("build_package_version")?;
            let release = values.get("build_package_release")?;
            if version.is_empty() || release.is_empty() {
                return None;
            }
            Some(format!("{}-{}", version, release))
        }

        Ok(join_version(&values))
    }

    pub async fn with_eden<F, Fut, T, E>(&self, f: F) -> Result<T, ConnectAndRequestError<E>>
    where
        F: Fn(&(dyn EdenService + Send + Sync)) -> Fut,
        Fut: Future<Output = Result<T, E>>,
        E: HasErrorHandlingStrategy + Display,
    {
        const MAX_ATTEMPTS: usize = 3;

        let mut connection = (*self.connection.lock()).clone();
        let mut attempts = 0;
        let mut retries = 0;

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
                Ok(res) => {
                    // Attempts may be > 1 if we had to reconnect. We only want to log a soft error
                    // on retry. Solely for logging purposes, don't panic if value wasn't "thrown"
                    if retries > 0 {
                        soft_error!(
                            "eden_io_succeeded_after_retry",
                            buck2_error!(buck2_error::ErrorTag::Input, "Eden IO retried {} times", retries),
                            quiet: true
                        ).ok();
                    }
                    break Ok(res);
                }
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
                    retries += 1;
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

/// A (potentially pending) Eden client.
type EdenClientFuture =
    Shared<BoxFuture<'static, buck2_error::Result<Arc<dyn EdenService + Send + Sync>>>>;

/// An Eden client and an epoch to keep track of reconnections.
#[derive(Clone, Allocative)]
struct EdenConnection {
    /// This starts at zero and increments every time we reconnect. We use this to keep track of
    /// whether another client already recycled the connection when we need to reconnect.
    epoch: usize,
    #[allocative(skip)]
    client: EdenClientFuture,
}

/// A factory for Eden clients.
#[derive(Allocative)]
struct EdenConnector {
    #[allocative(skip)]
    fb: FacebookInit,
    mount: Arc<EdenMountPoint>,
    socket: AbsPathBuf,
}

fn thrift_builder(
    fb: FacebookInit,
    socket: &AbsPathBuf,
) -> buck2_error::Result<::thriftclient::ThriftChannelBuilder> {
    // NOTE: This timeout is absurdly high, but bear in mind that what we're
    // "comparing" to is a FS call that has no timeouts at all.
    const THRIFT_TIMEOUT_MS: u32 = 120_000;

    Ok(
        ::thriftclient::ThriftChannelBuilder::from_path(fb, socket.as_path())
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?
            .with_conn_timeout(THRIFT_TIMEOUT_MS)
            .with_recv_timeout(THRIFT_TIMEOUT_MS)
            .with_secure(false),
    )
}

impl EdenConnector {
    fn connect(&self) -> EdenClientFuture {
        let socket = self.socket.clone();
        let fb = self.fb;
        let mount = self.mount.dupe();

        tokio::task::spawn(async move {
            tracing::info!("Creating a new Eden connection via `{}`", socket.display());
            let eden: Arc<dyn EdenService + Send + Sync> = thrift_builder(fb, &socket)?
                .build_client(::edenfs_clients::make_EdenService)
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
                .buck_error_context("Error constructing Eden client")?;

            wait_until_mount_is_ready(eden.as_ref(), &mount).await?;

            Ok(eden)
        })
        .map(|r| match r {
            Ok(r) => r,
            Err(e) => Err(e.into()), // Turn the JoinError into a buck2_error::Error.
        })
        .boxed()
        .shared()
    }

    fn connect_fb303(&self) -> buck2_error::Result<Arc<dyn BaseService + Send + Sync>> {
        thrift_builder(self.fb, &self.socket)?
            .build_client(::fb303_core_clients::make_BaseService)
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
    }
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = IoEdenMountNotReady)]
#[error("Mount never became ready: `{mount}`")]
struct MountNeverBecameReady {
    mount: AbsPathBuf,
}

/// Delay until a mount becomes ready (up to 10 seconds).
async fn wait_until_mount_is_ready(
    eden: &(dyn EdenService + Send + Sync),
    mount: &EdenMountPoint,
) -> buck2_error::Result<()> {
    let mut interval = tokio::time::interval(Duration::from_secs(1));
    interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);

    for _ in 0..10 {
        interval.tick().await;
        match is_mount_ready(eden, mount).await {
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

    Err(MountNeverBecameReady {
        mount: mount.0.clone(),
    }
    .into())
}

#[derive(buck2_error::Error, Debug)]
pub enum IsMountReadyError {
    #[error("Mount does not exist in Eden: `{mount}`")]
    #[buck2(tag = IoEdenMountDoesNotExist)]
    MountDoesNotExist { mount: AbsPathBuf },
    #[error(transparent)]
    #[buck2(tag = IoEdenRequestError)]
    RequestError(ListMountsError),
}

/// Check if a given mount is ready.
async fn is_mount_ready(
    eden: &(dyn EdenService + Send + Sync),
    mount: &EdenMountPoint,
) -> Result<bool, IsMountReadyError> {
    let mounts = eden
        .listMounts()
        .await
        .map_err(IsMountReadyError::RequestError)?;

    for candidate in mounts {
        if candidate.mountPoint == mount.0.as_path().as_os_str().as_encoded_bytes() {
            return Ok(candidate.state == MountState::RUNNING);
        }
    }

    Err(IsMountReadyError::MountDoesNotExist {
        mount: mount.0.clone(),
    })
}

#[derive(buck2_error::Error, Debug)]
pub enum ConnectAndRequestError<E> {
    #[error(transparent)]
    #[buck2(tag = IoEdenConnectionError)]
    ConnectionError(buck2_error::Error),
    #[error("Eden Request Failed: {0}")]
    #[buck2(tag = IoEdenRequestError)]
    RequestError(E),
}

impl<E> std::error::Error for ConnectAndRequestError<E>
where
    E: std::error::Error,
    Self: std::fmt::Debug + std::fmt::Display + std::marker::Send + std::marker::Sync + 'static,
{
    fn source(&self) -> std::option::Option<&(dyn std::error::Error + 'static)> {
        use buck2_error::__for_macro::AsDynError;
        match self {
            ConnectAndRequestError::ConnectionError { 0: transparent } => {
                std::error::Error::source(transparent.as_dyn_error())
            }
            ConnectAndRequestError::RequestError { 0: transparent } => {
                std::error::Error::source(transparent.as_dyn_error())
            }
        }
    }
}

#[derive(Copy, Clone, Dupe, PartialEq, Eq)]
pub enum ErrorHandlingStrategy {
    Reconnect,
    Retry,
    Abort,
}

pub trait HasErrorHandlingStrategy {
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

macro_rules! impl_has_error_handling_strategy {
    ($err: ident) => {
        impl HasErrorHandlingStrategy for ::edenfs_clients::errors::$err {
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

impl_has_error_handling_strategy!(GetAttributesFromFilesError);
impl_has_error_handling_strategy!(GetAttributesFromFilesV2Error);
impl_has_error_handling_strategy!(GlobFilesError);
impl_has_error_handling_strategy!(ListMountsError);
impl_has_error_handling_strategy!(SynchronizeWorkingCopyError);
impl_has_error_handling_strategy!(SetPathObjectIdError);
impl_has_error_handling_strategy!(RemoveRecursivelyError);
impl_has_error_handling_strategy!(EnsureMaterializedError);
impl_has_error_handling_strategy!(ReaddirError);
impl_has_error_handling_strategy!(GetSHA1Error);
impl_has_error_handling_strategy!(GetCurrentJournalPositionError);
impl_has_error_handling_strategy!(ChangesSinceV2Error);

fn eden_posix_error_tag(code: i32) -> ErrorTag {
    match code {
        libc::ENOENT => ErrorTag::IoEdenFileNotFound,
        libc::EACCES | libc::EPERM => ErrorTag::IoPermissionDenied,
        libc::ETIMEDOUT => ErrorTag::IoTimeout,
        libc::EBUSY => ErrorTag::IoExecutableFileBusy,
        libc::EPIPE => ErrorTag::IoBrokenPipe,
        libc::ENOSPC => ErrorTag::IoStorageFull,
        libc::ECONNABORTED => ErrorTag::IoConnectionAborted,
        libc::ENOTCONN => ErrorTag::IoNotConnected,
        _ => ErrorTag::IoEden,
    }
}

fn eden_service_error_tag(error: &edenfs::EdenError) -> ErrorTag {
    match error.errorType {
        EdenErrorType::WIN32_ERROR => ErrorTag::IoEdenWin32Error,
        EdenErrorType::HRESULT_ERROR => ErrorTag::IoEdenHresultError,
        EdenErrorType::ARGUMENT_ERROR => ErrorTag::IoEdenArgumentError,
        EdenErrorType::GENERIC_ERROR => ErrorTag::IoEdenGenericError,
        EdenErrorType::MOUNT_GENERATION_CHANGED => ErrorTag::IoEdenMountGenerationChanged,
        EdenErrorType::JOURNAL_TRUNCATED => ErrorTag::IoEdenJournalTruncated,
        EdenErrorType::CHECKOUT_IN_PROGRESS => ErrorTag::IoEdenCheckoutInProgress,
        EdenErrorType::OUT_OF_DATE_PARENT => ErrorTag::IoEdenOutOfDateParent,
        _ => ErrorTag::IoEden,
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = IoEden)]
pub enum EdenError {
    #[error("Eden POSIX error (code = {code}): {0}", error.message)]
    #[buck2(tag = eden_posix_error_tag(code))]
    PosixError { error: edenfs::EdenError, code: i32 },

    #[error("Eden service error: {0}", error.message)]
    #[buck2(tag = eden_service_error_tag(&error))]
    ServiceError { error: edenfs::EdenError },

    #[error("Eden returned an unexpected field: {field}")]
    #[buck2(tag = IoEdenUnknownField)]
    UnknownField { field: i32 },
}

impl From<edenfs::EdenError> for EdenError {
    fn from(error: edenfs::EdenError) -> Self {
        if error.errorType == EdenErrorType::POSIX_ERROR {
            if let Some(error_code) = error.errorCode {
                return Self::PosixError {
                    error,
                    code: error_code,
                };
            }
        } else if error.errorType == EdenErrorType::GENERIC_ERROR {
            // TODO(minglunli): Hacky solution to check if Eden errors are cert related
            if let Err(e) = futures::executor::block_on(validate_certs()) {
                let eden_err = edenfs::EdenError {
                    message: format!("{}", e),
                    ..error
                };

                return Self::ServiceError { error: eden_err };
            }
        }

        Self::ServiceError { error }
    }
}

pub trait EdenDataIntoResult {
    type Data;

    fn into_result(self) -> Result<Self::Data, EdenError>;
}

macro_rules! impl_eden_data_into_result {
    ($typ: ident, $data: ty, $ok_variant: ident) => {
        impl EdenDataIntoResult for ::edenfs::$typ {
            type Data = $data;

            fn into_result(self) -> Result<Self::Data, EdenError> {
                match self {
                    Self::$ok_variant(data) => Ok(data),
                    Self::error(e) => Err(e.into()),
                    Self::UnknownField(field) => Err(EdenError::UnknownField { field }),
                }
            }
        }
    };
}

impl_eden_data_into_result!(
    SourceControlTypeOrError,
    SourceControlType,
    sourceControlType
);

impl_eden_data_into_result!(FileAttributeDataOrError, FileAttributeData, data);

impl_eden_data_into_result!(
    FileAttributeDataOrErrorV2,
    FileAttributeDataV2,
    fileAttributeData
);

impl_eden_data_into_result!(SizeOrError, i64, size);

impl_eden_data_into_result!(Sha1OrError, BinaryHash, sha1);

impl_eden_data_into_result!(Blake3OrError, BinaryHash, blake3);

impl_eden_data_into_result!(
    DirListAttributeDataOrError,
    SortedVectorMap<PathString, FileAttributeDataOrErrorV2>,
    dirListAttributeData
);
