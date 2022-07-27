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
use buck2_core;
use buck2_core::fs::anyhow as fs;
use buck2_core::fs::paths::AbsPathBuf;
use edenfs::client::EdenService;
use edenfs::errors::eden_service::ListMountsError;
use edenfs::types::MountState;
use fbinit::FacebookInit;
use futures::future::BoxFuture;
use futures::future::Future;
use futures::future::FutureExt;
use futures::future::Shared;
use gazebo::prelude::*;
use parking_lot::Mutex;
use thiserror::Error;
use tokio::sync::Semaphore;

use crate::result::SharedResult;

pub struct EdenConnectionManager {
    connector: EdenConnector,
    connection: Mutex<EdenConnection>,
    /// Eden has limits on concurrency and will return server overloaded (or timeout) errors if we
    /// send too many. Experimentally, even for large builds (see details in D36136516), we don't
    /// much performance improvement beyond 2K concurrent requests, regardless of whether Eden has
    /// a fast or slow connectin to source control, a warm cache or not, and a lot of CPU available
    /// to run or not.
    semaphore: Semaphore,
}

impl EdenConnectionManager {
    pub fn new(
        fb: FacebookInit,
        root: &AbsPathBuf,
        semaphore: Semaphore,
    ) -> anyhow::Result<Option<Self>> {
        let eden_root = root.join(".eden");
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

        Ok(Some(Self {
            connector,
            connection,
            semaphore,
        }))
    }

    pub fn get_root(&self) -> &str {
        &self.connector.root
    }

    pub fn get_mount_point(&self) -> Vec<u8> {
        self.connector.root.as_bytes().to_vec()
    }

    pub async fn with_eden<F, Fut, T, E>(&self, f: F) -> Result<T, ConnectAndRequestError<E>>
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
pub enum ConnectAndRequestError<E> {
    #[error(transparent)]
    ConnectionError(anyhow::Error),

    #[error(transparent)]
    RequestError(E),
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

impl_has_error_handling_strategy!(GetAttributesFromFilesError);
impl_has_error_handling_strategy!(GlobFilesError);
impl_has_error_handling_strategy!(ListMountsError);
impl_has_error_handling_strategy!(SynchronizeWorkingCopyError);
impl_has_error_handling_strategy!(SetPathObjectIdError);
impl_has_error_handling_strategy!(RemoveRecursivelyError);
impl_has_error_handling_strategy!(EnsureMaterializedError);
impl_has_error_handling_strategy!(ReaddirError);
