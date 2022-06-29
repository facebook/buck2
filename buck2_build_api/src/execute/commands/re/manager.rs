/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// This triggers on Arc<Arc<...>>, but we do that here for lifetime/ownership reasons
#![allow(clippy::redundant_allocation)]

use std::sync::Arc;
use std::sync::Mutex;
use std::sync::RwLock;
use std::sync::Weak;

use anyhow::Context as _;
use buck2_core::async_once_cell::AsyncOnceCell;
use buck2_core::result::SharedResult;
use buck2_node::execute::config::RemoteExecutorUseCase;
use fbinit::FacebookInit;
use gazebo::prelude::*;
use remote_execution as RE;
use remote_execution::ActionResultResponse;
use remote_execution::ExecuteResponse;
use remote_execution::InlinedBlobWithDigest;
use remote_execution::NamedDigest;
use remote_execution::NamedDigestWithPermissions;
use remote_execution::NetworkStatisticsResponse;
use remote_execution::TDigest;

use crate::actions::directory::ActionImmutableDirectory;
use crate::execute::commands::re::client::ActionDigest;
use crate::execute::commands::re::client::RemoteExecutionClient;
use crate::execute::commands::re::client::RemoteExecutionStaticMetadata;
use crate::execute::commands::re::uploader::ActionBlobs;
use crate::execute::commands::re::ReActionIdentity;
use crate::execute::commands::re::ReExecutorGlobalKnobs;
use crate::execute::commands::CommandExecutionManager;
use crate::execute::materializer::Materializer;

/// Lifetime management of the Remote Execution connection (i.e. the RemoteExecutionClient).
///
/// The [ReConnectionManager] ensures that there is only ever a single remote execution connection. It
/// vends out a [ReConnectionHandle] on calls to [ReConnectionManager::get_re_connection()].
///
/// There will only be at most a single RE connection during the lifetime of an [ReConnectionHandle], the
/// connection will not be closed as long as any handle still exists. The [ReConnectionHandle] then vends
/// out a [ManagedRemoteExecutionClient].
///
/// The [ManagedRemoteExecutionClient] has all the RE functions on it. The first time one of these is
/// called will be when the RE connection is actually made, if no calls to RE are made, we don't create
/// a connection.
///
/// When a [ManagedRemoteExecutionClient] is used, if it's source [ReConnectionHandle] still exists then
/// the calls will go to the underlying RE client (and making the connection if this is the first call). If,
/// however, the [ReConnectionHandle] has been dropped the calls will fail (even if the connection is still
/// alive from other handles).
///
/// When all existing [ReConnectionHandle] (for a particular [ReConnectionManager]) are dropped, the
/// underlying connection will be closed.
///
/// This approach allows us to ensure that all RE interactions for a particular buck command use the
/// same RE session. Concurrent commands will share an RE session.

#[derive(Clone)]
struct RemoteExecutionConfig {
    fb: FacebookInit,
    /// whether to skip the cache when performing RE
    skip_remote_cache: bool,
    /// number of retries when attempting the initial RE connection
    connection_retries: usize,
    static_metadata: Arc<RemoteExecutionStaticMetadata>,
    logs_dir_path: Option<String>,
}

impl RemoteExecutionConfig {
    async fn connect_now(&self) -> anyhow::Result<RemoteExecutionClient> {
        RemoteExecutionClient::new_retry(
            self.fb,
            self.skip_remote_cache,
            self.connection_retries,
            self.static_metadata.dupe(),
            self.logs_dir_path.as_deref(),
        )
        .await
    }
}

pub trait ReConnectionObserver: 'static + Send + Sync {
    fn session_created(&self, session_id: &str);
}

struct LazyRemoteExecutionClient {
    client: AsyncOnceCell<SharedResult<RemoteExecutionClient>>,
    observers: Mutex<Vec<Weak<dyn ReConnectionObserver>>>,
    config: RemoteExecutionConfig,
}

impl LazyRemoteExecutionClient {
    fn new(config: RemoteExecutionConfig) -> LazyRemoteExecutionClient {
        Self {
            client: AsyncOnceCell::new(),
            observers: Mutex::new(Vec::new()),
            config,
        }
    }

    /// Apply F to the client contained in this LazyRemoteExecutionClient if and only if there is a
    /// valid client.
    fn with_client<F, T>(&self, f: F) -> Option<T>
    where
        F: FnOnce(&RemoteExecutionClient) -> T,
    {
        match self.client.get().as_ref() {
            Some(Ok(client)) => Some(f(client)),
            _ => None,
        }
    }

    async fn get(&self) -> anyhow::Result<&RemoteExecutionClient> {
        match self
            .client
            .get_or_init(async move { self.init().await })
            .await
        {
            Ok(v) => Ok(v),
            Err(e) => Err(e.dupe().into()),
        }
    }

    async fn init(&self) -> SharedResult<RemoteExecutionClient> {
        let client = self.config.connect_now().await?;

        let mut observers = self.observers.lock().unwrap();
        let session_id = client.get_session_id();
        let owned = std::mem::take(&mut *observers);
        // session_created is the only thing on observer right now, so we can just throw them all out at this point.
        for o in owned.into_iter() {
            if let Some(o) = o.upgrade() {
                o.session_created(session_id);
            }
        }
        Ok(client)
    }

    // Adds an observer. This observer will immediately get a session_created() call if we are already connected.
    fn observe(&self, observer: &Arc<dyn ReConnectionObserver>) {
        // lock observers so we don't race with the notifications in init()
        let mut observers = self.observers.lock().unwrap();
        match self.client.get() {
            Some(Ok(client)) => {
                observer.session_created(client.get_session_id());
            }
            Some(_) => {
                // ignored
            }
            _ => {
                observers.push(Arc::downgrade(observer));
            }
        }
    }
}

/// The main manager for the RE connections
pub struct ReConnectionManager {
    // We hold a single Weak to the lazy client. ReConnectionHandle will hold a strong Arc to the same. Once the
    // last ReConnectionHandle is dropped, the client we point to will be dropped and we'll create a new one for
    // the next ReConnectionHandle.
    data: RwLock<Weak<LazyRemoteExecutionClient>>,
    config: RemoteExecutionConfig,
}

impl ReConnectionManager {
    pub fn new(
        fb: FacebookInit,
        skip_remote_cache: bool,
        connection_retries: usize,
        static_metadata: Arc<RemoteExecutionStaticMetadata>,
        logs_dir_path: Option<String>,
    ) -> Self {
        Self {
            data: RwLock::new(Weak::new()),
            config: RemoteExecutionConfig {
                fb,
                skip_remote_cache,
                connection_retries,
                static_metadata,
                logs_dir_path,
            },
        }
    }

    /// Gets a new guard that holds a RE connection open
    pub fn get_re_connection(&self) -> ReConnectionHandle {
        ReConnectionHandle::new(self.get_client_handle())
    }

    fn get_client_handle(&self) -> Arc<LazyRemoteExecutionClient> {
        if let Some(conn) = self.data.read().unwrap().upgrade() {
            return conn;
        }

        let mut conn = self.data.write().unwrap();
        match conn.upgrade() {
            None => {
                let new_connection = Arc::new(LazyRemoteExecutionClient::new(self.config.clone()));
                *conn = Arc::downgrade(&new_connection);
                new_connection
            }
            Some(conn) => conn,
        }
    }

    pub fn get_network_stats(&self) -> anyhow::Result<Option<NetworkStatisticsResponse>> {
        let conn = self.data.read().unwrap().upgrade();
        conn.as_ref()
            .and_then(|lazy_client| lazy_client.with_client(|client| client.get_network_stats()))
            .transpose()
    }
}

/// The guard that holds the RE connection open. This can outlive the main manager
/// The client obtained from this handle will only be valid while this handle is held.
pub struct ReConnectionHandle {
    /// here we hold an Arc<Arc> so that we can hand out a Weak<Arc> so that upon the handle being
    /// dropped, the clients will lose the ability to access connection
    // TODO(cjhopman): While we vend out Weak<Arc> with this, due to the way this is stored/used on
    // the dice graph we are guaranteed that there's an Arc that outlives all the Weak from it. That
    // kind of defeats the purpose of this.
    #[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_arc_on_dupe))]
    connection: Arc<Arc<LazyRemoteExecutionClient>>,
    // We use a similar Arc/Weak to ensure that the observer only lives as long as the connection
    // handle. Otherwise it would be possible to observer a session create attached to one command
    // after that command ended. An alternative would be to register/deregister the connection
    // handle itself as an observer on the lazy client, but that doesn't seem any simpler.
    observer: Option<Arc<dyn ReConnectionObserver>>,
}

impl ReConnectionHandle {
    fn new(connection: Arc<LazyRemoteExecutionClient>) -> Self {
        Self {
            connection: Arc::new(connection),
            observer: None,
        }
    }

    /// Sets the connection observer. This will drop the previous observer if there is one.
    pub fn set_observer(&mut self, observer: Arc<dyn ReConnectionObserver>) {
        // We store it just to give it this handle's lifetime.
        self.connection.observe(&observer);
        self.observer = Some(observer);
    }

    /// gets a client that is tied to the scope of this guard
    pub fn get_client(&self) -> ManagedRemoteExecutionClient {
        ManagedRemoteExecutionClient {
            data: Arc::downgrade(&self.connection),
        }
    }
}

#[derive(Clone, Dupe)]
pub struct ManagedRemoteExecutionClient {
    data: Weak<Arc<LazyRemoteExecutionClient>>,
}

impl ManagedRemoteExecutionClient {
    #[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_arc_on_dupe))]
    fn lock(&self) -> anyhow::Result<Arc<Arc<LazyRemoteExecutionClient>>> {
        self.data
            .upgrade()
            .context("Internal error: the underlying RE connection has terminated because the corresponding guard has been dropped.")
    }

    pub async fn action_cache(
        &self,
        action_digest: ActionDigest,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<Option<ActionResultResponse>> {
        Ok(self
            .lock()?
            .get()
            .await?
            .action_cache(action_digest, use_case)
            .await)
    }

    pub async fn upload(
        &self,
        materializer: Arc<dyn Materializer>,
        blobs: &ActionBlobs,
        input_dir: &ActionImmutableDirectory,
        use_case: RemoteExecutorUseCase,
        knobs: &ReExecutorGlobalKnobs,
    ) -> anyhow::Result<()> {
        self.lock()?
            .get()
            .await?
            .upload(materializer, blobs, input_dir, use_case, knobs)
            .await
    }

    pub async fn upload_files_and_directories(
        &self,
        files_with_digest: Vec<NamedDigest>,
        directories: Vec<remote_execution::Path>,
        inlined_blobs_with_digest: Vec<InlinedBlobWithDigest>,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<()> {
        self.lock()?
            .get()
            .await?
            .upload_files_and_directories(
                files_with_digest,
                directories,
                inlined_blobs_with_digest,
                use_case,
            )
            .await
    }

    pub async fn execute(
        &self,
        action_digest: ActionDigest,
        platform: &RE::Platform,
        use_case: RemoteExecutorUseCase,
        identity: &ReActionIdentity<'_, '_>,
        manager: &mut CommandExecutionManager,
    ) -> anyhow::Result<ExecuteResponse> {
        self.lock()?
            .get()
            .await?
            .execute(action_digest, platform, use_case, identity, manager)
            .await
    }

    pub async fn materialize_files(
        &self,
        files: Vec<NamedDigestWithPermissions>,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<()> {
        self.lock()?
            .get()
            .await?
            .materialize_files(files, use_case)
            .await
    }

    pub async fn download_trees(
        &self,
        digests: Vec<TDigest>,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<Vec<RE::Tree>> {
        self.lock()?
            .get()
            .await?
            .download_trees(digests, use_case)
            .await
    }

    pub async fn download_blob(
        &self,
        digest: &TDigest,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<Vec<u8>> {
        self.lock()?
            .get()
            .await?
            .download_blob(digest, use_case)
            .await
    }

    pub async fn get_session_id(&self) -> anyhow::Result<String> {
        let session_id = self.lock()?.get().await?.get_session_id().to_owned();
        Ok(session_id)
    }
}
