/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    fmt::{Debug, Display},
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context as _;
use async_trait::async_trait;
use futures::future::Future;
use gazebo::prelude::*;
use serde::Deserialize;
use tokio::sync::{
    mpsc::{UnboundedReceiver, UnboundedSender},
    oneshot,
};
use watchman_client::prelude::*;

#[cfg(all(test, not(windows)))]
mod test;

// We use the "new" field. This is marked as deprecated, but buck1 uses it and
// I'm unaware of issues due to its use there.
//
// Putting this in it own mod was the best way to scope the allow(deprecated).
#[allow(deprecated)]
mod types {
    use super::*;
    query_result_type! {
        pub struct BuckQueryResult {
            name: NameField,
            file_type: FileTypeField,
            exists: ExistsField,
            new: NewField,
        }
    }

    impl BuckQueryResult {
        pub fn into_event(self) -> Option<WatchmanEvent> {
            let kind = match *self.file_type {
                FileType::BlockSpecial
                | FileType::CharSpecial
                | FileType::Fifo
                | FileType::Socket
                | FileType::SolarisDoor => {
                    return None;
                }
                FileType::Directory => WatchmanKind::Directory,
                FileType::Regular => WatchmanKind::File,
                FileType::Symlink => WatchmanKind::Symlink,
            };

            let event = match (*self.exists, *self.new) {
                (true, true) => WatchmanEventType::Create,
                (false, _) => WatchmanEventType::Delete,
                _ => WatchmanEventType::Modify,
            };

            Some(WatchmanEvent {
                kind,
                event,
                path: self.name.into_inner(),
            })
        }
    }
}

use types::*;

#[derive(Debug)]
pub enum WatchmanEventType {
    Create,
    Modify,
    Delete,
}

#[derive(Debug)]
pub enum WatchmanKind {
    File,
    Directory,
    Symlink,
}

impl WatchmanKind {
    fn symbol(&self) -> &'static str {
        match self {
            WatchmanKind::File => "f",
            WatchmanKind::Directory => "d",
            WatchmanKind::Symlink => "l",
        }
    }
}

#[derive(Debug)]
pub struct WatchmanEvent {
    pub kind: WatchmanKind,
    pub event: WatchmanEventType,
    pub path: PathBuf,
}

impl Display for WatchmanEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}{}<{}>",
            &self.event,
            self.kind.symbol(),
            self.path.to_string_lossy()
        )
    }
}

#[derive(Dupe, Clone)]
pub struct WatchmanClient(Arc<(watchman_client::Client, ResolvedRoot)>);

impl Debug for WatchmanClient {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "watchman<{:?}>", self.0.1)
    }
}

impl WatchmanClient {
    pub async fn connect(
        connector: &Connector,
        path: CanonicalPath,
    ) -> anyhow::Result<WatchmanClient> {
        let client = connector.connect().await?;
        let root = client.resolve_root(path).await?;
        Ok(Self(Arc::new((client, root))))
    }

    pub async fn query<
        F: serde::de::DeserializeOwned + std::fmt::Debug + Clone + QueryFieldList,
    >(
        &self,
        query: QueryRequestCommon,
    ) -> anyhow::Result<QueryResult<F>> {
        Ok(self.client().query(self.root(), query).await?)
    }

    fn root(&self) -> &ResolvedRoot {
        &self.0.1
    }

    fn client(&self) -> &watchman_client::Client {
        &self.0.0
    }
}

#[async_trait]
pub trait SyncableQueryProcessor: Send + Sync {
    type Output;

    /// Process a set of filesystem change events.
    async fn process_events(&self, events: Vec<WatchmanEvent>) -> anyhow::Result<Self::Output>;

    /// Indicates that all derived data should be invalidated. This could happen, for example, if the watchman server restarts.
    async fn on_fresh_instance(&self) -> anyhow::Result<Self::Output>;
}

/// commands to be sent to the SyncableQueryHandler.
enum SyncableQueryCommand<T> {
    Sync(oneshot::Sender<anyhow::Result<T>>),
}

/// A SyncableQuery is similar to a subscription. When created, it accepts a query expression
/// and a SyncableQueryProcessor. It can be used to maintain some state that depends on the
/// results of the query expression. As changes are made to the filesystem, the SyncableQueryProcessor
/// will be called to process the events.
///
/// It provides a single function `sync()` that will ensure that all file events seen by watchman have
/// been processed by the SyncableQueryProcessor.
///
/// In the background, the SyncableQuery may use a subscription to eagerly process updates, but this is
/// only an optimization and users should use `sync()` when they want events to have been processed.
pub struct SyncableQuery<T> {
    control_tx: UnboundedSender<SyncableQueryCommand<T>>,
}

/// The SyncableQueryHandler runs a processing loop that communicates with watchman and invokes the SyncableQueryProcessor. As
/// these only happen within the handler's run loop, for a particular SyncableQuery all watchman invocations and processor
/// processing will happen in a linear order.
///
/// The SyncableQueryHandler maintains the clock and last mergebase and updates them with each request.
struct SyncableQueryHandler<T> {
    connector: Connector,
    path: CanonicalPath,
    client: WatchmanClient,
    processor: Box<dyn SyncableQueryProcessor<Output = T>>,
    query: QueryRequestCommon,
    last_clock: ClockSpec,
    last_mergebase: Option<String>,
    mergebase_with: Option<String>,
    control_rx: UnboundedReceiver<SyncableQueryCommand<T>>,
}

impl<T> SyncableQueryHandler<T>
where
    T: Send + 'static,
{
    async fn run_loop(&mut self) {
        loop {
            match self.control_rx.recv().await {
                Some(SyncableQueryCommand::Sync(sync_tx)) => {
                    let res = match self.sync().await {
                        Ok(res) => Ok(res),
                        Err(e) => self.reconnect_and_sync().await.context(e),
                    };

                    // NOTE: If the receiver is gone, then they won't be told we finished their
                    // job. That's fine.
                    let _ignore = sync_tx.send(res);
                }
                None => {
                    // This indicates the controlling SyncableQuery has been dropped.
                    return;
                }
            }
        }
    }

    async fn reconnect(&mut self) -> anyhow::Result<()> {
        self.last_clock = Default::default();
        self.last_mergebase = None;
        self.client = WatchmanClient::connect(&self.connector, self.path.clone())
            .await
            .context("Error reconnecting to Watchman")?;
        Ok(())
    }

    async fn reconnect_and_sync(&mut self) -> anyhow::Result<T> {
        self.reconnect()
            .await
            .context("Error reconnecting to Watchman")?;

        let out = self.sync().await?;

        Ok(out)
    }

    /// sync() will send a since query to watchman and invoke the processor
    /// with either the received events or a fresh instance call.
    async fn sync(&mut self) -> anyhow::Result<T> {
        let mut query = self.query.clone();
        query.since = if let Some(mergebase_with) = self.mergebase_with.as_ref() {
            Some(Clock::ScmAware(FatClockData {
                clock: self.last_clock.clone(),
                scm: Some(ScmAwareClockData {
                    mergebase: self.last_mergebase.clone(),
                    mergebase_with: Some(mergebase_with.clone()),
                    saved_state: None,
                }),
            }))
        } else {
            Some(Clock::Spec(self.last_clock.clone()))
        };

        let QueryResult {
            // version,
            is_fresh_instance,
            files,
            clock,
            // subscription_canceled,
            // state_enter,
            // state_leave,
            // state_metadata,
            // saved_state_info,
            ..
        } = self.client.query::<BuckQueryResult>(query).await?;

        // While we use scm-based queries, the processor api doesn't really support them yet so we just treat it as a fresh instance.
        let (new_mergebase, clock) = unpack_clock(clock);

        let res = if !is_fresh_instance
            && (self.mergebase_with.is_none()
                || self.last_mergebase.is_some() && self.last_mergebase == new_mergebase)
        {
            // For regular wathcman queries, we process files when there are not fresh instances.
            // For scm aware queries, only process the files when it's not a fresh instance or mergebase change.
            let events: Vec<_> = files
                .ok_or_else(|| anyhow::anyhow!(""))?
                .into_iter()
                .filter_map(|f| f.into_event())
                .collect();

            self.processor.process_events(events).await?
        } else {
            self.processor.on_fresh_instance().await?
        };

        self.last_mergebase = new_mergebase;
        self.last_clock = clock;

        Ok(res)
    }
}

/// Unpacks the clock returned for an scm-aware query into a tuple of the mergebase and the clockspec.
fn unpack_clock(clock: Clock) -> (Option<String>, ClockSpec) {
    match clock {
        Clock::Spec(clock_spec) => (None, clock_spec),
        Clock::ScmAware(FatClockData {
            clock: clock_spec,
            scm:
                Some(ScmAwareClockData {
                    mergebase: Some(mergebase),
                    ..
                }),
        }) => (Some(mergebase), clock_spec),
        clock => panic!(
            "requested watchman query, got unexpected clock `{:?}`",
            clock
        ),
    }
}

impl<T> SyncableQuery<T>
where
    T: Send + 'static,
{
    /// Ensures that the processor has been sent all changes that watchman has seen.
    pub fn sync(&self) -> impl Future<Output = anyhow::Result<T>> + Send + 'static {
        let (sync_done_tx, sync_done_rx) = tokio::sync::oneshot::channel();
        let tx_res = self
            .control_tx
            .send(SyncableQueryCommand::Sync(sync_done_tx));

        async move {
            tx_res.ok().context("SyncableQueryHandler has exited")?;

            let out = sync_done_rx
                .await
                .context("SyncableQueryHandler did not return a response for sync request")?
                .context("SyncableQueryHandler returned an error")?;

            Ok(out)
        }
    }

    pub async fn new(
        connector: Connector,
        path: impl AsRef<Path>,
        expr: Expr,
        processor: Box<dyn SyncableQueryProcessor<Output = T>>,
        mergebase_with: Option<String>,
    ) -> anyhow::Result<SyncableQuery<T>> {
        let path = path.as_ref();
        let path = CanonicalPath::canonicalize(&path)
            .with_context(|| format!("Error canonicalizing: `{}`", path.display()))?;

        let client = WatchmanClient::connect(&connector, path.clone())
            .await
            .context("Error connecting to Watchman")?;

        let query = QueryRequestCommon {
            expression: Some(expr),
            fields: vec!["name"],
            empty_on_fresh_instance: true,
            relative_root: None,
            case_sensitive: true,
            dedup_results: false,
            // Required or we miss directory events
            always_include_directories: true,
            // TODO(cjhopman): Figure out reasonable timeouts.
            // sync_timeout: ???,
            // lock_timeout: ???,
            ..QueryRequestCommon::default()
        };

        let (control_tx, control_rx) =
            tokio::sync::mpsc::unbounded_channel::<SyncableQueryCommand<T>>();

        tokio::spawn(async move {
            let mut handler = SyncableQueryHandler {
                connector,
                path,
                client,
                query,
                last_clock: ClockSpec::default(),
                last_mergebase: None,
                mergebase_with,
                processor,
                control_rx,
            };
            handler.run_loop().await
        });

        Ok(Self { control_tx })
    }
}
