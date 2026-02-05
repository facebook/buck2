/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Write;
use std::sync::Arc;
use std::time::Instant;

use async_trait::async_trait;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::get_dispatcher;
use buck2_execute::materialize::materializer::DeferredMaterializerEntry;
use buck2_execute::materialize::materializer::DeferredMaterializerExtensions;
use buck2_execute::materialize::materializer::DeferredMaterializerIterItem;
use buck2_execute::materialize::materializer::DeferredMaterializerSubscription;
use buck2_fs::error::IoResultExt;
use buck2_fs::fs_util;
use chrono::DateTime;
use chrono::Duration;
use chrono::TimeZone;
use chrono::Utc;
use derivative::Derivative;
use dupe::Dupe;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use tokio::sync::mpsc;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::oneshot;
use tokio::sync::oneshot::Sender;
use tokio::task::JoinHandle;
use tokio_stream::wrappers::UnboundedReceiverStream;

use crate::materializers::deferred::ArtifactMaterializationMethod;
use crate::materializers::deferred::ArtifactMaterializationStage;
use crate::materializers::deferred::DeferredMaterializerAccessor;
use crate::materializers::deferred::DeferredMaterializerCommandProcessor;
use crate::materializers::deferred::MaterializerCommand;
use crate::materializers::deferred::Processing;
use crate::materializers::deferred::ProcessingFuture;
use crate::materializers::deferred::clean_stale::CleanStaleArtifactsCommand;
use crate::materializers::deferred::clean_stale::CleanStaleArtifactsExtensionCommand;
use crate::materializers::deferred::io_handler::IoHandler;
use crate::materializers::deferred::io_handler::create_ttl_refresh;
use crate::materializers::deferred::subscriptions::MaterializerSubscriptionOperation;

pub(super) trait ExtensionCommand<T>: Debug + Sync + Send + 'static {
    fn execute(self: Box<Self>, processor: &mut DeferredMaterializerCommandProcessor<T>);
}

#[derive(Debug)]
struct PathData {
    stage: PathStage,
    processing: PathProcessing,
}

#[derive(Debug)]
enum PathStage {
    Materialized {
        ts: DateTime<Utc>,
        size: Option<u64>,
    },
    Declared(Arc<ArtifactMaterializationMethod>),
}

#[derive(Debug)]
enum PathProcessing {
    Done,
    Materializing,
    Cleaning,
}

impl Display for PathData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.stage {
            PathStage::Materialized { ts, size } => {
                if let Some(size) = size {
                    write!(f, "materialized (ts={ts:?}, size={size})")?;
                } else {
                    write!(f, "materialized (ts={ts:?})")?;
                }
            }
            PathStage::Declared(method) => {
                write!(f, "declared: {method}")?;
            }
        }

        match &self.processing {
            PathProcessing::Done => {}
            PathProcessing::Materializing => {
                write!(f, " (materializing")?;
            }
            PathProcessing::Cleaning => {
                write!(f, " (cleaning)")?;
            }
        }

        Ok(())
    }
}

impl DeferredMaterializerEntry for PathData {}

#[derive(Derivative)]
#[derivative(Debug)]
struct Iterate {
    /// This is for debug commands so we use an unbounded channel to avoid locking up the
    /// materializer command thread.
    #[derivative(Debug = "ignore")]
    sender: UnboundedSender<DeferredMaterializerIterItem>,
}

impl<T: IoHandler> ExtensionCommand<T> for Iterate {
    fn execute(self: Box<Self>, processor: &mut DeferredMaterializerCommandProcessor<T>) {
        // Ensure up to date access times
        processor.flush_access_times(0);
        for (path, data) in processor.tree.iter_with_paths() {
            let stage = match &data.stage {
                ArtifactMaterializationStage::Declared { method, .. } => {
                    PathStage::Declared(method.dupe())
                }
                ArtifactMaterializationStage::Materialized {
                    last_access_time,
                    metadata,
                    ..
                } => {
                    // drop nano-seconds
                    let ts = Utc
                        .timestamp_opt(last_access_time.timestamp(), 0)
                        .single()
                        .unwrap();
                    PathStage::Materialized {
                        ts,
                        size: Some(metadata.size()),
                    }
                }
            };

            let processing = match &data.processing {
                Processing::Done(..) => PathProcessing::Done,
                Processing::Active {
                    future: ProcessingFuture::Materializing(..),
                    ..
                } => PathProcessing::Materializing,
                Processing::Active {
                    future: ProcessingFuture::Cleaning(..),
                    ..
                } => PathProcessing::Cleaning,
            };

            let path_data = PathData { stage, processing };

            let path = ProjectRelativePathBuf::from(path);

            let deps = match &data.deps {
                Some(deps) => processor.tree.find_artifacts_for_debug(deps),
                None => Vec::new(),
            };

            match self.sender.send(DeferredMaterializerIterItem {
                artifact_path: path,
                artifact_display: Box::new(path_data) as _,
                deps,
            }) {
                Ok(..) => {}
                Err(..) => break, // No use sending more if the client disconnected.
            }
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
struct ListSubscriptions {
    #[derivative(Debug = "ignore")]
    sender: UnboundedSender<ProjectRelativePathBuf>,
}

impl<T> ExtensionCommand<T> for ListSubscriptions {
    fn execute(self: Box<Self>, processor: &mut DeferredMaterializerCommandProcessor<T>) {
        for path in processor.subscriptions.list_subscribed_paths() {
            match self.sender.send(path.to_owned()) {
                Ok(..) => {}
                Err(..) => break, // No use sending more if the client disconnected.
            }
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
struct Fsck {
    /// This is for debug commands so we use an unbounded channel to avoid locking up the
    /// materializer command thread.
    #[derivative(Debug = "ignore")]
    sender: UnboundedSender<(ProjectRelativePathBuf, buck2_error::Error)>,
}

impl<T: IoHandler> ExtensionCommand<T> for Fsck {
    fn execute(self: Box<Self>, processor: &mut DeferredMaterializerCommandProcessor<T>) {
        for (path, data) in processor.tree.iter_with_paths() {
            match &data.stage {
                ArtifactMaterializationStage::Declared { .. } => {
                    continue;
                }
                ArtifactMaterializationStage::Materialized { .. } => {}
            };

            // We actually block the thread here. This is to ensure we don't try to delete things
            // when we check them. This is primarily a debug command. We don't run this while
            // actual things are in flight.

            let path = ProjectRelativePathBuf::from(path);
            let res =
                fs_util::symlink_metadata(processor.io.fs().resolve(&path)).categorize_internal();
            match res {
                Ok(..) => {}
                Err(e) => {
                    let _ignored = self.sender.send((path, e.into()));
                }
            }
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
struct RefreshTtls {
    sender: Sender<Option<JoinHandle<buck2_error::Result<()>>>>,
    min_ttl: i64,
}

impl<T: IoHandler> ExtensionCommand<T> for RefreshTtls {
    fn execute(self: Box<Self>, processor: &mut DeferredMaterializerCommandProcessor<T>) {
        let task = create_ttl_refresh(
            &processor.tree,
            processor.io.re_client_manager(),
            Duration::seconds(self.min_ttl),
            processor.io.digest_config(),
        )
        .map(|f| processor.spawn(f));
        let _ignored = self.sender.send(task);
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
struct GetTtlRefreshLog {
    sender: Sender<String>,
}

impl<T: IoHandler> ExtensionCommand<T> for GetTtlRefreshLog {
    fn execute(self: Box<Self>, processor: &mut DeferredMaterializerCommandProcessor<T>) {
        // We normally poll this very lazily, so actually force it to happen here.
        processor.poll_current_ttl_refresh();

        let mut out = String::new();

        for entry in &processor.ttl_refresh_history {
            write!(&mut out, "{:?}\t", entry.at).unwrap();
            match &entry.outcome {
                None => {
                    writeln!(&mut out, "SKIP").unwrap();
                }
                Some(Ok(())) => {
                    writeln!(&mut out, "OK").unwrap();
                }
                Some(Err(e)) => {
                    writeln!(&mut out, "ERR\t{e:#}").unwrap();
                }
            }
        }

        let _ignored = self.sender.send(out);
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
struct TestIter {
    sender: Sender<String>,
    count: usize,
}

impl<T> ExtensionCommand<T> for TestIter {
    fn execute(self: Box<Self>, processor: &mut DeferredMaterializerCommandProcessor<T>) {
        let mut out = String::new();

        let now = std::time::Instant::now();

        for _i in 0..self.count {
            let it = processor.tree.iter_without_paths();

            for e in it {
                let _e = e;
            }
        }

        writeln!(
            &mut out,
            "Elapsed for iter() ({} times): {:?}",
            self.count,
            Instant::now() - now
        )
        .unwrap();

        let now = std::time::Instant::now();

        for _i in 0..self.count {
            let it = processor.tree.iter_with_paths();

            for e in it {
                let _e = e;
            }
        }

        writeln!(
            &mut out,
            "Elapsed for iter().with_paths() ({} times): {:?}",
            self.count,
            Instant::now() - now
        )
        .unwrap();

        let _ignored = self.sender.send(out);
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
struct FlushAccessTimes {
    sender: Sender<String>,
}

impl<T: IoHandler> ExtensionCommand<T> for FlushAccessTimes {
    fn execute(self: Box<Self>, processor: &mut DeferredMaterializerCommandProcessor<T>) {
        let mut out = String::new();

        writeln!(&mut out, "{}", processor.flush_access_times(0)).unwrap();
        let _ignored = self.sender.send(out);
    }
}

#[async_trait]
impl<T: IoHandler> DeferredMaterializerExtensions for DeferredMaterializerAccessor<T> {
    fn iterate(&self) -> buck2_error::Result<BoxStream<'static, DeferredMaterializerIterItem>> {
        let (sender, receiver) = mpsc::unbounded_channel();
        self.command_sender.send(MaterializerCommand::Extension(
            Box::new(Iterate { sender }) as _
        ))?;
        Ok(UnboundedReceiverStream::new(receiver).boxed())
    }

    fn list_subscriptions(
        &self,
    ) -> buck2_error::Result<BoxStream<'static, ProjectRelativePathBuf>> {
        let (sender, receiver) = mpsc::unbounded_channel();
        self.command_sender
            .send(MaterializerCommand::Extension(
                Box::new(ListSubscriptions { sender }) as _,
            ))?;
        Ok(UnboundedReceiverStream::new(receiver).boxed())
    }

    fn fsck(
        &self,
    ) -> buck2_error::Result<BoxStream<'static, (ProjectRelativePathBuf, buck2_error::Error)>> {
        let (sender, receiver) = mpsc::unbounded_channel();
        self.command_sender.send(MaterializerCommand::Extension(
            Box::new(Fsck { sender }) as _
        ))?;
        Ok(UnboundedReceiverStream::new(receiver).boxed())
    }

    async fn refresh_ttls(&self, min_ttl: i64) -> buck2_error::Result<()> {
        let (sender, receiver) = oneshot::channel();
        self.command_sender
            .send(MaterializerCommand::Extension(
                Box::new(RefreshTtls { sender, min_ttl }) as _,
            ))?;
        match receiver
            .await
            .buck_error_context("No response from materializer")?
        {
            Some(task) => task
                .await
                .buck_error_context("Refresh task aborted")?
                .buck_error_context("Refresh failed")?,
            None => {}
        };
        Ok(())
    }

    async fn get_ttl_refresh_log(&self) -> buck2_error::Result<String> {
        let (sender, receiver) = oneshot::channel();
        self.command_sender
            .send(MaterializerCommand::Extension(
                Box::new(GetTtlRefreshLog { sender }) as _,
            ))?;
        receiver
            .await
            .buck_error_context("No response from materializer")
    }

    async fn clean_stale_artifacts(
        &self,
        keep_since_time: DateTime<Utc>,
        dry_run: bool,
        tracked_only: bool,
    ) -> buck2_error::Result<buck2_cli_proto::CleanStaleResponse> {
        let dispatcher = get_dispatcher();
        let (sender, recv) = oneshot::channel();
        self.command_sender
            .send(MaterializerCommand::Extension(Box::new(
                CleanStaleArtifactsExtensionCommand {
                    cmd: CleanStaleArtifactsCommand {
                        keep_since_time,
                        dry_run,
                        tracked_only,
                        dispatcher,
                    },
                    sender,
                },
            )))?;
        recv.await?.await.map(|res| res.into())
    }

    async fn test_iter(&self, count: usize) -> buck2_error::Result<String> {
        let (sender, receiver) = oneshot::channel();
        self.command_sender
            .send(MaterializerCommand::Extension(
                Box::new(TestIter { sender, count }) as _,
            ))?;
        receiver
            .await
            .buck_error_context("No response from materializer")
    }

    async fn flush_all_access_times(&self) -> buck2_error::Result<String> {
        let (sender, receiver) = oneshot::channel();
        self.command_sender
            .send(MaterializerCommand::Extension(
                Box::new(FlushAccessTimes { sender }) as _,
            ))?;
        receiver
            .await
            .buck_error_context("No response from materializer")
    }

    async fn create_subscription(
        &self,
    ) -> buck2_error::Result<Box<dyn DeferredMaterializerSubscription>> {
        let (sender, receiver) = oneshot::channel();
        self.command_sender.send(MaterializerCommand::Subscription(
            MaterializerSubscriptionOperation::Create { sender },
        ))?;
        Ok(Box::new(
            receiver
                .await
                .buck_error_context("No response from materializer")?,
        ) as _)
    }
}
