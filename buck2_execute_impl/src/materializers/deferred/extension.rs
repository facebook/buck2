/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::sync::Arc;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_execute::materialize::materializer::DeferredMaterializerEntry;
use buck2_execute::materialize::materializer::DeferredMaterializerExtensions;
use buck2_execute::re::manager::ReConnectionManager;
use chrono::Duration;
use derivative::Derivative;
use derive_more::Display;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use gazebo::prelude::*;
use tokio::sync::mpsc;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::oneshot;
use tokio::sync::oneshot::Sender;
use tokio::task::JoinHandle;
use tokio_stream::wrappers::UnboundedReceiverStream;

use crate::materializers::deferred::create_ttl_refresh;
use crate::materializers::deferred::ArtifactMaterializationMethod;
use crate::materializers::deferred::ArtifactMaterializationStage;
use crate::materializers::deferred::ArtifactTree;
use crate::materializers::deferred::DeferredMaterializer;
use crate::materializers::deferred::MaterializerCommand;
use crate::materializers::deferred::WithPathsIterator;

pub(super) trait ExtensionCommand: Debug + Sync + Send + 'static {
    fn execute(self: Box<Self>, tree: &ArtifactTree, re_manager: &Arc<ReConnectionManager>);
}

#[derive(Debug, Display)]
enum PathData {
    #[display(fmt = "materialized")]
    Materialized,

    #[display(fmt = "declared: {}", .0)]
    Declared(Arc<ArtifactMaterializationMethod>),
}

impl DeferredMaterializerEntry for PathData {}

#[derive(Derivative)]
#[derivative(Debug)]
struct Iterate {
    /// This is for debug commands so we use an unbounded channel to avoid locking up the
    /// materializer command thread.
    #[derivative(Debug = "ignore")]
    sender: UnboundedSender<(ProjectRelativePathBuf, Box<dyn DeferredMaterializerEntry>)>,
}

impl ExtensionCommand for Iterate {
    fn execute(self: Box<Self>, tree: &ArtifactTree, _re_manager: &Arc<ReConnectionManager>) {
        for (path, data) in tree.iter().with_paths() {
            let path_data = match &data.stage {
                ArtifactMaterializationStage::Declared { method, .. } => {
                    PathData::Declared(method.dupe())
                }
                ArtifactMaterializationStage::Materialized { .. } => PathData::Materialized,
            };

            match self.sender.send((path, box path_data as _)) {
                Ok(..) => {}
                Err(..) => break, // No use sending more if the client disconnected.
            }
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
struct RefreshTtls {
    sender: Sender<Option<JoinHandle<anyhow::Result<()>>>>,
    min_ttl: i64,
}

impl ExtensionCommand for RefreshTtls {
    fn execute(self: Box<Self>, tree: &ArtifactTree, re_manager: &Arc<ReConnectionManager>) {
        let task = create_ttl_refresh(tree, re_manager, Duration::seconds(self.min_ttl))
            .map(tokio::task::spawn);
        let _ignored = self.sender.send(task);
    }
}

#[async_trait]
impl DeferredMaterializerExtensions for DeferredMaterializer {
    fn iterate(
        &self,
    ) -> anyhow::Result<
        BoxStream<'static, (ProjectRelativePathBuf, Box<dyn DeferredMaterializerEntry>)>,
    > {
        let (sender, receiver) = mpsc::unbounded_channel();
        self.command_sender
            .send(MaterializerCommand::Extension(box Iterate { sender } as _))?;
        Ok(UnboundedReceiverStream::new(receiver).boxed())
    }

    async fn refresh_ttls(&self, min_ttl: i64) -> anyhow::Result<()> {
        let (sender, receiver) = oneshot::channel();
        self.command_sender.send(MaterializerCommand::Extension(
            box RefreshTtls { sender, min_ttl } as _,
        ))?;
        match receiver.await.context("No response from materializer")? {
            Some(task) => task
                .await
                .context("Refresh task aborted")?
                .context("Refresh failed")?,
            None => {}
        };
        Ok(())
    }
}
