use std::fmt::Debug;
use std::sync::Arc;

use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_execute::materialize::materializer::DeferredMaterializerEntry;
use buck2_execute::materialize::materializer::DeferredMaterializerExtensions;
use derivative::Derivative;
use derive_more::Display;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use gazebo::prelude::*;
use tokio::sync::mpsc;
use tokio::sync::mpsc::UnboundedSender;
use tokio_stream::wrappers::UnboundedReceiverStream;

use crate::materializers::deferred::ArtifactMaterializationMethod;
use crate::materializers::deferred::ArtifactMaterializationStage;
use crate::materializers::deferred::ArtifactTree;
use crate::materializers::deferred::DeferredMaterializer;
use crate::materializers::deferred::MaterializerCommand;
use crate::materializers::deferred::WithPathsIterator;

pub(super) trait ExtensionCommand: Debug + Sync + Send + 'static {
    fn execute(self: Box<Self>, tree: &ArtifactTree);
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
    fn execute(self: Box<Self>, tree: &ArtifactTree) {
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
}
