use std::fmt::Debug;

use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_execute::materialize::materializer::DeferredMaterializerExtensions;
use derivative::Derivative;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use tokio::sync::mpsc;
use tokio::sync::mpsc::UnboundedSender;
use tokio_stream::wrappers::UnboundedReceiverStream;

use crate::materializers::deferred::ArtifactTree;
use crate::materializers::deferred::DeferredMaterializer;
use crate::materializers::deferred::MaterializerCommand;
use crate::materializers::deferred::WithPathsIterator;

pub(super) trait ExtensionCommand: Debug + Sync + Send + 'static {
    fn execute(self: Box<Self>, tree: &ArtifactTree);
}

#[derive(Derivative)]
#[derivative(Debug)]
struct Iterate {
    /// This is for debug commands so we use an unbounded channel to avoid locking up the
    /// materializer command thread.
    #[derivative(Debug = "ignore")]
    sender: UnboundedSender<ProjectRelativePathBuf>,
}

impl ExtensionCommand for Iterate {
    fn execute(self: Box<Self>, tree: &ArtifactTree) {
        for (path, _) in tree.iter().with_paths() {
            match self.sender.send(path) {
                Ok(..) => {}
                Err(..) => break, // No use sending more if the client disconnected.
            }
        }
    }
}

impl DeferredMaterializerExtensions for DeferredMaterializer {
    fn iterate(&self) -> anyhow::Result<BoxStream<'static, ProjectRelativePathBuf>> {
        let (sender, receiver) = mpsc::unbounded_channel();
        self.command_sender
            .send(MaterializerCommand::Extension(box Iterate { sender } as _))?;
        Ok(UnboundedReceiverStream::new(receiver).boxed())
    }
}
