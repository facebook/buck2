use std::process::Child;
use std::sync::Arc;

use anyhow::Context;
use futures::future;
use futures::future::Future;
use futures::future::FutureExt;
use futures::stream;
use futures::stream::StreamExt;
use gazebo::prelude::*;
use tonic::transport::Channel;

use crate::convert::decode_event_stream;
use crate::run::decode_command_event_stream;
use crate::run::GatherOutputStatus;

#[derive(Clone, Dupe)]
pub struct ForkserverClient {
    inner: Arc<ForkserverClientInner>,
}

struct ForkserverClientInner {
    /// Keep the process reference to prevent its termination.
    _child: Child,
    rpc: forkserver_proto::forkserver_client::ForkserverClient<Channel>,
}

impl ForkserverClient {
    #[allow(unused)] // Unused on Windows
    pub(crate) fn new(child: Child, channel: Channel) -> Self {
        let rpc = forkserver_proto::forkserver_client::ForkserverClient::new(channel);
        Self {
            inner: Arc::new(ForkserverClientInner { _child: child, rpc }),
        }
    }

    pub async fn execute<C>(
        &self,
        req: forkserver_proto::CommandRequest,
        cancel: C,
    ) -> anyhow::Result<(GatherOutputStatus, Vec<u8>, Vec<u8>)>
    where
        C: Future<Output = ()> + Send + 'static,
    {
        let stream = stream::once(future::ready(forkserver_proto::RequestEvent {
            data: Some(req.into()),
        }))
        .chain(stream::once(cancel.map(|()| {
            forkserver_proto::RequestEvent {
                data: Some(forkserver_proto::CancelRequest {}.into()),
            }
        })));

        let stream = self
            .inner
            .rpc
            .clone()
            .run(stream)
            .await
            .context("Error dispatching command to Forkserver")?
            .into_inner();
        let stream = decode_event_stream(stream);
        decode_command_event_stream(stream).await
    }
}
