use anyhow::Context;
use futures::{
    future,
    stream::{self, StreamExt},
};
use tokio::{
    io::{AsyncRead, AsyncWrite},
    sync::mpsc::{self},
    task::JoinHandle,
};
use tonic::transport::server::Router;
use tower::Service;

use self::{connection_with_extra::ConnectionWithExtra, drop_notifier::DropNotifier};

pub struct ServerHandle {
    channel: DropNotifier,
    handle: JoinHandle<anyhow::Result<()>>,
}

impl ServerHandle {
    /// Tell the server to shutdown and wait for it to exit.
    pub async fn shutdown(self) -> anyhow::Result<()> {
        self.channel.notify_now();
        self.handle.await.context("Failed to join task")?
    }

    /// Obtain the JoinHandle to the task driving the server, without asking the server to
    /// shutdown.
    pub fn into_join_handle(self) -> JoinHandle<anyhow::Result<()>> {
        self.channel.cancel();
        self.handle
    }
}

pub fn spawn_oneshot<T, A, B>(io: T, router: Router<A, B>) -> ServerHandle
where
    T: AsyncRead + AsyncWrite + Send + Unpin + 'static + tonic::transport::server::Connected,

    A: Service<
            http::Request<tonic::transport::Body>,
            Response = http::Response<tonic::body::BoxBody>,
        > + Clone
        + Send
        + 'static,
    A::Future: Send + 'static,
    A::Error: Into<Box<dyn std::error::Error + Send + Sync>> + Send,
    B: Service<
            http::Request<tonic::transport::Body>,
            Response = http::Response<tonic::body::BoxBody>,
        > + Clone
        + Send
        + 'static,
    B::Future: Send + 'static,
    B::Error: Into<Box<dyn std::error::Error + Send + Sync>> + Send,
{
    // We reserve 2 slots here: one for the connection and one for the ServerHandle's
    // shutdown.
    let (send, mut recv) = mpsc::channel(2);

    let io = ConnectionWithExtra {
        inner: io,
        notifier: DropNotifier::new(send.clone()),
    };

    let fut = async move {
        // NOTE: We immediately yield the connection we have on hand, then just report no further
        // connections going forward. This is important to ensure that the server doesn't just
        // exist when it sees no connections (it's a little weird that the server exists even with
        // open connections if they're not in the middle of a request, but thats's what it is).
        let incoming = stream::once(future::ready(io))
            .chain(stream::once(future::pending()))
            .map(std::io::Result::Ok);

        router
            .serve_with_incoming_shutdown(incoming, async move {
                let _ignored = recv.recv().await;
            })
            .await
            .context("Server exited with an error")?;

        Ok(())
    };

    let handle = tokio::task::spawn(fut);

    ServerHandle {
        channel: DropNotifier::new(send),
        handle,
    }
}

mod drop_notifier {
    use tokio::sync::mpsc::Sender;

    /// Upon being dropped, this notifies the sender is contains.
    pub struct DropNotifier {
        sender: Option<Sender<()>>,
    }

    impl DropNotifier {
        pub fn new(sender: Sender<()>) -> Self {
            Self {
                sender: Some(sender),
            }
        }

        /// Notify the sender this contains (this is equivalent to dropping the DropNotifier).
        pub fn notify_now(self) {
            drop(self)
        }

        /// Drop this DropNotifier without notifying. This is useful in cases where "defusing" the
        /// DropNotifier is desired.
        pub fn cancel(mut self) {
            self.sender.take();
        }
    }

    impl Drop for DropNotifier {
        fn drop(&mut self) {
            if let Some(sender) = self.sender.as_ref() {
                let _ = sender.try_send(());
            }
        }
    }
}

mod connection_with_extra {
    use std::{
        io,
        pin::Pin,
        task::{Context, Poll},
    };

    use pin_project::pin_project;
    use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};
    use tonic::transport::server::Connected;

    use super::DropNotifier;

    /// A connection (AsyncWrite + AsyncRead + Connected), with a field for a DropNotifier. This
    /// acts a connection, but lets us observe when it's dropped.
    #[pin_project]
    pub struct ConnectionWithExtra<I> {
        #[pin]
        pub inner: I,
        pub notifier: DropNotifier,
    }

    impl<I> AsyncWrite for ConnectionWithExtra<I>
    where
        I: AsyncWrite,
    {
        fn poll_write(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &[u8],
        ) -> Poll<Result<usize, io::Error>> {
            self.project().inner.poll_write(cx, buf)
        }

        fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), io::Error>> {
            self.project().inner.poll_flush(cx)
        }

        fn poll_shutdown(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
        ) -> Poll<Result<(), io::Error>> {
            self.project().inner.poll_shutdown(cx)
        }
    }

    impl<I> AsyncRead for ConnectionWithExtra<I>
    where
        I: AsyncRead,
    {
        fn poll_read(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
            self.project().inner.poll_read(cx, buf)
        }
    }

    impl<I> Connected for ConnectionWithExtra<I>
    where
        I: Connected,
    {
        fn remote_addr(&self) -> Option<std::net::SocketAddr> {
            self.inner.remote_addr()
        }

        fn peer_certs(&self) -> Option<Vec<tonic::transport::Certificate>> {
            self.inner.peer_certs()
        }
    }
}
