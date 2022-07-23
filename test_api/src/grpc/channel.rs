use std::convert::TryFrom;
use std::io;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use anyhow::Context as _;
use futures::future;
use pin_project::pin_project;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tokio::io::ReadBuf;
use tonic::transport::server::Connected;
use tonic::transport::Channel;
use tonic::transport::Endpoint;
use tonic::transport::Uri;
use tower::service_fn;

#[pin_project]
pub struct DuplexChannel<R, W> {
    #[pin]
    read: R,
    #[pin]
    write: W,
}

impl<R, W> DuplexChannel<R, W> {
    pub fn new(read: R, write: W) -> Self {
        Self { read, write }
    }
}

impl<R, W> AsyncWrite for DuplexChannel<R, W>
where
    W: AsyncWrite,
{
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, io::Error>> {
        self.project().write.poll_write(cx, buf)
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), io::Error>> {
        self.project().write.poll_flush(cx)
    }

    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), io::Error>> {
        self.project().write.poll_shutdown(cx)
    }
}

impl<R, W> AsyncRead for DuplexChannel<R, W>
where
    R: AsyncRead,
{
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        self.project().read.poll_read(cx, buf)
    }
}

impl<R, W> Connected for DuplexChannel<R, W> {
    type ConnectInfo = ();

    fn connect_info(&self) {}
}

/// Create a channel using a pre-existing I/O instance. This will not support reconnecting since
/// there is no way to establish connections here. We're just using one that already exists.
pub async fn make_channel<T>(io: T, name: &str) -> anyhow::Result<Channel>
where
    T: AsyncRead + AsyncWrite + Send + Sync + Unpin + 'static,
{
    let mut io = Some(io);

    // NOTE: The uri here is only used to populate the requests we send. We don't actually connect
    // anywhere since we already have an I/O channel on hand.
    let channel = Endpoint::try_from(format!("http://{}.invalid", name))
        .context("Invalid endpoint")?
        .connect_with_connector(service_fn(move |_: Uri| {
            let io = io.take().context("Cannot reconnect after connection loss");
            future::ready(io)
        }))
        .await
        .context("Failed to create channel")?;

    Ok(channel)
}
