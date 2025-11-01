use std::io;
use std::pin::Pin;
use std::sync::Arc;
use std::sync::atomic::Ordering;
use std::task::Context;
use std::task::Poll;

use http::Uri;
use hyper::rt;
use hyper_util::rt::TokioIo;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tokio::io::ReadBuf;
use tower::Service;

use crate::NetworkStatisticsGlobal;
use crate::get_network_stats_global;

/// An IO wrapper that implements AsyncRead and AsyncWrite and increments the
/// global counters
pub struct CountingIo<T> {
    inner: T,
    stats: Arc<NetworkStatisticsGlobal>,
}

impl<T> CountingIo<T> {
    pub fn new(inner: T) -> Self {
        CountingIo {
            inner,
            stats: get_network_stats_global(),
        }
    }
}

impl<T: AsyncRead + Unpin> AsyncRead for CountingIo<T> {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<Result<(), io::Error>> {
        let before = buf.filled().len();
        match Pin::new(&mut self.inner).poll_read(cx, buf) {
            Poll::Ready(Ok(())) => {
                let after = buf.filled().len();
                let bytes_read = after - before;
                if bytes_read > 0 {
                    self.stats.downloaded.fetch_add(
                        i64::try_from(bytes_read).unwrap_or_default(),
                        Ordering::Relaxed,
                    );
                }
                Poll::Ready(Ok(()))
            }
            other => other,
        }
    }
}

impl<T: AsyncWrite + Unpin> AsyncWrite for CountingIo<T> {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, io::Error>> {
        match Pin::new(&mut self.inner).poll_write(cx, buf) {
            Poll::Ready(Ok(bytes_written)) => {
                if bytes_written > 0 {
                    self.stats.uploaded.fetch_add(
                        i64::try_from(bytes_written).unwrap_or_default(),
                        Ordering::Relaxed,
                    );
                }
                Poll::Ready(Ok(bytes_written))
            }
            other => other,
        }
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), io::Error>> {
        Pin::new(&mut self.inner).poll_flush(cx)
    }

    fn poll_shutdown(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Result<(), io::Error>> {
        Pin::new(&mut self.inner).poll_shutdown(cx)
    }
}

/// A connector that wraps the underlying IO device with a CountingIo wrapper
pub struct CountingConnector<C: Clone> {
    inner: C,
}

impl<C: Clone> CountingConnector<C> {
    pub fn new(inner: C) -> Self {
        Self { inner }
    }
}

impl<C: Clone> Service<Uri> for CountingConnector<C>
where
    C: Service<Uri> + Send + 'static,
    C::Response: rt::Read + rt::Write + Send + Unpin + 'static,
    C::Future: Send + 'static,
{
    type Response = TokioIo<CountingIo<TokioIo<C::Response>>>;
    type Error = C::Error;
    type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send>>;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx).map_err(Into::into)
    }

    fn call(&mut self, uri: Uri) -> Self::Future {
        let mut inner = self.inner.clone();

        Box::pin(async move {
            let io = inner.call(uri).await?;
            let io = TokioIo::new(CountingIo::new(TokioIo::new(io)));
            Ok(io)
        })
    }
}
