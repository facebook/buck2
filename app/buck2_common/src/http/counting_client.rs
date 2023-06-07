/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::pin::Pin;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use allocative::Allocative;
use bytes::Bytes;
use dupe::Dupe;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use futures::task::Poll;
use futures::Stream;
use hyper::Request;
use hyper::Response;
use pin_project::pin_project;

use crate::http::HttpClient;
use crate::http::HttpError;

#[derive(Allocative)]
pub struct HttpNetworkStats {
    pub downloaded_bytes: u64,
}

/// Wrapper for the HTTP client, storing collected metrics such as the number
/// of bytes downloaded by the client's GET requests during its entire lifetime.
#[derive(Allocative, Clone, Dupe)]
pub struct CountingHttpClient {
    inner: Arc<dyn HttpClient>,
    bytes_downloaded: Arc<AtomicU64>,
}

impl CountingHttpClient {
    pub fn new(client: Arc<dyn HttpClient>) -> Self {
        Self {
            inner: client,
            bytes_downloaded: Arc::new(AtomicU64::new(0)),
        }
    }

    pub fn get_updated_http_network_stats(&self) -> HttpNetworkStats {
        HttpNetworkStats {
            downloaded_bytes: self.bytes_downloaded.load(Ordering::Relaxed),
        }
    }
}

#[pin_project]
struct CountingStream<S> {
    #[pin]
    inner: S,
    bytes_read: Arc<AtomicU64>,
}

impl<S> CountingStream<S> {
    fn new(stream: S, bytes_read: Arc<AtomicU64>) -> Self {
        Self {
            inner: stream,
            bytes_read,
        }
    }
}

impl<S, E> Stream for CountingStream<S>
where
    S: Stream<Item = Result<Bytes, E>>,
{
    type Item = Result<Bytes, E>;

    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Option<Self::Item>> {
        let mut this = self.project();

        match this.inner.as_mut().poll_next(cx) {
            Poll::Ready(Some(Ok(bytes))) => {
                this.bytes_read
                    .fetch_add(bytes.len() as u64, Ordering::Relaxed);
                Poll::Ready(Some(Ok(bytes)))
            }
            x => x,
        }
    }
}

#[async_trait::async_trait]
impl HttpClient for CountingHttpClient {
    async fn put(
        &self,
        uri: &str,
        body: Bytes,
        headers: Vec<(String, String)>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        Ok(self.inner.put(uri, body, headers).await?)
    }
    async fn request(
        &self,
        request: Request<Bytes>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        let resp = self.inner.request(request).await?;
        let resp = resp.map(|body| CountingStream::new(body, self.bytes_downloaded.dupe()).boxed());
        Ok(resp)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::Ordering;

    use futures::StreamExt;
    use httptest::matchers::*;
    use httptest::responders;
    use httptest::Expectation;

    use super::*;
    use crate::http::tls_config_with_system_roots;
    use crate::http::SecureHttpClient;

    #[tokio::test]
    async fn test_count_response_size() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/foo"))
                .times(2)
                // Response body is 100 bytes in size.
                .respond_with(responders::status_code(200).body(vec![0; 100])),
        );

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 10);
        let counting_http_client = CountingHttpClient::new(Arc::new(client));

        let mut resp = counting_http_client
            .get(&test_server.url_str("/foo"))
            .await?;
        // Consume the stream so we trigger a count.
        while (resp.body_mut().next().await).is_some() {}
        assert_eq!(
            100,
            counting_http_client
                .bytes_downloaded
                .load(Ordering::Relaxed)
        );

        let mut resp = counting_http_client
            .get(&test_server.url_str("/foo"))
            .await?;
        // Consume the stream so we trigger a count.
        while (resp.body_mut().next().await).is_some() {}
        assert_eq!(
            200,
            counting_http_client
                .bytes_downloaded
                .load(Ordering::Relaxed)
        );

        Ok(())
    }
}
