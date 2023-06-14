/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use bytes::Bytes;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use futures::TryStreamExt;
use hyper::client::connect::Connect;
use hyper::Body;
use hyper::Request;
use hyper::Response;
use hyper_rustls::HttpsConnectorBuilder;
use rustls::ClientConfig;
use tokio::io::AsyncReadExt;
use tokio_util::io::StreamReader;

use crate::http::redirect::PendingRequest;
use crate::http::redirect::RedirectEngine;
use crate::http::HttpClient;
use crate::http::HttpError;
use crate::http::RequestClient;

/// A simple client that can make requests to HTTPS or HTTP endpoints. Handles
/// redirects (up to max_redirects).
#[derive(Allocative)]
pub(super) struct SecureHttpClient {
    // hyper::Client doesn't implement Allocative.
    #[allocative(skip)]
    inner: Arc<dyn RequestClient>,
    max_redirects: usize,
}

impl SecureHttpClient {
    /// Constructs a client that uses default system roots to setup TLS.
    pub(super) fn new(tls_config: ClientConfig, max_redirects: usize) -> Self {
        let connector = HttpsConnectorBuilder::new()
            .with_tls_config(tls_config)
            .https_or_http()
            .enable_http1()
            .enable_http2()
            .build();
        Self::with_connector(connector, max_redirects)
    }

    pub(super) fn with_connector<C: Connect + Clone + Send + Sync + 'static>(
        connector: C,
        max_redirects: usize,
    ) -> Self {
        Self {
            inner: Arc::new(hyper::Client::builder().build::<_, Body>(connector)),
            max_redirects,
        }
    }

    async fn send_request_impl(
        &self,
        request: Request<Bytes>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        self.inner
            .request(request)
            .await
            .map_err(HttpError::SendRequest)
            .map(|res| res.map(|body| body.boxed()))
    }
}

#[async_trait::async_trait]
impl HttpClient for SecureHttpClient {
    async fn request(
        &self,
        request: Request<Bytes>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        let pending_request = PendingRequest::from_request(&request);
        let uri = request.uri().to_string();
        tracing::debug!("http: request: {:?}", request);
        let resp = self.send_request_impl(request).await?;
        tracing::debug!("http: response: {:?}", resp.status());

        // Handle redirects up to self.max_redirects times.
        let redirect_engine = RedirectEngine::new(self.max_redirects, pending_request, resp);
        let resp = redirect_engine
            .handle_redirects(|req| self.send_request_impl(req))
            .await?;

        if !resp.status().is_success() {
            let status = resp.status();
            let text = read_truncated_error_response(resp).await;
            return Err(HttpError::Status { status, uri, text });
        }

        Ok(resp)
    }
}

async fn read_truncated_error_response(
    mut resp: Response<BoxStream<'_, hyper::Result<Bytes>>>,
) -> String {
    let read = StreamReader::new(
        resp.body_mut()
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e)),
    );
    let mut buf = Vec::with_capacity(1024);
    read.take(1024).read_to_end(&mut buf).await.map_or_else(
        |e| format!("Error decoding response: {:?}", e),
        |_| String::from_utf8_lossy(buf.as_ref()).into_owned(),
    )
}

#[cfg(test)]
mod tests {
    use httptest::matchers::*;
    use httptest::responders;
    use httptest::Expectation;
    use hyper::StatusCode;

    use super::*;
    use crate::http::tls_config_with_system_roots;

    #[tokio::test]
    async fn test_simple_get_success() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/foo"))
                .respond_with(responders::status_code(200)),
        );

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 10);
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_simple_put_success() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(all_of![
                request::method_path("PUT", "/foo"),
                request::body("Hello, world!")
            ])
            .respond_with(responders::status_code(200)),
        );

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 1);
        let bytes = Bytes::from_static(b"Hello, world!");
        let resp = client
            .put(
                &test_server.url_str("/foo"),
                bytes,
                vec![("key".to_owned(), "value".to_owned())],
            )
            .await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_simple_post_success() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(all_of![
                request::method_path("POST", "/foo"),
                request::body("Hello, world!")
            ])
            .respond_with(responders::status_code(200)),
        );

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 1);
        let bytes = Bytes::from_static(b"Hello, world!");
        let resp = client
            .post(
                &test_server.url_str("/foo"),
                bytes,
                vec![("key".to_owned(), "value".to_owned())],
            )
            .await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_404_not_found_is_error() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/foo"))
                .respond_with(responders::status_code(404)),
        );

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 10);
        let url = test_server.url_str("/foo");
        let result = client.get(&url).await;
        assert!(result.is_err());
        if let HttpError::Status { status, uri, text } = result.as_ref().err().unwrap() {
            assert_eq!(StatusCode::NOT_FOUND, *status);
            assert_eq!(url.to_owned(), *uri);
            assert!(text.is_empty());
        } else {
            unreachable!(
                "Expected HttpError::Status, got {:?}",
                result.err().unwrap()
            );
        }

        Ok(())
    }
}
