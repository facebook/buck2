/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;

use allocative::Allocative;
use anyhow::Context;
use bytes::Bytes;
use futures::stream::BoxStream;
use http::uri::Scheme;
use http::Uri;
use hyper::Request;
use hyper::Response;
use hyper_proxy::Intercept;
use hyper_proxy::Proxy;
use hyper_proxy::ProxyConnector;

use crate::http::HttpClient;
use crate::http::HttpError;
use crate::http::SecureHttpClient;
use crate::http::SecureProxiedClient;
use crate::http::DEFAULT_MAX_REDIRECTS;

/// x2pagent proxies only speak plain HTTP, so we need to mutate requests prior
/// to sending them off.
fn change_scheme_to_http(request: &mut Request<Bytes>) {
    let uri = request.uri().clone();
    let mut parts = uri.into_parts();
    parts.scheme = Some(Scheme::HTTP);
    *request.uri_mut() = Uri::from_parts(parts).expect("Unexpected invalid URI from request");
}

#[derive(Allocative)]
pub(super) struct X2PAgentProxyClient {
    inner: SecureProxiedClient,
}

impl X2PAgentProxyClient {
    pub(super) fn new(http1_proxy_port: u16) -> anyhow::Result<Self> {
        let proxy = Proxy::new(
            Intercept::All,
            format!("http://localhost:{}", http1_proxy_port)
                .try_into()
                .context("Error converting x2pagent proxy address into URI")?,
        );
        let client = SecureProxiedClient::with_proxies([proxy])?;
        Ok(Self { inner: client })
    }

    async fn request_impl(
        &self,
        mut request: Request<Bytes>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        change_scheme_to_http(&mut request);
        self.inner.request(request).await
    }
}

#[async_trait::async_trait]
impl HttpClient for X2PAgentProxyClient {
    async fn request(
        &self,
        request: Request<Bytes>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        self.request_impl(request).await
    }

    fn supports_vpnless(&self) -> bool {
        true
    }
}

// TODO(skarlage): Remove this client once x2pagent http proxy is deployed and works.
#[cfg(unix)]
mod unix {
    use hyper_unix_connector::UnixClient;

    use super::*;

    /// Wraps a secure client so that we can talk to the x2pagent-provided unix socket
    /// proxy server.
    ///
    /// This server has a few requirements that are reflected in the methods below:
    /// 1. Requests are http-only. The proxy server handles TLS encapsulation + picking
    ///    correct certs.
    /// 2. Requests must be sent to a unix domain socket
    #[derive(Allocative)]
    pub struct X2PAgentUnixSocketClient {
        inner: SecureHttpClient,
    }

    impl X2PAgentUnixSocketClient {
        pub fn new<P: AsRef<Path>>(socket_path: P) -> anyhow::Result<Self> {
            let proxy = Proxy::new(
                Intercept::All,
                hyper_unix_connector::Uri::new(socket_path, "/").into(),
            );
            let proxy_connector = ProxyConnector::from_proxy(UnixClient, proxy)
                .context("Failed to create proxy connector to unix domain scoket")?;
            Ok(Self {
                inner: SecureHttpClient::with_connector(proxy_connector, DEFAULT_MAX_REDIRECTS),
            })
        }

        async fn request_impl(
            &self,
            mut request: Request<Bytes>,
        ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
            change_scheme_to_http(&mut request);
            self.inner.request(request).await
        }
    }

    #[async_trait::async_trait]
    impl HttpClient for X2PAgentUnixSocketClient {
        async fn request(
            &self,
            request: Request<Bytes>,
        ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
            self.request_impl(request).await
        }

        fn supports_vpnless(&self) -> bool {
            true
        }
    }
}

#[cfg(unix)]
pub(super) use unix::X2PAgentUnixSocketClient;

#[cfg(test)]
mod tests {
    use http::Method;

    use super::*;

    #[test]
    fn test_change_scheme_to_http_succeeds() -> anyhow::Result<()> {
        let mut request = Request::builder()
            .method(Method::GET)
            .uri("https://some.site/foo")
            .body(Bytes::new())?;
        change_scheme_to_http(&mut request);

        assert_eq!(
            Scheme::HTTP,
            *request
                .uri()
                .scheme()
                .expect("should have scheme after mutating request")
        );
        Ok(())
    }

    #[test]
    fn test_change_scheme_to_http_no_effect() -> anyhow::Result<()> {
        let uri: Uri = "http://some.site/foo".try_into()?;
        let mut request = Request::builder()
            .method(Method::GET)
            .uri(uri.clone())
            .body(Bytes::new())?;
        change_scheme_to_http(&mut request);

        assert_eq!(&uri, request.uri());
        Ok(())
    }
}

#[cfg(all(test, unix))]
mod unix_tests {
    use std::convert::Infallible;
    use std::path::PathBuf;

    use http::HeaderValue;
    use httptest::matchers::*;
    use httptest::responders;
    use httptest::Expectation;
    use hyper::service::make_service_fn;
    use hyper::service::service_fn;
    use hyper::Body;
    use hyper::Server;
    use hyper_unix_connector::UnixConnector;
    use tokio::task::JoinHandle;

    use super::*;

    /// Conceptually similar to crate::http::tests::ProxyServer, but sets up a
    /// local unix domain socket instead.
    struct UnixSocketProxyServer {
        pub socket: PathBuf,
        // Need to hold a ref so when Drop runs on Self we cancel the task.
        #[allow(dead_code)]
        handle: JoinHandle<()>,
        // Need to hold ref so socket doesn't get removed.
        #[allow(dead_code)]
        tempdir: tempfile::TempDir,
    }

    impl UnixSocketProxyServer {
        async fn new() -> anyhow::Result<Self> {
            let tempdir = tempfile::tempdir()?;
            let socket = tempdir.path().join("test-uds.sock");

            let listener: UnixConnector = tokio::net::UnixListener::bind(&socket)
                .context("binding to unix socket")?
                .into();
            let handler_func = make_service_fn(|_conn| async move {
                Ok::<_, Infallible>(service_fn(|mut req: Request<Body>| async move {
                    let client = hyper::Client::new();
                    req.headers_mut().insert(
                        http::header::VIA,
                        HeaderValue::from_static("testing-proxy-server"),
                    );
                    println!("Proxying request: {:?}", req);
                    client
                        .request(req.map(Body::from))
                        .await
                        .context("Failed sending requeest to destination")
                }))
            });

            let handle = tokio::task::spawn(async move {
                println!("started proxy server");
                Server::builder(listener)
                    .serve(handler_func)
                    .await
                    .expect("Proxy server exited unexpectedly");
            });

            Ok(Self {
                socket,
                handle,
                tempdir,
            })
        }
    }

    #[tokio::test]
    async fn test_proxy_mutates_request_and_succeeds() -> anyhow::Result<()> {
        let proxy_server = UnixSocketProxyServer::new().await?;

        let test_server = httptest::Server::run();
        let url = test_server.url("/foo");
        let host = url.authority().unwrap().to_string();
        test_server.expect(
            Expectation::matching(all_of![
                request::method_path("GET", "/foo"),
                request::headers(contains(("via", "testing-proxy-server"))),
                request::headers(contains(("host", host))),
            ])
            .times(1)
            .respond_with(responders::status_code(200)),
        );

        let client = X2PAgentUnixSocketClient::new(&proxy_server.socket)?;
        let resp = client.get(&url.to_string()).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }
}
