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
use hyper::Request;
use hyper::Response;
use hyper_proxy::Proxy;
use hyper_proxy::ProxyConnector;
use hyper_rustls::HttpsConnectorBuilder;
use tokio_rustls::TlsConnector;

use crate::http::secure_client::SecureHttpClient;
use crate::http::tls_config_with_system_roots;
use crate::http::HttpClient;
use crate::http::HttpError;
use crate::http::DEFAULT_MAX_REDIRECTS;

#[derive(Allocative)]
pub(super) struct SecureProxiedClient {
    inner: SecureHttpClient,
}

impl SecureProxiedClient {
    pub(super) fn with_proxies<I: IntoIterator<Item = Proxy>>(proxies: I) -> anyhow::Result<Self> {
        let config = tls_config_with_system_roots()?;

        // This connector establishes a secure connection from client -> dest
        let https_connector = HttpsConnectorBuilder::new()
            .with_tls_config(config.clone())
            .https_or_http()
            .enable_http1()
            .enable_http2()
            .build();

        // Note: we use the `unsecured()` constructor here, but all that does is not load TLS config. We pass our own TLS config below:
        let mut proxy_connector = ProxyConnector::unsecured(https_connector);

        // This connector wraps the above and _also_ establishes a secure connection to
        // the proxy, re-using the same TLS config for the above connector.
        //
        // The net effect is that we can establish a secure connection to the proxy and
        // have that tunnel our secure connection to the destination.

        proxy_connector.set_tls(Some(TlsConnector::from(Arc::new(config))));
        proxy_connector.extend_proxies(proxies);

        Ok(Self {
            inner: SecureHttpClient::with_connector(proxy_connector, DEFAULT_MAX_REDIRECTS),
        })
    }
}

#[async_trait::async_trait]
impl HttpClient for SecureProxiedClient {
    async fn request(
        &self,
        request: Request<Bytes>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        self.inner.request(request).await
    }
}

#[cfg(test)]
#[cfg(any(fbcode_build, cargo_internal_build))]
mod tests {
    use anyhow::Context as _;
    use httptest::matchers::*;
    use httptest::responders;
    use httptest::Expectation;
    use hyper::Body;

    use super::*;

    /// Barebones proxy server implementation that simply forwards requests onto
    /// the destination server.
    struct ProxyServer {
        addr: std::net::SocketAddr,
        // Need to hold a ref to the task so when Drop runs on Self we cancel
        // the task.
        #[allow(dead_code)]
        handle: tokio::task::JoinHandle<()>,
    }

    impl ProxyServer {
        async fn new() -> anyhow::Result<Self> {
            use std::convert::Infallible;
            use std::net::TcpListener;
            use std::net::ToSocketAddrs;

            use hyper::service::make_service_fn;
            use hyper::service::service_fn;
            use hyper::Server;

            let proxy_server_addr = "[::1]:0".to_socket_addrs().unwrap().next().unwrap();
            let listener =
                TcpListener::bind(proxy_server_addr).context("failed to bind to local address")?;
            let proxy_server_addr = listener.local_addr()?;

            let make_proxy_service = make_service_fn(|_conn| async move {
                Ok::<_, Infallible>(service_fn(|mut req: Request<Body>| async move {
                    let client = hyper::Client::new();
                    req.headers_mut().insert(
                        http::header::VIA,
                        http::HeaderValue::from_static("testing-proxy-server"),
                    );
                    println!("Proxying request: {:?}", req);
                    client
                        .request(req)
                        .await
                        .context("Failed sending requeest to destination")
                }))
            });

            let handle = tokio::task::spawn(async move {
                println!("started proxy server");
                Server::from_tcp(listener)
                    .unwrap()
                    .serve(make_proxy_service)
                    .await
                    .expect("Proxy server exited unexpectedly");
            });

            Ok(Self {
                addr: proxy_server_addr,
                handle,
            })
        }

        fn uri(&self) -> anyhow::Result<http::Uri> {
            http::Uri::builder()
                .scheme("http")
                .authority(self.addr.to_string().as_str())
                .path_and_query("/")
                .build()
                .context("failed to build proxy server URI")
        }
    }

    #[tokio::test]
    async fn test_uses_http_proxy() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(all_of![
                request::method_path("GET", "/foo"),
                request::headers(contains(("via", "testing-proxy-server")))
            ])
            .times(1)
            .respond_with(responders::status_code(200)),
        );

        let proxy_server = ProxyServer::new().await?;
        println!("proxy_server uri: {}", proxy_server.uri()?);

        let client = SecureProxiedClient::with_proxies([Proxy::new(
            hyper_proxy::Intercept::Http,
            proxy_server.uri()?,
        )])?;
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_uses_http_proxy_with_no_scheme_in_proxy_uri() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(all_of![
                request::method_path("GET", "/foo"),
                request::headers(contains(("via", "testing-proxy-server")))
            ])
            .times(1)
            .respond_with(responders::status_code(200)),
        );

        let proxy_server = ProxyServer::new().await?;

        let authority = proxy_server.uri()?.authority().unwrap().clone();
        let proxy_uri = format!("{}:{}", authority.host(), authority.port().unwrap());
        println!("proxy_uri: {}", proxy_uri);
        let client = SecureProxiedClient::with_proxies([Proxy::new(
            hyper_proxy::Intercept::Http,
            crate::http::proxy::DefaultSchemeUri(proxy_uri.try_into()?).into(),
        )])?;
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    #[cfg(any(fbcode_build, cargo_internal_build))] // TODO(@skarlage): Debug why this fails on CircleCI
    async fn test_does_not_proxy_when_no_proxy_matches() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(all_of![request::method_path("GET", "/foo")])
                .times(1)
                .respond_with(responders::status_code(200)),
        );

        let proxy_server = ProxyServer::new().await?;
        println!("proxy_server uri: {}", proxy_server.uri()?);

        let test_server_host = test_server
            .url("/")
            .authority()
            .unwrap()
            .clone()
            .host()
            .to_owned();
        let no_proxy = crate::http::proxy::NoProxy::new(http::uri::Scheme::HTTP, test_server_host);

        // Don't proxy connections to test_server.
        let client = SecureProxiedClient::with_proxies([Proxy::new(
            no_proxy.into_proxy_intercept(),
            proxy_server.uri()?,
        )])?;
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_proxies_when_no_proxy_does_not_match() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(all_of![
                request::method_path("GET", "/foo"),
                request::headers(contains(("via", "testing-proxy-server")))
            ])
            .times(1)
            .respond_with(responders::status_code(200)),
        );

        let proxy_server = ProxyServer::new().await?;
        println!("proxy_server uri: {}", proxy_server.uri()?);

        // Don't proxy HTTPS connections to *.foobar.com
        let no_proxy = crate::http::proxy::NoProxy::new(http::uri::Scheme::HTTP, ".foobar.com");

        let client = SecureProxiedClient::with_proxies([Proxy::new(
            no_proxy.into_proxy_intercept(),
            proxy_server.uri()?,
        )])?;
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }
}
