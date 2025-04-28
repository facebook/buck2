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
use buck2_error::BuckErrorContext;
use bytes::Bytes;
use dupe::Dupe;
use futures::StreamExt;
use futures::TryStreamExt;
use futures::stream::BoxStream;
use http::Method;
use http::Uri;
use http::request::Builder;
use http::uri::Scheme;
use hyper::Body;
use hyper::Request;
use hyper::Response;
use hyper::client::ResponseFuture;
use hyper::client::connect::Connect;
use tokio::io::AsyncReadExt;
use tokio_util::io::StreamReader;

use crate::HttpError;
use crate::redirect::PendingRequest;
use crate::redirect::RedirectEngine;
use crate::stats::CountingStream;
use crate::stats::HttpNetworkStats;
use crate::x2p::X2PAgentError;

mod builder;
pub use builder::HttpClientBuilder;

const DEFAULT_USER_AGENT: &str = "Buck2";

#[derive(Allocative, Clone, Dupe)]
pub struct HttpClient {
    // hyper::Client doesn't impl Allocative.
    #[allocative(skip)]
    inner: Arc<dyn RequestClient>,
    max_redirects: Option<usize>,
    supports_vpnless: bool,
    http2: bool,
    stats: HttpNetworkStats,
}

impl HttpClient {
    fn request_builder(&self, uri: &str) -> Builder {
        Request::builder()
            .uri(uri)
            .header(http::header::USER_AGENT, DEFAULT_USER_AGENT)
    }

    /// Send a HEAD request. Assumes no body will be returned. If one is returned, it will be ignored.
    pub async fn head(&self, uri: &str) -> Result<Response<()>, HttpError> {
        let req = self
            .request_builder(uri)
            .method(Method::HEAD)
            .body(Bytes::new())
            .map_err(HttpError::BuildRequest)?;
        self.request(req).await.map(|resp| resp.map(|_| ()))
    }

    /// Send a GET request.
    pub async fn get(
        &self,
        uri: &str,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        let req = self
            .request_builder(uri)
            .method(Method::GET)
            .body(Bytes::new())
            .map_err(HttpError::BuildRequest)?;
        self.request(req).await
    }

    pub async fn post(
        &self,
        uri: &str,
        body: Bytes,
        headers: Vec<(String, String)>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        let mut builder = self.request_builder(uri).method(Method::POST);
        for (name, value) in headers {
            builder = builder.header(name, value);
        }
        let req = builder.body(body).map_err(HttpError::BuildRequest)?;
        self.request(req).await
    }

    pub async fn put(
        &self,
        uri: &str,
        body: Bytes,
        headers: Vec<(String, String)>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        let mut builder = self.request_builder(uri).method(Method::PUT);
        for (name, value) in headers {
            builder = builder.header(name, value);
        }
        let req = builder.body(body).map_err(HttpError::BuildRequest)?;
        self.request(req).await
    }

    async fn send_request_impl(
        &self,
        mut request: Request<Bytes>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        let uri = request.uri().to_string();
        let now = tokio::time::Instant::now();

        // x2p requires scheme to be http since it handles all TLS.
        if self.supports_vpnless() {
            tracing::debug!(
                "http: request: changing scheme for '{}' to http for vpnless",
                request.uri()
            );
            change_scheme_to_http(&mut request)?;
        }
        let resp = self.inner.request(request).await.map_err(|e| {
            if is_hyper_error_due_to_timeout(&e) {
                HttpError::Timeout {
                    uri,
                    duration: now.elapsed().as_secs(),
                }
            } else {
                HttpError::SendRequest { uri, source: e }
            }
        })?;
        Ok(
            resp.map(|body| {
                CountingStream::new(body, self.stats.downloaded_bytes().dupe()).boxed()
            }),
        )
    }

    /// Send a generic request.
    pub async fn request(
        &self,
        request: Request<Bytes>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        let pending_request = PendingRequest::from_request(&request);
        let uri = request.uri().clone();
        tracing::debug!("http: request: {:?}", request);
        let resp = self.send_request_impl(request).await?;
        tracing::debug!("http: response: {:?}", resp.status());

        // Handle redirects up to self.max_redirects times.
        let resp = if let Some(max_redirects) = self.max_redirects {
            let redirect_engine = RedirectEngine::new(max_redirects, pending_request, resp);
            redirect_engine
                .handle_redirects(|req| self.send_request_impl(req))
                .await?
        } else {
            resp
        };

        if !resp.status().is_success() {
            // Handle x2p errors as indicated by headers.
            if let Some(x2p_err) = X2PAgentError::from_headers(&uri, resp.headers()) {
                return Err(HttpError::X2P {
                    uri: uri.to_string(),
                    source: x2p_err,
                });
            }

            let status = resp.status();
            let text = read_truncated_error_response(resp).await;
            return Err(HttpError::Status {
                status,
                uri: uri.to_string(),
                text,
            });
        }

        Ok(resp)
    }

    pub fn stats(&self) -> &HttpNetworkStats {
        &self.stats
    }

    /// Whether this client supports vpnless operation. When set, will make requests
    /// to the `vpnless_url` attribute in the `download_file` action rather than the
    /// normal `url` attribute.
    pub fn supports_vpnless(&self) -> bool {
        self.supports_vpnless
    }

    pub fn http2(&self) -> bool {
        self.http2
    }
}

/// Trait wrapper around a hyper::Client because hyper::Client is parameterized by
/// the connector. At runtime, we want to pick different connectors (e.g. HttpsConnector,
/// ProxyConnector<HttpsConnector<..>>, etc); thus wrap the client so we can switch
/// out the concrete type without exposing implementation details to callers.
pub(super) trait RequestClient: Send + Sync {
    fn request(&self, request: Request<Bytes>) -> ResponseFuture;
}

impl<C> RequestClient for hyper::Client<C>
where
    C: Connect + Clone + Send + Sync + 'static,
{
    fn request(&self, request: Request<Bytes>) -> ResponseFuture {
        self.request(request.map(Body::from))
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

/// Helper function to consume a response stream and convert it to a Bytes container.
/// Warning: This does no length checking (like hyper::body::to_bytes). Should
/// only be used for trusted endpoints.
pub async fn to_bytes(body: BoxStream<'_, hyper::Result<Bytes>>) -> anyhow::Result<Bytes> {
    let mut reader =
        StreamReader::new(body.map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e)));
    let mut buf = Vec::new();
    reader
        .read_to_end(&mut buf)
        .await
        .buck_error_context_anyhow("Reading response body")?;
    Ok(buf.into())
}

/// x2pagent proxies only speak plain HTTP, so we need to mutate requests prior
/// to sending them off.
fn change_scheme_to_http(request: &mut Request<Bytes>) -> Result<(), HttpError> {
    let uri = request.uri().clone();
    let uri_for_error = uri.clone();
    let mut parts = uri.into_parts();
    parts.scheme = Some(Scheme::HTTP);
    *request.uri_mut() = Uri::from_parts(parts).map_err(|e| HttpError::InvalidUriParts {
        uri: uri_for_error.to_string(),
        source: e,
    })?;
    Ok(())
}

/// Helper function to check if any error in the chain of errors produced by
/// hyper is due to a timeout.
fn is_hyper_error_due_to_timeout(e: &hyper::Error) -> bool {
    use std::error::Error;

    let mut cause = e.source();
    while let Some(err) = cause {
        if let Some(io_err) = err.downcast_ref::<std::io::Error>() {
            if let std::io::ErrorKind::TimedOut = io_err.kind() {
                return true;
            }
        }
        cause = err.source();
    }

    false
}

#[cfg(test)]
mod tests {
    use http::StatusCode;
    use httptest::Expectation;
    use httptest::matchers::*;
    use httptest::responders;

    use super::*;

    #[test]
    fn test_change_scheme_to_http_succeeds() -> buck2_error::Result<()> {
        let mut request = Request::builder()
            .method(Method::GET)
            .uri("https://some.site/foo")
            .body(Bytes::new())?;
        change_scheme_to_http(&mut request)?;

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
    fn test_change_scheme_to_http_no_effect() -> buck2_error::Result<()> {
        let uri: Uri = "http://some.site/foo".try_into()?;
        let mut request = Request::builder()
            .method(Method::GET)
            .uri(uri.clone())
            .body(Bytes::new())?;
        change_scheme_to_http(&mut request)?;

        assert_eq!(&uri, request.uri());
        Ok(())
    }

    #[tokio::test]
    async fn test_simple_get_success() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/foo"))
                .respond_with(responders::status_code(200)),
        );

        let client = HttpClientBuilder::https_with_system_roots().await?.build();
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_simple_put_success() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(all_of![
                request::method_path("PUT", "/foo"),
                request::body("Hello, world!")
            ])
            .respond_with(responders::status_code(200)),
        );

        let client = HttpClientBuilder::https_with_system_roots().await?.build();
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
    async fn test_simple_post_success() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(all_of![
                request::method_path("POST", "/foo"),
                request::body("Hello, world!")
            ])
            .respond_with(responders::status_code(200)),
        );

        let client = HttpClientBuilder::https_with_system_roots().await?.build();
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
    async fn test_404_not_found_is_error() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/foo"))
                .respond_with(responders::status_code(404)),
        );

        let client = HttpClientBuilder::https_with_system_roots().await?.build();
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

    #[tokio::test]
    async fn test_count_response_size() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/foo"))
                .times(2)
                // Response body is 100 bytes in size.
                .respond_with(responders::status_code(200).body(vec![0; 100])),
        );

        let client = HttpClientBuilder::https_with_system_roots().await?.build();
        let mut resp = client.get(&test_server.url_str("/foo")).await?;

        // Consume the stream so we trigger a count.
        while (resp.body_mut().next().await).is_some() {}
        assert_eq!(100, client.stats().get_downloaded_bytes());

        let mut resp = client.get(&test_server.url_str("/foo")).await?;

        // Consume the stream so we trigger a count.
        while (resp.body_mut().next().await).is_some() {}
        assert_eq!(200, client.stats().get_downloaded_bytes());

        Ok(())
    }

    #[tokio::test]
    async fn test_follows_redirects() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        // Chain of two redirects /foo -> /bar -> /baz.
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/foo"))
                .times(1)
                .respond_with(
                    responders::status_code(302).append_header(http::header::LOCATION, "/bar"),
                ),
        );
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/bar"))
                .times(1)
                .respond_with(
                    responders::status_code(302).append_header(http::header::LOCATION, "/baz"),
                ),
        );
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/baz"))
                .times(1)
                .respond_with(responders::status_code(200)),
        );

        let client = HttpClientBuilder::https_with_system_roots()
            .await?
            .with_max_redirects(10)
            .build();
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_head_changes_to_get_on_redirect() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        // Chain of two redirects /foo -> /bar -> /baz.
        test_server.expect(
            Expectation::matching(request::method_path("HEAD", "/foo"))
                .times(1)
                .respond_with(
                    responders::status_code(302).append_header(http::header::LOCATION, "/bar"),
                ),
        );
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/bar"))
                .times(1)
                .respond_with(responders::status_code(200)),
        );

        let client = HttpClientBuilder::https_with_system_roots()
            .await?
            .with_max_redirects(10)
            .build();
        let resp = client.head(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_post_gets_redirected() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        // Redirect /foo -> /bar
        test_server.expect(
            Expectation::matching(all_of![
                request::method_path("POST", "/foo"),
                request::body("Hello, world!"),
            ])
            .times(1)
            .respond_with(
                responders::status_code(307).append_header(http::header::LOCATION, "/bar"),
            ),
        );
        test_server.expect(
            Expectation::matching(all_of![
                request::method_path("POST", "/bar"),
                request::body("Hello, world!"),
                request::headers(not(contains(key(hyper::header::ORIGIN.as_str())))),
                request::headers(not(contains(key(hyper::header::AUTHORIZATION.as_str())))),
                request::headers(not(contains(key(hyper::header::WWW_AUTHENTICATE.as_str())))),
                request::headers(not(contains(key(hyper::header::COOKIE.as_str())))),
                request::headers(not(contains(key(
                    hyper::header::PROXY_AUTHORIZATION.as_str()
                )))),
            ])
            .times(1)
            .respond_with(responders::status_code(200)),
        );

        let client = HttpClientBuilder::https_with_system_roots()
            .await?
            .with_max_redirects(10)
            .build();
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
    async fn test_too_many_redirects_fails() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        // Chain of three redirects /foo -> /bar -> /baz -> /boo.
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/foo"))
                .times(1)
                .respond_with(
                    responders::status_code(302).append_header(http::header::LOCATION, "/bar"),
                ),
        );
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/bar"))
                .times(1)
                .respond_with(
                    responders::status_code(302).append_header(http::header::LOCATION, "/baz"),
                ),
        );
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/baz"))
                .times(1)
                .respond_with(
                    responders::status_code(302).append_header(http::header::LOCATION, "/boo"),
                ),
        );
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/boo"))
                .times(0)
                .respond_with(responders::status_code(200)),
        );

        let client = HttpClientBuilder::https_with_system_roots()
            .await?
            .with_max_redirects(1)
            .build();
        let url = test_server.url_str("/foo");
        let result = client.get(&url).await;
        if let HttpError::TooManyRedirects { uri, max_redirects } = result.as_ref().err().unwrap() {
            assert_eq!(url.to_owned(), *uri);
            assert_eq!(1, *max_redirects);
        } else {
            unreachable!(
                "Expected HttpError::TooManyRedirects, got {:?}",
                result.err().unwrap()
            );
        }

        Ok(())
    }

    #[cfg(unix)]
    mod unix {
        use std::convert::Infallible;
        use std::path::PathBuf;

        use hyper::Server;
        use hyper::service::make_service_fn;
        use hyper::service::service_fn;
        use hyper_unix_connector::UnixConnector;

        use super::*;

        /// Conceptually similar to crate::http::tests::ProxyServer, but sets up a
        /// local unix domain socket instead.
        pub struct UnixSocketProxyServer {
            pub socket: PathBuf,
            // Need to hold a ref so when Drop runs on Self we cancel the task.
            #[allow(dead_code)]
            handle: tokio::task::JoinHandle<()>,
            // Need to hold ref so socket doesn't get removed.
            #[allow(dead_code)]
            tempdir: tempfile::TempDir,
        }

        impl UnixSocketProxyServer {
            pub async fn new() -> buck2_error::Result<Self> {
                let tempdir = tempfile::tempdir()?;
                let socket = tempdir.path().join("test-uds.sock");

                let listener: UnixConnector = tokio::net::UnixListener::bind(&socket)
                    .buck_error_context("binding to unix socket")?
                    .into();
                let handler_func = make_service_fn(|_conn| async move {
                    Ok::<_, Infallible>(service_fn(|mut req: Request<Body>| async move {
                        let client = hyper::Client::new();
                        req.headers_mut().insert(
                            http::header::VIA,
                            http::HeaderValue::from_static("testing-proxy-server"),
                        );
                        println!("Proxying request: {:?}", req);
                        client
                            .request(req.map(Body::from))
                            .await
                            .buck_error_context_anyhow("Failed sending requeest to destination")
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
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_proxies_through_unix_socket_when_set() -> buck2_error::Result<()> {
        let proxy_server = unix::UnixSocketProxyServer::new().await?;

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

        let client = HttpClientBuilder::https_with_system_roots()
            .await?
            .with_x2p_proxy(hyper_proxy::Proxy::new(
                hyper_proxy::Intercept::Http,
                hyper_unix_connector::Uri::new(proxy_server.socket, "/").into(),
            ))
            .build();
        let resp = client.get(&url.to_string()).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_x2p_error_response_is_forbidden_host() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        let url = test_server.url("/foo");
        test_server.expect(
            Expectation::matching(all_of![request::method_path("GET", "/foo")])
                .times(1)
                .respond_with(
                    responders::status_code(400)
                        .append_header("x-x2pagentd-error-type", "FORBIDDEN_HOST")
                        .append_header("x-x2pagentd-error-msg", "Nope"),
                ),
        );

        let client = HttpClientBuilder::https_with_system_roots().await?.build();
        let result = client.get(&url.to_string()).await;
        assert!(result.is_err());
        assert!(matches!(
            result,
            Err(HttpError::X2P {
                source: X2PAgentError::ForbiddenHost { .. },
                ..
            })
        ));

        Ok(())
    }

    #[tokio::test]
    async fn test_x2p_error_response_is_access_denied() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        let url = test_server.url("/foo");
        test_server.expect(
            Expectation::matching(all_of![request::method_path("GET", "/foo")])
                .times(1)
                .respond_with(
                    responders::status_code(400)
                        .append_header("x-fb-validated-x2pauth-decision", "deny")
                        .append_header("x-x2pagentd-error-msg", "Nope"),
                ),
        );

        let client = HttpClientBuilder::https_with_system_roots().await?.build();
        let result = client.get(&url.to_string()).await;
        assert!(result.is_err());
        assert!(matches!(
            result,
            Err(HttpError::X2P {
                source: X2PAgentError::AccessDenied { .. },
                ..
            }),
        ));

        Ok(())
    }

    #[tokio::test]
    async fn test_x2p_error_response_is_generic_error() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        let url = test_server.url("/foo");
        test_server.expect(
            Expectation::matching(all_of![request::method_path("GET", "/foo")])
                .times(1)
                .respond_with(
                    responders::status_code(400)
                        .append_header("x-x2pagentd-error-msg", "Something else happened"),
                ),
        );

        let client = HttpClientBuilder::https_with_system_roots().await?.build();
        let result = client.get(&url.to_string()).await;
        assert!(result.is_err());
        assert!(matches!(
            result,
            Err(HttpError::X2P {
                source: X2PAgentError::Error(..),
                ..
            }),
        ));

        Ok(())
    }
}

// TODO(skarlage, T160529958): Debug why these tests fail on CircleCI
#[cfg(all(test, fbcode_build))]
mod proxy_tests {
    use std::convert::Infallible;
    use std::net::TcpListener;
    use std::net::ToSocketAddrs;
    use std::time::Duration;

    use buck2_error::BuckErrorContext;
    use httptest::Expectation;
    use httptest::matchers::*;
    use httptest::responders;
    use hyper::Server;
    use hyper::service::make_service_fn;
    use hyper::service::service_fn;
    use hyper_proxy::Intercept;
    use hyper_proxy::Proxy;

    use super::*;
    use crate::proxy::DefaultSchemeUri;

    const HEADER_SLEEP_DURATION_MS: &str = "x-buck2-test-proxy-sleep-duration-ms";

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
        async fn new() -> buck2_error::Result<Self> {
            let proxy_server_addr = "[::1]:0".to_socket_addrs().unwrap().next().unwrap();
            let listener = TcpListener::bind(proxy_server_addr)
                .buck_error_context("failed to bind to local address")?;
            let proxy_server_addr = listener.local_addr()?;

            let make_proxy_service = make_service_fn(|_conn| async move {
                Ok::<_, Infallible>(service_fn(|mut req: Request<Body>| async move {
                    // Sleep if requested to simulate slow reads.
                    if let Some(s) = req.headers().get(HEADER_SLEEP_DURATION_MS) {
                        let sleep_duration =
                            Duration::from_millis(s.to_str().unwrap().parse().unwrap());
                        tokio::time::sleep(sleep_duration).await;
                    }

                    let client = hyper::Client::new();
                    req.headers_mut().insert(
                        http::header::VIA,
                        http::HeaderValue::from_static("testing-proxy-server"),
                    );
                    println!("Proxying request: {:?}", req);
                    client
                        .request(req)
                        .await
                        .buck_error_context_anyhow("Failed sending requeest to destination")
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

        fn uri(&self) -> buck2_error::Result<http::Uri> {
            http::Uri::builder()
                .scheme("http")
                .authority(self.addr.to_string().as_str())
                .path_and_query("/")
                .build()
                .buck_error_context("failed to build proxy server URI")
        }
    }

    #[tokio::test]
    async fn test_uses_http_proxy() -> buck2_error::Result<()> {
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

        let client = HttpClientBuilder::https_with_system_roots()
            .await?
            .with_proxy(Proxy::new(Intercept::Http, proxy_server.uri()?))
            .build();
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_uses_http_proxy_with_no_scheme_in_proxy_uri() -> buck2_error::Result<()> {
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
        let client = HttpClientBuilder::https_with_system_roots()
            .await?
            .with_proxy(Proxy::new(
                Intercept::Http,
                DefaultSchemeUri(proxy_uri.try_into()?).into(),
            ))
            .build();
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_does_not_proxy_when_no_proxy_matches() -> buck2_error::Result<()> {
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
        let no_proxy = crate::proxy::NoProxy::new(http::uri::Scheme::HTTP, test_server_host);

        // Don't proxy connections to test_server.
        let client = HttpClientBuilder::https_with_system_roots()
            .await?
            .with_proxy(Proxy::new(
                no_proxy.into_proxy_intercept(),
                proxy_server.uri()?,
            ))
            .build();
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_proxies_when_no_proxy_does_not_match() -> buck2_error::Result<()> {
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
        let no_proxy = crate::proxy::NoProxy::new(http::uri::Scheme::HTTP, ".foobar.com");

        let client = HttpClientBuilder::https_with_system_roots()
            .await?
            .with_proxy(Proxy::new(
                no_proxy.into_proxy_intercept(),
                proxy_server.uri()?,
            ))
            .build();
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    // Use proxy server harness to test slow connections.
    #[tokio::test]
    async fn test_timeout() -> buck2_error::Result<()> {
        let test_server = httptest::Server::run();
        let proxy_server = ProxyServer::new().await?;

        let client = HttpClientBuilder::https_with_system_roots()
            .await?
            .with_proxy(Proxy::new(Intercept::Http, proxy_server.uri()?))
            .with_read_timeout(Some(Duration::from_millis(10)))
            .build();

        let req = Request::builder()
            .uri(test_server.url_str("/foo"))
            .header(HEADER_SLEEP_DURATION_MS, "200")
            .method(Method::GET)
            .body(Bytes::new())?;
        let res = client.request(req).await;
        assert!(matches!(res, Err(HttpError::Timeout { .. })));
        Ok(())
    }
}
