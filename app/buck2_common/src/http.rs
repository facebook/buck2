/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs::File;
use std::future::Future;
use std::io::BufReader;
use std::path::Path;
use std::sync::Arc;

use anyhow::Context;
use gazebo::prelude::VecExt;
use http::HeaderMap;
use http::Method;
use http::Uri;
use hyper::body;
use hyper::client::connect::Connect;
use hyper::client::ResponseFuture;
use hyper::Body;
use hyper::Request;
use hyper::Response;
use hyper::StatusCode;
use hyper_proxy::Proxy;
use hyper_proxy::ProxyConnector;
use hyper_rustls::HttpsConnectorBuilder;
use rustls::Certificate;
use rustls::ClientConfig;
use rustls::PrivateKey;
use rustls::RootCertStore;
use thiserror::Error;
use tokio_rustls::TlsConnector;

/// Support following up to 10 redirects, after which a redirected request will
/// error out.
const DEFAULT_MAX_REDIRECTS: usize = 10;

/// Load the system root certificates into rustls cert store.
fn load_system_root_certs() -> anyhow::Result<RootCertStore> {
    let mut roots = rustls::RootCertStore::empty();
    let native_certs = rustls_native_certs::load_native_certs()
        .context("Error loading system root certificates")?;
    for cert in native_certs {
        let cert = rustls::Certificate(cert.0);
        if let Err(e) = roots.add(&cert) {
            anyhow::bail!("Error loading system certificate in to cert store: {:?}", e);
        }
    }
    Ok(roots)
}

/// Deserialize certificate pair at `cert` and `key` into structures that can
/// be inserted into rustls CertStore.
#[allow(dead_code)]
fn load_cert_pair<P: AsRef<Path>>(
    cert: P,
    key: P,
) -> anyhow::Result<(Vec<Certificate>, PrivateKey)> {
    let cert_file = File::open(cert).context("opening certificate file")?;
    let key_file = File::open(key).context("opening private key file")?;
    let mut cert_reader = BufReader::new(&cert_file);
    let mut key_reader = BufReader::new(&key_file);

    let certs = rustls_pemfile::certs(&mut cert_reader)
        .context("creating PEM from internal certificate and private key")?
        .into_map(Certificate);

    let private_key = rustls_pemfile::pkcs8_private_keys(&mut key_reader)
        .context("reading private key from internal certificate")?
        .pop()
        .ok_or_else(|| {
            anyhow::anyhow!("Expected internal certificate to contain at least one private key")
        })?;
    let key = PrivateKey(private_key);

    Ok((certs, key))
}

fn tls_config_with_system_roots() -> anyhow::Result<ClientConfig> {
    Ok(ClientConfig::builder()
        .with_safe_defaults()
        .with_root_certificates(load_system_root_certs()?)
        .with_no_client_auth())
}

fn http_error_label(status: StatusCode) -> &'static str {
    if status.is_server_error() {
        "Server"
    } else if status.is_client_error() {
        "Client"
    } else {
        "Unknown"
    }
}

#[derive(Debug, Error)]
pub enum HttpError {
    #[error("HTTP URI Error: URI {uri} is malformed: {source:?}")]
    InvalidUri {
        uri: String,
        #[source]
        source: http::uri::InvalidUri,
    },
    #[error("HTTP: Error building request: {0}")]
    BuildRequest(#[from] http::Error),
    #[error("HTTP: Error sending request: {0}")]
    SendRequest(#[from] hyper::Error),
    #[error("HTTP {} Error ({status}) when querying URI: {uri}. Response text: {text}", http_error_label(*.status))]
    Status {
        status: StatusCode,
        uri: String,
        text: String,
    },
    #[error("HTTP Error: Exceeded max redirects ({max_redirects}) while fetching URI: {uri}. ")]
    TooManyRedirects { uri: String, max_redirects: usize },
    #[error("HTTP: Error while processing redirect: {0}")]
    RedirectError(#[from] anyhow::Error),
}

trait UriWithRedirect {
    fn with_redirect(&self, location: &Uri) -> anyhow::Result<Uri>;

    fn is_cross_host(&self, other: &Uri) -> bool;
}

impl UriWithRedirect for Uri {
    /// Converts this Uri into the redirect Uri by combining it with the URI
    /// obtained from the Location header of a response.
    fn with_redirect(&self, location: &Uri) -> anyhow::Result<Uri> {
        let mut redirected = Uri::builder();
        if let Some(scheme) = location.scheme().or_else(|| self.scheme()) {
            redirected = redirected.scheme(scheme.clone());
        };
        if let Some(authority) = location.authority().or_else(|| self.authority()) {
            redirected = redirected.authority(authority.clone());
        }
        if let Some(path_and_query) = location.path_and_query().or_else(|| self.path_and_query()) {
            redirected = redirected.path_and_query(path_and_query.clone());
        }
        redirected.build().context("Building redirected URI")
    }

    /// Returns whether this Uri is the same host as represented by 'other'.
    fn is_cross_host(&self, other: &Uri) -> bool {
        self.host() != other.host() || self.port_u16() != other.port_u16()
    }
}

/// Intermediate type to store aspects of a request we need to mutate during
/// redirect state machine.
/// This is mostly because hyper::Request cannot be cloned.
struct PendingRequest {
    method: Method,
    uri: Uri,
    headers: HeaderMap,
}

impl PendingRequest {
    fn from_request(request: &Request<Body>) -> Self {
        Self {
            method: request.method().clone(),
            uri: request.uri().clone(),
            headers: request.headers().clone(),
        }
    }

    fn to_request(&self) -> anyhow::Result<Request<Body>> {
        let mut builder = Request::builder()
            .method(self.method.clone())
            .uri(self.uri.clone());
        *builder
            .headers_mut()
            .expect("Request builder should not error here") = self.headers.clone();
        // TODO(skarlage): Need to support passing actual body for POST.
        builder
            .body(Body::empty())
            .context("building redirected request")
    }
}

/// A simple state machine that drives following redirects. Much of this is derived
/// from how [`reqwest` handles redirects](https://docs.rs/reqwest/latest/src/reqwest/redirect.rs.html#1-337)
/// as well as the [`follow-redirects`](https://github.com/srijs/rust-follow-redirects) crate.
/// Unfortunately, the latter is abandoned; until and unless it's maintained by someone
/// (preferably the hyper folks), let's roll our own.
struct RedirectEngine {
    processed_redirects: usize,
    max_redirects: usize,
    pending_request: PendingRequest,
    response: Response<Body>,
}

impl RedirectEngine {
    pub fn new(
        max_redirects: usize,
        pending_request: PendingRequest,
        response: Response<Body>,
    ) -> Self {
        Self {
            processed_redirects: 0,
            max_redirects,
            pending_request,
            response,
        }
    }

    /// Handle any redirects we get in the course of sending the request (up to
    /// self.max_redirects).
    pub async fn handle_redirects<S, F>(
        mut self,
        sender_func: S,
    ) -> Result<Response<Body>, HttpError>
    where
        F: Future<Output = Result<Response<Body>, HttpError>>,
        S: Fn(Request<Body>) -> F,
    {
        let initial_uri = self.pending_request.uri.clone();
        loop {
            if self.processed_redirects > self.max_redirects {
                return Err(HttpError::TooManyRedirects {
                    uri: initial_uri.to_string(),
                    max_redirects: self.max_redirects,
                });
            }
            if !self.should_redirect() {
                break;
            }
            tracing::debug!(
                "http: processing redirect request ({}) for {}",
                self.response.status(),
                self.pending_request.uri,
            );

            if let Some(redirect_request) = self
                .update_and_create_request()
                .map_err(HttpError::RedirectError)?
            {
                self.response = sender_func(redirect_request).await?;
                self.processed_redirects += 1;
            } else {
                break;
            }
        }

        Ok(self.response)
    }

    /// Whether we should redirect this response.
    fn should_redirect(&self) -> bool {
        let got_redirect_status_code = matches!(
            self.response.status(),
            StatusCode::MOVED_PERMANENTLY
                | StatusCode::FOUND
                | StatusCode::SEE_OTHER
                | StatusCode::TEMPORARY_REDIRECT
                | StatusCode::PERMANENT_REDIRECT
        );

        got_redirect_status_code && self.extract_redirect_location_from_response().is_some()
    }

    /// Updates the request in place to send to the redirect location.
    fn update_and_create_request(&mut self) -> anyhow::Result<Option<Request<Body>>> {
        let redirect_location =
            if let Some(location) = self.extract_redirect_location_from_response() {
                location
            } else {
                return Ok(None);
            };

        let redirect_uri = self
            .pending_request
            .uri
            .clone()
            .with_redirect(&redirect_location)?;
        let is_cross_host = redirect_uri.is_cross_host(&self.pending_request.uri);
        self.pending_request.uri = redirect_uri;

        if is_cross_host {
            for sensitive_header in &[
                hyper::header::AUTHORIZATION,
                hyper::header::COOKIE,
                hyper::header::PROXY_AUTHORIZATION,
                hyper::header::WWW_AUTHENTICATE,
            ] {
                self.pending_request.headers.remove(sensitive_header);
            }
        }

        match self.response.status() {
            // TODO(skarlage): For POST, need to clear out body here.
            StatusCode::MOVED_PERMANENTLY | StatusCode::FOUND | StatusCode::SEE_OTHER => {
                for sensitive_header in &[
                    hyper::header::TRANSFER_ENCODING,
                    hyper::header::CONTENT_ENCODING,
                    hyper::header::CONTENT_TYPE,
                    hyper::header::CONTENT_LENGTH,
                ] {
                    self.pending_request.headers.remove(sensitive_header);
                }

                if self.pending_request.method == Method::HEAD {
                    self.pending_request.method = Method::GET;
                }
            }
            _ => {}
        }

        Some(self.pending_request.to_request()).transpose()
    }

    /// Extracts location header from the current response and tries to convert it
    /// to a URI.
    fn extract_redirect_location_from_response(&self) -> Option<Uri> {
        self.response
            .headers()
            .get(hyper::header::LOCATION)
            .and_then(|location| Uri::try_from(location.as_bytes()).ok())
    }
}

/// Trait describe http client that can perform simple HEAD and GET requests.
#[async_trait::async_trait]
pub trait HttpClient {
    /// Send a HEAD request. Assumes no body will be returned. If one is returned, it will be ignored.
    async fn head(&self, uri: &str) -> Result<Response<()>, HttpError> {
        let req = Request::builder()
            .uri(uri)
            .method(Method::HEAD)
            .body(Body::empty())
            .map_err(HttpError::BuildRequest)?;
        self.request(req).await.map(|resp| resp.map(|_| ()))
    }

    /// Send a GET request.
    async fn get(&self, uri: &str) -> Result<Response<Body>, HttpError> {
        let req = Request::builder()
            .uri(uri)
            .method(Method::GET)
            .body(Body::empty())
            .map_err(HttpError::BuildRequest)?;
        self.request(req).await
    }

    /// Send a generic request.
    async fn request(&self, request: Request<Body>) -> Result<Response<Body>, HttpError>;
}

/// Trait wrapper around a hyper::Client because hyper::Client is parameterized by
/// the connector. At runtime, we want to pick different connectors (e.g. HttpsConnector,
/// ProxyConnector<HttpsConnector<..>>, etc); thus wrap the client so we can switch
/// out the concrete type without exposing implementation details to callers.
trait RequestClient: Send + Sync {
    fn request(&self, request: Request<Body>) -> ResponseFuture;
}

impl<C> RequestClient for hyper::Client<C>
where
    C: Connect + Clone + Send + Sync + 'static,
{
    fn request(&self, request: Request<Body>) -> ResponseFuture {
        self.request(request)
    }
}

/// A simple client that can make requests to HTTPS or HTTP endpoints. Handles
/// redirects (up to max_redirects).
pub struct SecureHttpClient {
    inner: Arc<dyn RequestClient>,
    max_redirects: usize,
}

impl SecureHttpClient {
    /// Constructs a client that uses default system roots to setup TLS.
    pub fn new() -> anyhow::Result<Self> {
        let config = tls_config_with_system_roots()?;
        Ok(Self::configure(config, DEFAULT_MAX_REDIRECTS))
    }

    pub fn with_max_redirects(max_redirects: usize) -> anyhow::Result<Self> {
        let config = tls_config_with_system_roots()?;
        Ok(Self::configure(config, max_redirects))
    }

    pub fn with_connector<C: Connect + Clone + Send + Sync + 'static>(
        connector: C,
        max_redirects: usize,
    ) -> Self {
        Self {
            inner: Arc::new(hyper::Client::builder().build::<_, Body>(connector)),
            max_redirects,
        }
    }

    fn configure(tls_config: ClientConfig, max_redirects: usize) -> Self {
        let connector = HttpsConnectorBuilder::new()
            .with_tls_config(tls_config)
            .https_or_http()
            .enable_http1()
            .enable_http2()
            .build();
        Self::with_connector(connector, max_redirects)
    }

    async fn send_request_impl(&self, request: Request<Body>) -> Result<Response<Body>, HttpError> {
        self.inner
            .request(request)
            .await
            .map_err(HttpError::SendRequest)
    }
}

#[async_trait::async_trait]
impl HttpClient for SecureHttpClient {
    async fn request(&self, request: Request<Body>) -> Result<Response<Body>, HttpError> {
        let pending_request = PendingRequest::from_request(&request);
        let uri = request.uri().to_string();
        tracing::debug!("http: request: {:?}", request);
        let resp = self.send_request_impl(request).await?;
        tracing::debug!("http: response: {:?}", resp);

        // Handle redirects up to self.max_redirects times.
        let redirect_engine = RedirectEngine::new(self.max_redirects, pending_request, resp);
        let resp = redirect_engine
            .handle_redirects(|req| self.send_request_impl(req))
            .await?;

        if !resp.status().is_success() {
            let status = resp.status();
            // TODO(skarlage): This is unsafe; malicious URIs could cause callers
            // to OOM by sending huge responses. We should limit this.
            let text = body::to_bytes(resp)
                .await
                .context("Reading all bytes from response body")
                .and_then(|bytes| {
                    String::from_utf8(bytes.to_vec()).context("Converting response bytes to string")
                })
                .unwrap_or_else(|e| format!("Error decoding response: {:?}", e));

            return Err(HttpError::Status { status, uri, text });
        }

        Ok(resp)
    }
}

pub struct SecureProxiedClient {
    inner: SecureHttpClient,
}

impl SecureProxiedClient {
    pub fn new(proxy: Proxy) -> anyhow::Result<Self> {
        Self::with_proxies([proxy])
    }

    pub fn with_proxies<I: IntoIterator<Item = Proxy>>(proxies: I) -> anyhow::Result<Self> {
        let config = tls_config_with_system_roots()?;

        // This connector establishes a secure connection from client -> dest
        let https_connector = HttpsConnectorBuilder::new()
            .with_tls_config(config.clone())
            .https_or_http()
            .enable_http1()
            .enable_http2()
            .build();

        // This connector wraps the above and _also_ establishes a secure connection to
        // the proxy, re-using the same TLS config for the above connector.
        //
        // The net effect is that we can establish a secure connection to the proxy and
        // have that tunnel our secure connection to the destination.
        let mut proxy_connector = ProxyConnector::new(https_connector)
            .context("Error creating secured proxy connector")?;
        proxy_connector.set_tls(Some(TlsConnector::from(Arc::new(config))));
        proxy_connector.extend_proxies(proxies);

        Ok(Self {
            inner: SecureHttpClient::with_connector(proxy_connector, DEFAULT_MAX_REDIRECTS),
        })
    }
}

#[async_trait::async_trait]
impl HttpClient for SecureProxiedClient {
    async fn request(&self, request: Request<Body>) -> Result<Response<Body>, HttpError> {
        self.inner.request(request).await
    }
}

#[cfg(test)]
mod tests {
    use std::convert::Infallible;
    use std::net::SocketAddr;
    use std::net::TcpListener;
    use std::net::ToSocketAddrs;

    use http::HeaderValue;
    use httptest::matchers::*;
    use httptest::responders;
    use httptest::Expectation;
    use hyper::service::make_service_fn;
    use hyper::service::service_fn;
    use hyper::Server;
    use hyper_proxy::Intercept;
    use tokio::task::JoinHandle;

    use super::*;

    /// Barebones proxy server implementation that simply forwards requests onto
    /// the destination server.
    struct ProxyServer {
        addr: SocketAddr,
        // Need to hold a ref to the task so when Drop runs on Self we cancel
        // the task.
        #[allow(dead_code)]
        handle: JoinHandle<()>,
    }

    impl ProxyServer {
        async fn new() -> anyhow::Result<Self> {
            let proxy_server_addr = "[::1]:0".to_socket_addrs().unwrap().next().unwrap();
            let listener =
                TcpListener::bind(proxy_server_addr).context("failed to bind to local address")?;
            let proxy_server_addr = listener.local_addr()?;

            let make_proxy_service = make_service_fn(|_conn| async move {
                Ok::<_, Infallible>(service_fn(|mut req: Request<Body>| async move {
                    let client = hyper::Client::new();
                    req.headers_mut().insert(
                        http::header::VIA,
                        HeaderValue::from_static("testing-proxy-server"),
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

        fn uri(&self) -> anyhow::Result<Uri> {
            Uri::builder()
                .scheme("http")
                .authority(self.addr.to_string().as_str())
                .path_and_query("/")
                .build()
                .context("failed to build proxy server URI")
        }
    }

    #[tokio::test]
    async fn test_simple_get_success() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/foo"))
                .respond_with(responders::status_code(200)),
        );

        let client = SecureHttpClient::new()?;
        let resp = client.get(&test_server.url_str("/foo")).await?;
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

        let client = SecureHttpClient::new()?;
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
    async fn test_follows_redirects() -> anyhow::Result<()> {
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

        let client = SecureHttpClient::new()?;
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_head_changes_to_get_on_redirect() -> anyhow::Result<()> {
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

        let client = SecureHttpClient::new()?;
        let resp = client.head(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_too_many_redirects_fails() -> anyhow::Result<()> {
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

        let client = SecureHttpClient::with_max_redirects(1)?;
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

        let client = SecureProxiedClient::new(Proxy::new(Intercept::Http, proxy_server.uri()?))?;
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }
}
