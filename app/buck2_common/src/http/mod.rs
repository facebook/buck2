/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::str::FromStr;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_core::is_open_source;
use dice::UserComputationData;
use dupe::Dupe;
use gazebo::prelude::VecExt;
use http::uri::InvalidUri;
use http::uri::PathAndQuery;
use http::uri::Scheme;
use http::Method;
use http::Uri;
use hyper::body;
use hyper::client::connect::Connect;
use hyper::client::ResponseFuture;
use hyper::Body;
use hyper::Request;
use hyper::Response;
use hyper::StatusCode;
use hyper_proxy::Intercept;
use hyper_proxy::Proxy;
use hyper_proxy::ProxyConnector;
use hyper_rustls::HttpsConnectorBuilder;
use rustls::Certificate;
use rustls::ClientConfig;
use rustls::PrivateKey;
use rustls::RootCertStore;
use thiserror::Error;
use tokio_rustls::TlsConnector;

mod redirect;
use redirect::PendingRequest;
use redirect::RedirectEngine;

#[cfg(unix)]
mod x2p;

/// Support following up to 10 redirects, after which a redirected request will
/// error out.
const DEFAULT_MAX_REDIRECTS: usize = 10;

/// General-purpose function to get a regular HTTP client for use throughout the
/// buck2 codebase.
///
/// This should work for internal and OSS use cases.
pub fn http_client() -> anyhow::Result<Arc<dyn HttpClient>> {
    if is_open_source() {
        http_client_for_oss()
    } else {
        http_client_for_internal()
    }
}

/// Dice implementations so we can pass along the HttpClient to various subsystems
/// that need to use it (Materializer, RunActions, etc).
pub trait HasHttpClient {
    fn get_http_client(&self) -> Arc<dyn HttpClient>;
}

pub trait SetHttpClient {
    fn set_http_client(&mut self, client: Arc<dyn HttpClient>);
}

impl HasHttpClient for UserComputationData {
    fn get_http_client(&self) -> Arc<dyn HttpClient> {
        self.data
            .get::<Arc<dyn HttpClient>>()
            .expect("HttpClient should be set")
            .dupe()
    }
}

impl SetHttpClient for UserComputationData {
    fn set_http_client(&mut self, client: Arc<dyn HttpClient>) {
        self.data.set(client);
    }
}

/// Returns a client suitable for OSS usecases. Supports standard Curl-like
/// proxy environment variables: $HTTP_PROXY, $HTTPS_PROXY.
fn http_client_for_oss() -> anyhow::Result<Arc<dyn HttpClient>> {
    // Add standard proxy variables if defined.
    // Ignores values that cannot be turned into valid URIs.
    let mut proxies = Vec::new();
    if let Some(proxy) = https_proxy_from_env() {
        proxies.push(proxy);
    }
    if let Some(proxy) = http_proxy_from_env() {
        proxies.push(proxy);
    }

    if !proxies.is_empty() {
        Ok(Arc::new(SecureProxiedClient::with_proxies(proxies)?))
    } else {
        let config = tls_config_with_system_roots()?;
        Ok(Arc::new(SecureHttpClient::new(
            config,
            DEFAULT_MAX_REDIRECTS,
        )))
    }
}

/// Returns a client suitable for Meta-internal usecases. Supports standard
/// $THRIFT_TLS_CL_* environment variables.
fn http_client_for_internal() -> anyhow::Result<Arc<dyn HttpClient>> {
    let tls_config = if let (Some(cert_path), Some(key_path)) = (
        std::env::var_os("THRIFT_TLS_CL_CERT_PATH"),
        std::env::var_os("THRIFT_TLS_CL_KEY_PATH"),
    ) {
        tls_config_with_single_cert(cert_path.as_os_str(), key_path.as_os_str())?
    } else {
        tls_config_with_system_roots()?
    };
    Ok(Arc::new(SecureHttpClient::new(
        tls_config,
        DEFAULT_MAX_REDIRECTS,
    )))
}

/// Lookup environment variable and return string value. Checks first for uppercase
/// and falls back to lowercase if unset.
fn env_to_string(env: &'static str) -> Option<String> {
    std::env::var_os(env)
        .or_else(|| std::env::var_os(env.to_lowercase()))
        .and_then(|s| s.into_string().ok())
}

fn https_proxy_from_env() -> Option<Proxy> {
    env_to_string("HTTPS_PROXY")
        .and_then(|https_proxy| https_proxy.parse::<DefaultSchemeUri>().ok())
        .map(|uri| Proxy::new(Intercept::Https, uri.into()))
}

fn http_proxy_from_env() -> Option<Proxy> {
    env_to_string("HTTP_PROXY")
        .and_then(|http_proxy| http_proxy.parse::<DefaultSchemeUri>().ok())
        .map(|uri| Proxy::new(Intercept::Http, uri.into()))
}

/// A wrapped Uri that handles inserting a default scheme (http) if one is not present.
///
/// See https://everything.curl.dev/usingcurl/proxies/type for more information about
/// how curl treats default schemes for e.g. proxy env vars.
struct DefaultSchemeUri(Uri);

impl FromStr for DefaultSchemeUri {
    type Err = InvalidUri;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<Uri>().map(Self)
    }
}

impl From<DefaultSchemeUri> for Uri {
    fn from(default_scheme_uri: DefaultSchemeUri) -> Self {
        let mut parts = default_scheme_uri.0.into_parts();
        if parts.scheme.is_none() {
            parts.scheme = Some(Scheme::HTTP);
        }
        if parts.path_and_query.is_none() {
            parts.path_and_query = Some(PathAndQuery::from_static("/"));
        }
        Uri::from_parts(parts).expect("Got invalid uri from formerly valid uri")
    }
}

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
    let system_roots = load_system_root_certs()?;
    Ok(ClientConfig::builder()
        .with_safe_defaults()
        .with_root_certificates(system_roots)
        .with_no_client_auth())
}

fn tls_config_with_single_cert<P: AsRef<Path>>(
    cert_path: P,
    key_path: P,
) -> anyhow::Result<ClientConfig> {
    let system_roots = load_system_root_certs()?;
    let (cert, key) = load_cert_pair(cert_path, key_path)?;
    ClientConfig::builder()
        .with_safe_defaults()
        .with_root_certificates(system_roots)
        .with_single_cert(cert, key)
        .context("Error creating TLS config with cert and key path")
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
    #[error("HTTP: Error mutating request: {0}")]
    MutateRequest(#[from] anyhow::Error),
    #[error("HTTP: Testing client, http methods not supported")]
    Test,
}

/// Trait describe http client that can perform simple HEAD and GET requests.
#[async_trait::async_trait]
pub trait HttpClient: Allocative + Send + Sync {
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
#[derive(Allocative)]
pub struct SecureHttpClient {
    // hyper::Client doesn't implement Allocative.
    #[allocative(skip)]
    inner: Arc<dyn RequestClient>,
    max_redirects: usize,
}

impl SecureHttpClient {
    /// Constructs a client that uses default system roots to setup TLS.
    fn new(tls_config: ClientConfig, max_redirects: usize) -> Self {
        let connector = HttpsConnectorBuilder::new()
            .with_tls_config(tls_config)
            .https_or_http()
            .enable_http1()
            .enable_http2()
            .build();
        Self::with_connector(connector, max_redirects)
    }

    fn with_connector<C: Connect + Clone + Send + Sync + 'static>(
        connector: C,
        max_redirects: usize,
    ) -> Self {
        Self {
            inner: Arc::new(hyper::Client::builder().build::<_, Body>(connector)),
            max_redirects,
        }
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

#[derive(Allocative)]
struct SecureProxiedClient {
    inner: SecureHttpClient,
}

impl SecureProxiedClient {
    fn with_proxies<I: IntoIterator<Item = Proxy>>(proxies: I) -> anyhow::Result<Self> {
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

/// Http client used for unit testing; errors on any calls to underlying http methods.
#[derive(Allocative)]
pub struct ClientForTest {}

#[async_trait::async_trait]
impl HttpClient for ClientForTest {
    async fn request(&self, _request: Request<Body>) -> Result<Response<Body>, HttpError> {
        Err(HttpError::Test)
    }
}

#[cfg(test)]
mod tests {
    use httptest::matchers::*;
    use httptest::responders;
    use httptest::Expectation;

    use super::*;

    /// Barebones proxy server implementation that simply forwards requests onto
    /// the destination server.
    #[cfg(any(fbcode_build, cargo_internal_build))]
    struct ProxyServer {
        addr: std::net::SocketAddr,
        // Need to hold a ref to the task so when Drop runs on Self we cancel
        // the task.
        #[allow(dead_code)]
        handle: tokio::task::JoinHandle<()>,
    }

    #[cfg(any(fbcode_build, cargo_internal_build))]
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

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 10);
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

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 10);
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

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 10);
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

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 1);
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
    #[cfg(any(fbcode_build, cargo_internal_build))] // TODO(@akozhevnikov): Debug why this fails on CircleCI
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
    #[cfg(any(fbcode_build, cargo_internal_build))] // TODO(@akozhevnikov): Debug why this fails on CircleCI
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
            DefaultSchemeUri(proxy_uri.try_into()?).into(),
        )])?;
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }
}
