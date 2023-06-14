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
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_core::is_open_source;
use bytes::Bytes;
use dice::UserComputationData;
use dupe::Dupe;
use futures::stream::BoxStream;
use gazebo::prelude::VecExt;
use http::Method;
use hyper::client::connect::Connect;
use hyper::client::ResponseFuture;
use hyper::Body;
use hyper::Request;
use hyper::Response;
use hyper::StatusCode;
use rustls::Certificate;
use rustls::ClientConfig;
use rustls::PrivateKey;
use rustls::RootCertStore;
use thiserror::Error;

use self::find_certs::find_tls_cert;

pub mod counting_client;
pub mod find_certs;
mod proxy;
mod redirect;
pub mod retries;
mod secure_client;
mod secure_proxied_client;
#[cfg(fbcode_build)]
mod x2p;
use counting_client::CountingHttpClient;
use proxy::http_proxy_from_env;
use proxy::https_proxy_from_env;
use secure_client::SecureHttpClient;
use secure_proxied_client::SecureProxiedClient;

const BUCK2_USER_AGENT: &str = "Buck2";

/// Support following up to 10 redirects, after which a redirected request will
/// error out.
const DEFAULT_MAX_REDIRECTS: usize = 10;

/// General-purpose function to get a regular HTTP client for use throughout the
/// buck2 codebase.
///
/// This should work for internal and OSS use cases.
/// TODO(skarlage): Remove `allow_vpnless` when vpnless becomes default.
pub fn http_client(allow_vpnless: bool) -> anyhow::Result<CountingHttpClient> {
    let http_client = if is_open_source() {
        http_client_for_oss()
    } else if allow_vpnless && supports_vpnless() {
        http_client_for_vpnless()
    } else {
        http_client_for_internal()
    }?;
    Ok(CountingHttpClient::new(http_client))
}

/// Returns a client suitable for OSS usecases. Supports standard Curl-like
/// proxy environment variables: $HTTP_PROXY, $HTTPS_PROXY.
pub fn http_client_for_oss() -> anyhow::Result<Arc<dyn HttpClient>> {
    // Add standard proxy variables if defined.
    // Ignores values that cannot be turned into valid URIs.
    let mut proxies = Vec::new();
    if let Some(proxy) = https_proxy_from_env()? {
        proxies.push(proxy);
    }
    if let Some(proxy) = http_proxy_from_env()? {
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
    let tls_config = if let Ok(Some(cert_path)) = find_tls_cert() {
        tls_config_with_single_cert(cert_path.clone(), cert_path)?
    } else {
        tls_config_with_system_roots()?
    };

    Ok(Arc::new(SecureHttpClient::new(
        tls_config,
        DEFAULT_MAX_REDIRECTS,
    )))
}

/// Returns a client suitable for making http requests via the VPNless x2pagent
/// proxy running on the local machine. Supports both http proxy server and
/// unix domain socket proxy path.
#[cfg(fbcode_build)]
fn http_client_for_vpnless() -> anyhow::Result<Arc<dyn HttpClient>> {
    if let Some(port) = cpe::x2p::http1_proxy_port() {
        let client = x2p::X2PAgentProxyClient::new(port)?;
        Ok(Arc::new(client))
    } else {
        #[cfg(unix)]
        {
            let proxy_path = cpe::x2p::proxy_url_http1();
            let client = x2p::X2PAgentUnixSocketClient::new(proxy_path)?;
            Ok(Arc::new(client))
        }

        #[cfg(not(unix))]
        anyhow::bail!("VPNless unix domain socket http client not supported in non-unix");
    }
}

#[cfg(not(fbcode_build))]
fn http_client_for_vpnless() -> anyhow::Result<Arc<dyn HttpClient>> {
    anyhow::bail!("VPNless client is not supported for non-internal fbcode builds")
}

/// Dice implementations so we can pass along the HttpClient to various subsystems
/// that need to use it (Materializer, RunActions, etc).
pub trait HasHttpClient {
    fn get_http_client(&self) -> CountingHttpClient;
}

pub trait SetHttpClient {
    fn set_http_client(&mut self, client: CountingHttpClient);
}

impl HasHttpClient for UserComputationData {
    fn get_http_client(&self) -> CountingHttpClient {
        self.data
            .get::<CountingHttpClient>()
            .expect("HttpClient should be set")
            .dupe()
    }
}

impl SetHttpClient for UserComputationData {
    fn set_http_client(&mut self, client: CountingHttpClient) {
        self.data.set(client);
    }
}

/// Whether the machine buck is running on supports vpnless operation.
fn supports_vpnless() -> bool {
    #[cfg(fbcode_build)]
    return cpe::x2p::is_edge_enabled()
        && cpe::user::is_gk_enabled("cpe_x2p_edgeterm_remote_execution")
        && cpe::user::is_gk_enabled("cpe_x2p_edgeterm_dotslash");

    #[cfg(not(fbcode_build))]
    return false;
}

/// Load the system root certificates into rustls cert store.
fn load_system_root_certs() -> anyhow::Result<RootCertStore> {
    let mut roots = rustls::RootCertStore::empty();
    let native_certs: Vec<_> = rustls_native_certs::load_native_certs()
        .context("Error loading system root certificates")?
        .into_map(|cert| cert.0);

    // According to [`rustls` documentation](https://docs.rs/rustls/latest/rustls/struct.RootCertStore.html#method.add_parsable_certificates),
    // it's better to only add parseable certs when loading system certs because
    // there are typically many system certs and not all of them can be valid. This
    // is pertinent for e.g. macOS which may have a lot of old certificates that may
    // not parse correctly.
    let (valid, invalid) = roots.add_parsable_certificates(native_certs.as_slice());

    // But make sure we get at least _one_ valid cert, otherwise we legitimately won't be
    // able to make any connections via https.
    anyhow::ensure!(
        valid > 0,
        "Error loading system certs: unable to find any valid system certs"
    );
    tracing::debug!("Loaded {} valid system root certs", valid);
    tracing::debug!("Loaded {} invalid system root certs", invalid);
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
            .header(http::header::USER_AGENT, BUCK2_USER_AGENT)
            .body(Bytes::new())
            .map_err(HttpError::BuildRequest)?;
        self.request(req).await.map(|resp| resp.map(|_| ()))
    }

    /// Send a GET request.
    async fn get(&self, uri: &str) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        let req = Request::builder()
            .uri(uri)
            .method(Method::GET)
            .header(http::header::USER_AGENT, BUCK2_USER_AGENT)
            .body(Bytes::new())
            .map_err(HttpError::BuildRequest)?;
        self.request(req).await
    }

    async fn post(
        &self,
        uri: &str,
        body: Bytes,
        headers: Vec<(String, String)>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        let mut req = Request::builder()
            .uri(uri)
            .method(Method::POST)
            .header(http::header::USER_AGENT, BUCK2_USER_AGENT);
        for (name, value) in headers {
            req = req.header(name, value);
        }
        let req = req.body(body).map_err(HttpError::BuildRequest)?;
        self.request(req).await
    }

    async fn put(
        &self,
        uri: &str,
        body: Bytes,
        headers: Vec<(String, String)>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        let mut req = Request::builder()
            .uri(uri)
            .method(Method::PUT)
            .header(http::header::USER_AGENT, BUCK2_USER_AGENT);
        for (name, value) in headers {
            req = req.header(name, value);
        }
        let req = req.body(body).map_err(HttpError::BuildRequest)?;
        self.request(req).await
    }

    /// Send a generic request.
    async fn request(
        &self,
        request: Request<Bytes>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError>;

    /// Whether this client supports vpnless operation. When set, will make requests
    /// to the `vpnless_url` attribute in the `download_file` action rather than the
    /// normal `url` attribute.
    fn supports_vpnless(&self) -> bool {
        // Most clients do not support vpnless.
        false
    }
}

/// Trait wrapper around a hyper::Client because hyper::Client is parameterized by
/// the connector. At runtime, we want to pick different connectors (e.g. HttpsConnector,
/// ProxyConnector<HttpsConnector<..>>, etc); thus wrap the client so we can switch
/// out the concrete type without exposing implementation details to callers.
trait RequestClient: Send + Sync {
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

/// Http client used for unit testing; errors on any calls to underlying http methods.
#[derive(Allocative)]
pub struct ClientForTest {}

#[async_trait::async_trait]
impl HttpClient for ClientForTest {
    async fn request(
        &self,
        _request: Request<Bytes>,
    ) -> Result<Response<BoxStream<hyper::Result<Bytes>>>, HttpError> {
        Err(HttpError::Test)
    }
}

#[cfg(test)]
mod tests {
    use httptest::matchers::*;
    use httptest::responders;
    use httptest::Expectation;

    use super::*;

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
    async fn test_post_gets_redirected() -> anyhow::Result<()> {
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

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 10);
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
}
