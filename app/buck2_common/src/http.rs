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
use std::path::PathBuf;

use anyhow::Context;
use gazebo::prelude::VecExt;
use http::HeaderMap;
use http::Method;
use http::Uri;
use hyper::body;
use hyper::client::connect::HttpConnector;
use hyper::Body;
use hyper::Client;
use hyper::Request;
use hyper::Response;
use hyper::StatusCode;
use hyper_rustls::HttpsConnector;
use hyper_rustls::HttpsConnectorBuilder;
use rustls::Certificate;
use rustls::ClientConfig;
use rustls::PrivateKey;
use rustls::RootCertStore;
use thiserror::Error;

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

pub struct HttpClientBuilder {
    cert_pair_paths: Option<(PathBuf, PathBuf)>,
    max_redirects: usize,
}

impl HttpClientBuilder {
    pub fn new() -> Self {
        Self {
            cert_pair_paths: None,
            max_redirects: 10,
        }
    }

    /// Configure this client to use the provided cert pair paths on disk for
    /// client authentication.
    pub fn with_cert_pair<P: AsRef<Path>>(&mut self, cert: P, key: P) -> &mut Self {
        self.cert_pair_paths = Some((cert.as_ref().to_path_buf(), key.as_ref().to_path_buf()));
        self
    }

    /// Configure maximum number of redirects. If this number of redirects is
    /// exceeded, client will return HttpError::TooManyRedirects.
    pub fn max_redirects(&mut self, max_redirects: usize) -> &mut Self {
        self.max_redirects = max_redirects;
        self
    }

    /// Create the https TLS config.
    fn make_tls_config(&self) -> anyhow::Result<ClientConfig> {
        let root_cert_store = load_system_root_certs()?;
        let builder = ClientConfig::builder()
            .with_safe_defaults()
            .with_root_certificates(root_cert_store);
        if let Some((cert_path, key_path)) = self.cert_pair_paths.as_ref() {
            let (certs, key) = load_cert_pair(cert_path, key_path)?;
            builder
                .with_single_cert(certs, key)
                .context("Error constructing TLS config")
        } else {
            Ok(builder.with_no_client_auth())
        }
    }

    pub fn build(&self) -> anyhow::Result<HttpClient> {
        let config = self.make_tls_config()?;
        let connector = HttpsConnectorBuilder::new()
            .with_tls_config(config)
            .https_or_http()
            .enable_http1()
            .enable_http2()
            .build();

        Ok(HttpClient::new(connector, self.max_redirects))
    }
}

pub struct HttpClient {
    inner: Client<HttpsConnector<HttpConnector>>,
    max_redirects: usize,
}

impl HttpClient {
    pub fn new(connector: HttpsConnector<HttpConnector>, max_redirects: usize) -> Self {
        let client = Client::builder().build::<_, Body>(connector);
        Self {
            inner: client,
            max_redirects,
        }
    }

    /// Standard HEAD request.
    pub async fn head(&self, uri: &str) -> Result<Response<()>, HttpError> {
        let req = Request::builder()
            .uri(uri)
            .method(Method::HEAD)
            .body(Body::empty())
            .map_err(HttpError::BuildRequest)?;
        let resp = self.send_request(req).await?;
        Ok(resp.map(|_| ()))
    }

    /// Standard GET request.
    pub async fn get(&self, uri: &str) -> Result<Response<Body>, HttpError> {
        let req = Request::builder()
            .uri(uri)
            .method(Method::GET)
            .body(Body::empty())
            .map_err(HttpError::BuildRequest)?;
        self.send_request(req).await
    }

    async fn send_request(&self, req: Request<Body>) -> Result<Response<Body>, HttpError> {
        let pending_request = PendingRequest::from_request(&req);
        let uri = req.uri().to_string();
        tracing::debug!("http: request: {:?}", req);
        let resp = self.send_request_impl(req).await?;
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

    async fn send_request_impl(&self, req: Request<Body>) -> Result<Response<Body>, HttpError> {
        self.inner
            .request(req)
            .await
            .map_err(HttpError::SendRequest)
    }
}

#[cfg(test)]
mod tests {
    use httptest::matchers::*;
    use httptest::responders;
    use httptest::Expectation;

    use super::*;

    #[tokio::test]
    async fn test_simple_get_success() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/foo"))
                .respond_with(responders::status_code(200)),
        );

        let client = HttpClientBuilder::new().build()?;
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

        let client = HttpClientBuilder::new().build()?;
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

        let client = HttpClientBuilder::new().build()?;
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

        let client = HttpClientBuilder::new().build()?;
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

        let client = HttpClientBuilder::new().max_redirects(1).build()?;
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
