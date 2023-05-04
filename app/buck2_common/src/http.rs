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
use std::path::PathBuf;

use anyhow::Context;
use http::Method;
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
        .into_iter()
        .map(Certificate)
        .collect();

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
}

pub struct HttpClientBuilder {
    cert_pair_paths: Option<(PathBuf, PathBuf)>,
}

impl HttpClientBuilder {
    pub fn new() -> Self {
        Self {
            cert_pair_paths: None,
        }
    }

    /// Configure this client to use the provided cert pair paths on disk for
    /// client authentication.
    pub fn with_cert_pair<P: AsRef<Path>>(&mut self, cert: P, key: P) -> &mut Self {
        self.cert_pair_paths = Some((cert.as_ref().to_path_buf(), key.as_ref().to_path_buf()));
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

        Ok(HttpClient::new(connector))
    }
}

pub struct HttpClient {
    inner: Client<HttpsConnector<HttpConnector>>,
}

impl HttpClient {
    pub fn new(connector: HttpsConnector<HttpConnector>) -> Self {
        let client = Client::builder().build::<_, Body>(connector);
        Self { inner: client }
    }

    /// Standard HEAD request.
    pub async fn head(&self, uri: &str) -> Result<Response<()>, HttpError> {
        let resp = self.send_request(Method::HEAD, uri).await?;
        Ok(resp.map(|_| ()))
    }

    /// Standard GET request.
    pub async fn get(&self, uri: &str) -> Result<Response<Body>, HttpError> {
        self.send_request(Method::GET, uri).await
    }

    async fn send_request(&self, method: Method, uri: &str) -> Result<Response<Body>, HttpError> {
        let req = Request::builder()
            .uri(uri)
            .method(method)
            .body(Body::empty())
            .map_err(HttpError::BuildRequest)?;

        let resp = self
            .inner
            .request(req)
            .await
            .map_err(HttpError::SendRequest)?;

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

            return Err(HttpError::Status {
                status,
                uri: uri.to_owned(),
                text,
            });
        }

        Ok(resp)
    }
}

#[cfg(test)]
mod tests {
    use httptest::matchers::request;
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
}
