/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::sync::Arc;

use hyper::client::HttpConnector;
use hyper::service::Service;
use hyper::Body;
use hyper::Uri;
use hyper_proxy::Proxy;
use hyper_proxy::ProxyConnector;
use hyper_rustls::HttpsConnector;
use hyper_rustls::HttpsConnectorBuilder;
use rustls::ClientConfig;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tokio_rustls::TlsConnector;

use super::HttpClient;
use super::RequestClient;
use crate::http::proxy;
use crate::http::stats::HttpNetworkStats;
use crate::http::tls;

pub struct HttpClientBuilder {
    tls_config: ClientConfig,
    proxies: Vec<Proxy>,
    max_redirects: Option<usize>,
    supports_vpnless: bool,
}

impl HttpClientBuilder {
    pub fn https_with_system_roots() -> anyhow::Result<Self> {
        let tls_config = tls::tls_config_with_system_roots()?;
        Ok(Self {
            tls_config,
            proxies: Vec::new(),
            max_redirects: None,
            supports_vpnless: false,
        })
    }

    pub fn with_tls_config(&mut self, tls_config: ClientConfig) -> &mut Self {
        self.tls_config = tls_config;
        self
    }

    pub fn with_client_auth_cert<P: AsRef<Path>>(&mut self, path: P) -> anyhow::Result<&mut Self> {
        let tls_config = tls::tls_config_with_single_cert(path.as_ref(), path.as_ref())?;
        Ok(self.with_tls_config(tls_config))
    }

    pub fn with_proxy(&mut self, proxy: Proxy) -> &mut Self {
        self.proxies.push(proxy);
        self
    }

    pub fn with_x2p_proxy(&mut self, proxy: Proxy) -> &mut Self {
        self.with_proxy(proxy).supports_vpnless();
        self
    }

    pub fn with_proxy_from_env(&mut self) -> anyhow::Result<&mut Self> {
        if let Some(proxy) = proxy::https_proxy_from_env()? {
            self.with_proxy(proxy);
        }
        if let Some(proxy) = proxy::http_proxy_from_env()? {
            self.with_proxy(proxy);
        }
        Ok(self)
    }

    pub fn with_max_redirects(&mut self, max_redirects: usize) -> &mut Self {
        self.max_redirects = Some(max_redirects);
        self
    }

    fn supports_vpnless(&mut self) -> &mut Self {
        self.supports_vpnless = true;
        self
    }

    fn build_inner(&self) -> Arc<dyn RequestClient> {
        match self.proxies.as_slice() {
            // Construct x2p unix socket client.
            // Note: This ignores (and does not require) the TLS config.
            #[cfg(unix)]
            proxies @ [_, ..] if let Some(unix_socket) = find_unix_proxy(proxies) => {
                let proxy_connector = build_proxy_connector(&[unix_socket.clone()], hyper_unix_connector::UnixClient, None);
                Arc::new(hyper::Client::builder().build::<_, Body>(proxy_connector))
            },

            // Construct x2p http proxy client.
            proxies @ [_, ..] if self.supports_vpnless => {
                let mut http_connector = HttpConnector::new();
                // When talking to local x2pagent proxy, only http is supported.
                http_connector.enforce_http(true);
                let proxy_connector = build_proxy_connector(proxies, http_connector, None);
                Arc::new(hyper::Client::builder().build::<_, Body>(proxy_connector))
            }

            // Proxied http client with TLS.
            proxies @ [_, ..] => {
                let https_connector = build_https_connector(self.tls_config.clone());
                let proxy_connector = build_proxy_connector(proxies, https_connector, Some(self.tls_config.clone()));
                Arc::new(hyper::Client::builder().build::<_, Body>(proxy_connector))
            },

            // Client with TLS only.
            [] => {
                let https_connector = build_https_connector(self.tls_config.clone());
                Arc::new(hyper::Client::builder().build::<_, Body>(https_connector))
            },
        }
    }

    pub fn build(&self) -> HttpClient {
        HttpClient {
            inner: self.build_inner(),
            max_redirects: self.max_redirects,
            supports_vpnless: self.supports_vpnless,
            stats: HttpNetworkStats::new(),
        }
    }
}

fn build_https_connector(tls_config: ClientConfig) -> HttpsConnector<HttpConnector> {
    HttpsConnectorBuilder::new()
        .with_tls_config(tls_config)
        .https_or_http()
        .enable_http1()
        .enable_http2()
        .build()
}

/// Build a proxy connector using `proxies`, wrapping underlying `connector`,
/// and optionally using `tls_config` to secure communications with the proxy.
///
/// Note: Not all proxy connectors built by this client need TLS communication
/// with the proxy, e.g. if the proxy is on localhost.
fn build_proxy_connector<C>(
    proxies: &[Proxy],
    connector: C,
    tls_config: Option<ClientConfig>,
) -> ProxyConnector<C>
where
    C: Service<Uri> + Send,
    C::Response: AsyncRead + AsyncWrite + Send + Unpin,
    C::Future: Send + 'static,
    C::Error: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    // Note: we use the `unsecured()` constructor here, but all that does is
    // not load the default TLS config. You can optionally pass your own tls
    // config if needed.
    let mut proxy_connector = ProxyConnector::unsecured(connector);
    proxy_connector.extend_proxies(proxies.iter().cloned());
    if let Some(tls_config) = tls_config {
        proxy_connector.set_tls(Some(TlsConnector::from(Arc::new(tls_config))));
    }
    proxy_connector
}

/// Helper function to find any proxies with unix:// as the scheme (which
/// indicates we want to proxy through a unix domain socket).
///
/// Note: This _does_ compile on non-unix, but is only used at runtime in unix;
/// adding this to silence dead code warnings.
#[cfg(unix)]
fn find_unix_proxy(proxies: &[Proxy]) -> Option<&Proxy> {
    proxies
        .iter()
        .find(|proxy| proxy.uri().scheme_str() == Some("unix"))
}

#[cfg(test)]
mod tests {
    use hyper_proxy::Intercept;

    use super::*;

    #[test]
    fn test_default_builder() -> anyhow::Result<()> {
        let builder = HttpClientBuilder::https_with_system_roots()?;

        assert_eq!(None, builder.max_redirects);
        assert!(builder.proxies.is_empty());
        assert!(!builder.supports_vpnless);
        Ok(())
    }

    #[test]
    fn test_supports_vpnless_set_true() -> anyhow::Result<()> {
        let mut builder = HttpClientBuilder::https_with_system_roots()?;
        builder.supports_vpnless();

        assert!(builder.supports_vpnless);
        Ok(())
    }

    #[test]
    fn test_with_max_redirects_overrides_default() -> anyhow::Result<()> {
        let mut builder = HttpClientBuilder::https_with_system_roots()?;
        builder.with_max_redirects(5);

        assert_eq!(5, builder.max_redirects.unwrap());
        Ok(())
    }

    #[test]
    fn test_builder_with_proxy_adds_proxy() -> anyhow::Result<()> {
        let proxy = Proxy::new(Intercept::All, "http://localhost:12345".try_into()?);
        let mut builder = HttpClientBuilder::https_with_system_roots()?;
        builder.with_proxy(proxy);

        assert_eq!(1, builder.proxies.len());
        Ok(())
    }
}
