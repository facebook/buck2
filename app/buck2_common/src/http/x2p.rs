/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use http::HeaderMap;
use http::Uri;
use hyper_proxy::Proxy;

#[cfg(fbcode_build)]
mod imp {
    use anyhow::Context;
    use http::Uri;
    use hyper_proxy::Intercept;

    use super::*;

    #[cfg(unix)]
    pub(super) fn find_unix_socket_proxy() -> Option<Proxy> {
        let unix_socket = cpe::x2p::proxy_url_http1();
        if !unix_socket.is_empty() {
            tracing::debug!(
                "Using x2pagent unix socket proxy client at: {}",
                unix_socket
            );
            let unix_uri = hyper_unix_connector::Uri::new(unix_socket, "/");
            Some(Proxy::new(Intercept::All, unix_uri.into()))
        } else {
            None
        }
    }

    pub(super) fn find_http_proxy() -> anyhow::Result<Option<Proxy>> {
        if let Some(port) = cpe::x2p::http1_proxy_port() {
            tracing::debug!("Using x2pagent http proxy client on port: {}", port);
            let uri: Uri = format!("http://localhost:{}", port)
                .try_into()
                .context("Error converting x2pagent proxy address into URI")?;
            Ok(Some(Proxy::new(Intercept::All, uri)))
        } else {
            Ok(None)
        }
    }
}

#[cfg(fbcode_build)]
pub fn find_proxy() -> anyhow::Result<Option<Proxy>> {
    #[cfg(unix)]
    if let Some(proxy) = imp::find_unix_socket_proxy() {
        return Ok(Some(proxy));
    }

    imp::find_http_proxy()
}

#[cfg(not(fbcode_build))]
pub fn find_proxy() -> anyhow::Result<Option<Proxy>> {
    anyhow::bail!("VPNless development not supported for non-internal fbcode builds");
}

/// Whether the machine buck is running on supports vpnless operation.
pub fn supports_vpnless() -> bool {
    #[cfg(fbcode_build)]
    return cpe::x2p::supports_vpnless();

    #[cfg(not(fbcode_build))]
    return false;
}

/// Collection of different kinds of errors we can see from x2pagent. Typically
/// denotes a URL is not authorized for vpnless access and/or using the wrong,
/// non-vpnless url.
#[derive(Debug, thiserror::Error)]
pub enum X2PAgentError {
    #[error("Host `{0}` is not authorized for vpnless access")]
    ForbiddenHost(String),
    #[error("Failed to connect to `{0}`; is it authorized for vpnless?")]
    Connection(String),
    #[error("Host `{host}` and path `{path}` is not authorized on vpnless")]
    AccessDenied { host: String, path: String },
    #[error(transparent)]
    Error(#[from] anyhow::Error),
}

impl X2PAgentError {
    pub fn from_headers(uri: &Uri, headers: &HeaderMap) -> Option<Self> {
        let auth_decision = headers.get("x-fb-validated-x2pauth-decision");
        let error_type = headers.get("x-x2pagentd-error-type");
        let error_msg = headers.get("x-x2pagentd-error-msg");

        let host = uri.host().unwrap_or("<no host>").to_owned();
        match (auth_decision, error_type, error_msg) {
            (Some(decision), _, _) if decision == "deny" => Some(Self::AccessDenied {
                host,
                path: uri.path().to_owned(),
            }),
            (_, Some(typ), Some(_)) if typ == "FORBIDDEN_HOST" => Some(Self::ForbiddenHost(host)),
            (_, Some(typ), Some(_)) if typ == "CONNECTION" => Some(Self::Connection(host)),
            (_, _, Some(msg)) => Some(Self::Error(anyhow::anyhow!(
                String::from_utf8_lossy(msg.as_bytes()).into_owned()
            ))),
            _ => None,
        }
    }
}
