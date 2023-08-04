/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

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
