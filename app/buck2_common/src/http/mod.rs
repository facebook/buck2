/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_core::is_open_source;
use dice::UserComputationData;
use dupe::Dupe;
use hyper::StatusCode;
use thiserror::Error;

mod client;
mod proxy;
mod redirect;
pub mod retries;
mod stats;
pub mod tls;
mod x2p;

pub use client::HttpClient;
pub use client::HttpClientBuilder;

/// General-purpose function to get a regular HTTP client for use throughout the
/// buck2 codebase.
///
/// This should work for internal and OSS use cases.
/// TODO(skarlage): Remove `allow_vpnless` when vpnless becomes default.
pub fn http_client(allow_vpnless: bool) -> anyhow::Result<HttpClient> {
    let mut builder = HttpClientBuilder::https_with_system_roots()?;
    builder.with_max_redirects(10);

    if is_open_source() {
        Ok(builder.with_proxy_from_env()?.build())
    } else if allow_vpnless && supports_vpnless() {
        let proxy = x2p::find_proxy()?.context(
            "Expected unix domain socket or http proxy port for x2p client but did not find either",
        )?;
        Ok(builder.with_x2p_proxy(proxy).build())
    } else if let Ok(Some(cert_path)) = tls::find_internal_cert() {
        Ok(builder.with_client_auth_cert(cert_path)?.build())
    } else {
        Ok(builder.build())
    }
}

/// Dice implementations so we can pass along the HttpClient to various subsystems
/// that need to use it (Materializer, RunActions, etc).
pub trait HasHttpClient {
    fn get_http_client(&self) -> HttpClient;
}

pub trait SetHttpClient {
    fn set_http_client(&mut self, client: HttpClient);
}

impl HasHttpClient for UserComputationData {
    fn get_http_client(&self) -> HttpClient {
        self.data
            .get::<HttpClient>()
            .expect("HttpClient should be set")
            .dupe()
    }
}

impl SetHttpClient for UserComputationData {
    fn set_http_client(&mut self, client: HttpClient) {
        self.data.set(client);
    }
}

/// Whether the machine buck is running on supports vpnless operation.
fn supports_vpnless() -> bool {
    #[cfg(fbcode_build)]
    return cpe::x2p::supports_vpnless();

    #[cfg(not(fbcode_build))]
    return false;
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
    #[error("HTTP: Error sending request to {uri}")]
    SendRequest {
        uri: String,
        #[source]
        source: hyper::Error,
    },
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
    #[error("HTTP: Timed out while make request to URI: {uri} after {duration} seconds.")]
    Timeout { uri: String, duration: u64 },
}
