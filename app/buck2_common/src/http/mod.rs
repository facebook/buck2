/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

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
    #[error("HTTP: Error building request")]
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
    #[error("HTTP: Error mutating request")]
    MutateRequest(#[from] anyhow::Error),
    #[error("HTTP: Timed out while making request to URI: {uri} after {duration} seconds.")]
    Timeout { uri: String, duration: u64 },
    #[error("While making request to {uri} via x2p")]
    X2P {
        uri: String,
        #[source]
        source: x2p::X2PAgentError,
    },
}
