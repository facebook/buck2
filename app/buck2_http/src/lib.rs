/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(if_let_guard)]

use hyper::StatusCode;

mod client;
mod proxy;
mod redirect;
pub mod retries;
mod stats;
mod x2p;

pub use client::to_bytes;
pub use client::HttpClient;
pub use client::HttpClientBuilder;

fn http_error_label(status: StatusCode) -> &'static str {
    if status.is_server_error() {
        "Server"
    } else if status.is_client_error() {
        "Client"
    } else {
        "Unknown"
    }
}

fn tag_from_status(status: StatusCode) -> buck2_error::ErrorTag {
    if status.is_server_error() {
        buck2_error::ErrorTag::HttpServer
    } else if status.is_client_error() {
        buck2_error::ErrorTag::HttpClient
    } else {
        buck2_error::ErrorTag::Http
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Http)]
pub enum HttpError {
    #[error("HTTP URI Error: URI {uri} is malformed: {source:?}")]
    InvalidUri {
        uri: String,
        #[source]
        source: http::uri::InvalidUri,
    },
    #[error("HTTP URI Error: URI parts {uri} is malformed: {source:?}")]
    InvalidUriParts {
        uri: String,
        #[source]
        source: http::uri::InvalidUriParts,
    },
    #[error("HTTP: Error building request")]
    BuildRequest(#[source] http::Error),
    #[error("HTTP: Error sending request to {uri}")]
    #[buck2(tier0)]
    SendRequest {
        uri: String,
        #[source]
        source: hyper::Error,
    },
    #[error("HTTP {} Error ({status}) when querying URI: {uri}. Response text: {text}", http_error_label(*.status))]
    #[buck2(tag = tag_from_status(status))]
    Status {
        status: StatusCode,
        uri: String,
        text: String,
    },
    #[error("HTTP Error: Exceeded max redirects ({max_redirects}) while fetching URI: {uri}. ")]
    TooManyRedirects { uri: String, max_redirects: usize },
    #[error("HTTP: Error mutating request")]
    MutateRequest(#[source] buck2_error::Error),
    #[error("HTTP: Timed out while making request to URI: {uri} after {duration} seconds.")]
    #[buck2(tier0)]
    Timeout { uri: String, duration: u64 },
    #[error("While making request to {uri} via x2p")]
    X2P {
        uri: String,
        #[source]
        source: x2p::X2PAgentError,
    },
}

impl From<http::Error> for HttpError {
    fn from(err: http::Error) -> Self {
        Self::BuildRequest(err)
    }
}

impl From<buck2_error::Error> for HttpError {
    fn from(err: buck2_error::Error) -> Self {
        Self::MutateRequest(err)
    }
}
