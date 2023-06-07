/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use futures::future::Future;
use http::StatusCode;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum HttpError {
    #[error("HTTP Client error: {0}")]
    Client(#[from] crate::http::HttpError),

    #[error("HTTP Transfer Error when querying URL: {}. Failed after {} bytes", .url, .received)]
    Transfer {
        received: u64,
        url: String,
        #[source]
        source: hyper::Error,
    },
}

impl HttpError {
    fn is_retryable(&self) -> bool {
        match self {
            Self::Client(client_error) => match client_error {
                crate::http::HttpError::Status { status, .. } => {
                    status.is_server_error() || *status == StatusCode::TOO_MANY_REQUESTS
                }
                crate::http::HttpError::SendRequest(err) => !err.is_connect(),
                _ => false,
            },
            Self::Transfer { source, .. } => !source.is_connect(),
        }
    }
}
pub trait AsHttpError {
    fn as_http_error(&self) -> Option<&HttpError>;
}

pub async fn http_retry<Exec, F, T, E>(exec: Exec) -> Result<T, E>
where
    Exec: Fn() -> F,
    E: AsHttpError + std::fmt::Display,
    F: Future<Output = Result<T, E>>,
{
    let mut backoff = [0, 2, 4, 8].into_iter().peekable();

    while let Some(duration) = backoff.next() {
        tokio::time::sleep(Duration::from_secs(duration)).await;

        let res = exec().await;

        let http_error = res.as_ref().err().and_then(|err| err.as_http_error());

        if let Some(http_error) = http_error {
            if http_error.is_retryable() {
                if let Some(b) = backoff.peek() {
                    tracing::warn!(
                        "Retrying a HTTP error after {} seconds: {:#}",
                        b,
                        http_error
                    );
                    continue;
                }
            }
        }

        return res;
    }

    unreachable!("The loop above will exit before we get to the end")
}
