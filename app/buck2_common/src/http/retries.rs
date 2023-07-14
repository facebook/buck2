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
                crate::http::HttpError::SendRequest(..) => true,
                _ => false,
            },
            Self::Transfer { source, .. } => !source.is_connect(),
        }
    }
}
pub trait AsHttpError {
    fn as_http_error(&self) -> Option<&HttpError>;
}

/// Used to dispatch warnings to the event log in the case in the case of a
/// retryable HTTP error. This often occurs when some level of exponential
/// backoff is happening due to a lot of concurrent http_archive() requests for
/// a single host; see GH-316 and GH-321 for background.
pub trait DispatchableHttpRetryWarning {
    /// Fire off a warning. This might go to the console, event log, or
    /// some other place.
    fn dispatch(&self, dur: &Duration, retries: usize, url: &str);
}

/// No-op implementation of DispatchableHttpRetryWarning. This does nothing at
/// all when a retry occurs; useful for mock tests.
pub struct NoopDispatchableHttpRetryWarning {}

impl NoopDispatchableHttpRetryWarning {
    pub fn new() -> Self {
        Self {}
    }
}

impl DispatchableHttpRetryWarning for NoopDispatchableHttpRetryWarning {
    fn dispatch(&self, _backoff: &Duration, _retries: usize, _url: &str) {}
}

pub async fn http_retry<Exec, F, T, E, R>(
    url: &str,
    dispatch_retry_warning: R,
    exec: Exec,
    mut intervals: Vec<Duration>,
) -> Result<T, E>
where
    Exec: Fn() -> F,
    E: AsHttpError + std::fmt::Display,
    F: Future<Output = Result<T, E>>,
    R: DispatchableHttpRetryWarning,
{
    intervals.insert(0, Duration::from_secs(0));
    let mut backoff = intervals.into_iter().peekable();

    let mut retries = 0;
    while let Some(duration) = backoff.next() {
        tokio::time::sleep(duration).await;

        let res = exec().await;

        let http_error = res.as_ref().err().and_then(|err| err.as_http_error());

        if let Some(http_error) = http_error {
            if http_error.is_retryable() {
                retries += 1;
                if let Some(b) = backoff.peek() {
                    dispatch_retry_warning.dispatch(&b, retries, url);
                    continue;
                }
            }
        }

        return res;
    }

    unreachable!("The loop above will exit before we get to the end")
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Mock {
        responses: std::cell::RefCell<Vec<Result<String, HttpTestError>>>,
    }

    impl Mock {
        fn new(responses: Vec<Result<String, HttpTestError>>) -> Self {
            Self {
                responses: std::cell::RefCell::new(responses),
            }
        }

        async fn exec(&self) -> Result<String, HttpTestError> {
            // This function is expected to be of type Fn and not FnMut
            // We use internal mutability (RefCell) here to ensure that
            let mut responses = self.responses.borrow_mut();
            responses.remove(0)
        }
    }

    #[derive(Debug, Error)]
    enum HttpTestError {
        #[error("Error in test")]
        Client(#[from] HttpError),
    }

    impl AsHttpError for HttpTestError {
        fn as_http_error(&self) -> Option<&HttpError> {
            match self {
                Self::Client(e) => Some(e),
            }
        }
    }

    fn ok_response() -> Result<String, HttpTestError> {
        Ok("Success".to_owned()).map_err(|_: anyhow::Error| test_error(StatusCode::IM_A_TEAPOT))
    }

    fn retryable() -> Result<String, HttpTestError> {
        Err(test_error(StatusCode::INTERNAL_SERVER_ERROR))
    }

    fn non_retryable() -> Result<String, HttpTestError> {
        Err(test_error(StatusCode::NOT_FOUND))
    }

    fn test_error(status: StatusCode) -> HttpTestError {
        HttpTestError::Client(HttpError::Client(crate::http::HttpError::Status {
            status,
            uri: "something".to_owned(),
            text: "something else".to_owned(),
        }))
    }

    fn retries(n: usize) -> Vec<Duration> {
        vec![Duration::from_secs(0); n]
    }

    #[tokio::test]
    async fn test_http_retry_success() {
        let url = "http://example.com";
        let dispatch_retry_warning = NoopDispatchableHttpRetryWarning::new();
        let mock = Mock::new(vec![ok_response()]);
        let result = http_retry(url, dispatch_retry_warning, || mock.exec(), retries(0)).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_http_retry_retryable() {
        let url = "http://example.com";
        let dispatch_retry_warning = NoopDispatchableHttpRetryWarning::new();
        let mock = Mock::new(vec![retryable(), ok_response()]);
        let result = http_retry(url, dispatch_retry_warning, || mock.exec(), retries(1)).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_http_retry_exhaust_retries() {
        let url = "http://example.com";
        let dispatch_retry_warning = NoopDispatchableHttpRetryWarning::new();
        let mock = Mock::new(vec![retryable(), ok_response()]);
        let result = http_retry(url, dispatch_retry_warning, || mock.exec(), retries(0)).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_http_retry_non_retryable() {
        let url = "http://example.com";
        let dispatch_retry_warning = NoopDispatchableHttpRetryWarning::new();
        let mock = Mock::new(vec![non_retryable(), ok_response()]);
        let result = http_retry(url, dispatch_retry_warning, || mock.exec(), retries(1)).await;
        assert!(result.is_err());
    }
}
