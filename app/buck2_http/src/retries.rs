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

#[derive(Debug, buck2_error::Error)]
pub enum HttpError {
    #[error(transparent)]
    Client(crate::HttpError),

    #[error("HTTP Transfer Error when querying URL: {}. Failed after {} bytes", .url, .received)]
    Transfer {
        received: u64,
        url: String,
        #[source]
        source: hyper::Error,
    },
}

impl From<crate::HttpError> for HttpError {
    fn from(value: crate::HttpError) -> Self {
        Self::Client(value)
    }
}

impl HttpError {
    fn is_retryable(&self) -> bool {
        match self {
            Self::Client(client_error) => match client_error {
                crate::HttpError::Status { status, .. } => {
                    status.is_server_error() || *status == StatusCode::TOO_MANY_REQUESTS
                }
                crate::HttpError::Timeout { .. } => true,
                crate::HttpError::SendRequest { .. } => true,
                _ => false,
            },
            Self::Transfer { source, .. } => !source.is_connect(),
        }
    }
}
pub trait AsHttpError {
    fn as_http_error(&self) -> Option<&HttpError>;
}

pub async fn http_retry<Exec, F, T, E>(exec: Exec, mut intervals: Vec<Duration>) -> Result<T, E>
where
    Exec: Fn() -> F,
    E: std::error::Error + AsHttpError + std::fmt::Display + Send + Sync + 'static,
    F: Future<Output = Result<T, E>>,
{
    intervals.insert(0, Duration::from_secs(0));
    let mut backoff = intervals.into_iter().peekable();

    while let Some(duration) = backoff.next() {
        tokio::time::sleep(duration).await;

        let err = match exec().await {
            Ok(val) => return Ok(val),
            Err(err) => err,
        };

        if let Some(http_error) = err.as_http_error() {
            if http_error.is_retryable() {
                if let Some(b) = backoff.peek() {
                    tracing::warn!(
                        "Retrying a HTTP error after {} seconds: {:#}",
                        b.as_secs(),
                        // Print as a buck2_error to make sure we get the source
                        buck2_error::Error::from(err)
                    );
                    continue;
                }
            }
        }

        return Err(err);
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

    #[derive(Debug, buck2_error::Error)]
    enum HttpTestError {
        #[error("Error in test")]
        Client(#[source] HttpError),
    }

    impl From<HttpError> for HttpTestError {
        fn from(value: HttpError) -> Self {
            Self::Client(value)
        }
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
        HttpTestError::Client(HttpError::Client(crate::HttpError::Status {
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
        let mock = Mock::new(vec![ok_response()]);
        let result = http_retry(|| mock.exec(), retries(0)).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_http_retry_retryable() {
        let mock = Mock::new(vec![retryable(), ok_response()]);
        let result = http_retry(|| mock.exec(), retries(1)).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_http_retry_exhaust_retries() {
        let mock = Mock::new(vec![retryable(), ok_response()]);
        let result = http_retry(|| mock.exec(), retries(0)).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_http_retry_non_retryable() {
        let mock = Mock::new(vec![non_retryable(), ok_response()]);
        let result = http_retry(|| mock.exec(), retries(1)).await;
        assert!(result.is_err());
    }
}
