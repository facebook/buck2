/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;
use std::future::Future;
use crate::error::{REClientError, TCode};
use tracing::warn;

pub async fn retry<F, Fut, T>(
    mut f: F,
    max_retries: usize,
    initial_delay: Duration,
    max_delay: Duration,
    retry_not_found: bool,
) -> anyhow::Result<T>
where
    F: FnMut() -> Fut,
    Fut: Future<Output = anyhow::Result<T>>,
{
    let mut retries = 0;
    let mut delay = initial_delay;

    loop {
        match f().await {
            Ok(val) => return Ok(val),
            Err(err) => {
                if retries >= max_retries {
                    return Err(err);
                }
                let retryable = is_retryable(&err, retry_not_found);

                if retryable == Retryable::No {
                    return Err(err);
                }

                retries += 1;
                // Try to downcast to REClientError to get a better message, otherwise just print the error
                let msg = if let Some(re_err) = err.downcast_ref::<REClientError>() {
                    re_err.message.as_str()
                } else {
                    // Try tonic::Status
                    if let Some(status) = err.downcast_ref::<tonic::Status>() {
                        status.message()
                    } else {
                        "unknown error"
                    }
                };

                if retryable == Retryable::Wait {
                    warn!("Retrying request after error: {}. Attempt {}/{} (waiting {:?})", msg, retries, max_retries, delay);
                    tokio::time::sleep(delay).await;

                    delay *= 2;
                    if delay > max_delay {
                        delay = max_delay;
                    }
                } else {
                    warn!("Retrying request after error: {}. Attempt {}/{}", msg, retries, max_retries);
                }
            }
        }
    }
}

#[derive(PartialEq)]
enum Retryable {
    Immediately,
    Wait,
    No,
}

fn is_retryable(err: &anyhow::Error, retry_not_found: bool) -> Retryable {
    for cause in err.chain() {
        if let Some(re_err) = cause.downcast_ref::<REClientError>() {
            return match re_err.code {
                TCode::DEADLINE_EXCEEDED => Retryable::Immediately,
                TCode::UNKNOWN
                | TCode::CANCELLED
                | TCode::ABORTED
                | TCode::INTERNAL
                | TCode::UNAVAILABLE
                | TCode::RESOURCE_EXHAUSTED => Retryable::Wait,
                TCode::NOT_FOUND if retry_not_found => Retryable::Wait,
                _ => Retryable::No,
            };
        }
        if let Some(status) = cause.downcast_ref::<tonic::Status>() {
            return match status.code() {
                tonic::Code::DeadlineExceeded => Retryable::Immediately,
                tonic::Code::Unknown
                | tonic::Code::Cancelled
                | tonic::Code::Aborted
                | tonic::Code::Internal
                | tonic::Code::Unavailable
                | tonic::Code::ResourceExhausted => Retryable::Wait,
                tonic::Code::NotFound if retry_not_found => Retryable::Wait,
                _ => Retryable::No,
            };
        }
    }
    Retryable::No
}
