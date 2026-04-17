/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Duration;
use std::future::Future;
use futures::Stream;
use futures::TryStreamExt;
use crate::error::{REClientError, TCode};
use tracing::warn;

pub async fn retry<F, Fut, T>(
    method: &str,
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
                    warn!(
                        "Retrying {} request after error: {}. Attempt {}/{} (waiting {:?})",
                        method, msg, retries, max_retries, delay
                    );
                    tokio::time::sleep(delay).await;

                    delay *= 2;
                    if delay > max_delay {
                        delay = max_delay;
                    }
                } else {
                    warn!(
                        "Retrying {} request after error: {}. Attempt {}/{}",
                        method, msg, retries, max_retries
                    );
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

/// Like [`retry`], but covers both stream establishment and stream reading.
///
/// `make_stream` is called to open the gRPC streaming call. If the call itself fails, or if
/// reading a message from the resulting stream fails, and the error is retryable, `make_stream`
/// is called again to establish a fresh stream (which causes tonic to obtain a new h2 connection
/// when the old one was closed by a GOAWAY frame, e.g. `ENHANCE_YOUR_CALM` /
/// `"too_many_internal_resets"`).
///
/// Items from the stream are yielded progressively to the caller (preserving streaming progress
/// updates), so this is a drop-in replacement for `retry(make_stream) + try_unfold(try_next)`.
pub fn retrying_stream<F, Fut, S, T>(
    method: &'static str,
    make_stream: F,
    max_retries: usize,
    initial_delay: Duration,
    max_delay: Duration,
    retry_not_found: bool,
) -> impl Stream<Item = anyhow::Result<T>> + Send + 'static
where
    F: Fn() -> Fut + Send + Sync + 'static,
    Fut: Future<Output = anyhow::Result<S>> + Send + 'static,
    S: futures::TryStream<Ok = T, Error = tonic::Status> + Send + Unpin + 'static,
    T: Send + 'static,
{
    struct RetryState<F, S> {
        make_stream: Arc<F>,
        stream: Option<S>,
        retries_left: usize,
        delay: Duration,
        max_delay: Duration,
        retry_not_found: bool,
    }

    let state = RetryState {
        make_stream: Arc::new(make_stream),
        stream: None,
        retries_left: max_retries,
        delay: initial_delay,
        max_delay,
        retry_not_found,
    };

    futures::stream::try_unfold(state, move |mut state| async move {
        loop {
            // Establish a stream if we don't have one (first call or after a retry).
            if state.stream.is_none() {
                match (state.make_stream)().await {
                    Ok(s) => {
                        state.stream = Some(s);
                    }
                    Err(err) => {
                        if is_retryable(&err, state.retry_not_found) != Retryable::No
                            && state.retries_left > 0
                        {
                            warn!(
                                "Retrying {} stream open after error: {}. Attempts remaining: {}",
                                method,
                                err,
                                state.retries_left,
                            );
                            tokio::time::sleep(state.delay).await;
                            state.delay *= 2;
                            if state.delay > state.max_delay {
                                state.delay = state.max_delay;
                            }
                            state.retries_left -= 1;
                            continue;
                        }
                        return Err(err);
                    }
                }
            }

            // Read the next message from the stream.
            match state.stream.as_mut().unwrap().try_next().await {
                Ok(Some(item)) => return Ok(Some((item, state))),
                Ok(None) => return Ok(None),
                Err(status) => {
                    let err = anyhow::Error::from(status);
                    if is_retryable(&err, state.retry_not_found) != Retryable::No
                        && state.retries_left > 0
                    {
                        warn!(
                            "Retrying {} stream read after error: {}. Attempts remaining: {}",
                            method,
                            err,
                            state.retries_left,
                        );
                        tokio::time::sleep(state.delay).await;
                        state.delay *= 2;
                        if state.delay > state.max_delay {
                            state.delay = state.max_delay;
                        }
                        state.retries_left -= 1;
                        // Drop the broken stream; next iteration re-establishes it.
                        state.stream = None;
                        continue;
                    }
                    return Err(err);
                }
            }
        }
    })
}
