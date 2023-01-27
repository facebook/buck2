/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use buck2_cli_proto::CommandProgress;
use buck2_cli_proto::MultiCommandProgress;
use futures::stream::Stream;
use pin_project::pin_project;
use prost::Message;

const PREFERRED_MESSAGE_SIZE_BYTES: usize = 32768; // This is an approximation.

/// Buffer CommandProgress into MultiCommandProgress batches.
#[pin_project]
pub struct MultiEventStream<S, E> {
    #[pin]
    inner: S,
    done: bool,
    buffered_err: Option<E>,
}

impl<S, E> MultiEventStream<S, E> {
    pub fn new(inner: S) -> Self {
        Self {
            inner,
            done: false,
            buffered_err: None,
        }
    }
}

impl<S, E> Stream for MultiEventStream<S, E>
where
    S: Stream<Item = Result<CommandProgress, E>>,
{
    type Item = Result<MultiCommandProgress, E>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let mut this = self.project();

        let mut current_size = 0;
        let mut messages = Vec::new();

        loop {
            if *this.done {
                break;
            };

            if current_size > PREFERRED_MESSAGE_SIZE_BYTES {
                break;
            }

            if this.buffered_err.is_some() {
                break;
            }

            match this.inner.as_mut().poll_next(cx) {
                Poll::Ready(Some(Ok(item))) => {
                    // The current_size is an approximation.
                    let len = item.encoded_len();
                    current_size += len + prost::length_delimiter_len(len);
                    messages.push(item);
                }
                Poll::Ready(Some(Err(e))) => {
                    *this.buffered_err = Some(e);
                }
                Poll::Ready(None) => {
                    *this.done = true;
                }
                Poll::Pending => {
                    break;
                }
            }
        }

        if !messages.is_empty() {
            tracing::trace!(n = messages.len(), "multi");
            return Poll::Ready(Some(Ok(MultiCommandProgress { messages })));
        }

        if let Some(err) = this.buffered_err.take() {
            return Poll::Ready(Some(Err(err)));
        }

        if *this.done {
            Poll::Ready(None)
        } else {
            Poll::Pending
        }
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use buck2_data::BuckEvent;
    use futures::stream::poll_fn;
    use futures::stream::Stream;
    use futures::stream::StreamExt;

    use super::*;

    fn event(span_id: u64) -> CommandProgress {
        CommandProgress {
            progress: Some(buck2_cli_proto::command_progress::Progress::Event(
                box BuckEvent {
                    span_id,
                    ..Default::default()
                },
            )),
        }
    }

    fn ready_event(span_id: u64) -> Poll<Option<Result<CommandProgress, ()>>> {
        Poll::Ready(Some(Ok(event(span_id))))
    }

    fn test_stream(
        mut events: Vec<Poll<Option<Result<CommandProgress, ()>>>>,
    ) -> impl Stream<Item = Result<CommandProgress, ()>> {
        events.push(Poll::Ready(None));
        events.reverse();
        poll_fn(move |_| events.pop().expect("Polled after completion!"))
    }

    #[tokio::test]
    async fn test_batch_and_done() {
        let s = test_stream(vec![ready_event(1), ready_event(2)]);

        let s = MultiEventStream::new(s);

        assert_eq!(
            s.collect::<Vec<_>>().await,
            vec![Ok(MultiCommandProgress {
                messages: vec![event(1), event(2)]
            })]
        );
    }

    #[tokio::test]
    async fn test_err() {
        let s = test_stream(vec![Poll::Ready(Some(Err(())))]);

        let s = MultiEventStream::new(s);

        assert_eq!(s.collect::<Vec<_>>().await, vec![Err(())]);
    }

    #[tokio::test]
    async fn test_err_buffered() {
        let s = test_stream(vec![ready_event(1), Poll::Ready(Some(Err(())))]);

        let s = MultiEventStream::new(s);

        assert_eq!(
            s.collect::<Vec<_>>().await,
            vec![
                Ok(MultiCommandProgress {
                    messages: vec![event(1)]
                }),
                Err(())
            ]
        );
    }

    #[tokio::test]
    async fn test_batch_on_pending() {
        let s = test_stream(vec![ready_event(1), Poll::Pending, ready_event(2)]);
        let s = MultiEventStream::new(s);
        futures::pin_mut!(s);

        assert_eq!(
            s.next().await,
            Some(Ok(MultiCommandProgress {
                messages: vec![event(1)]
            })),
        );
    }

    #[tokio::test]
    async fn test_batch_limit() {
        let s = poll_fn(|_| ready_event(0));
        let s = MultiEventStream::new(s);
        futures::pin_mut!(s);

        // Make sure this actually finishes and lands on a reasonable message size. Note that
        // given the test messages are *very* small this will be off by quite a bit.
        assert_matches!(s.next().await, Some(Ok(msg)) => {
            assert!(msg.encoded_len() > PREFERRED_MESSAGE_SIZE_BYTES);
            assert!(msg.encoded_len() < 2 * PREFERRED_MESSAGE_SIZE_BYTES);
        })
    }
}
