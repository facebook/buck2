/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::future::Future;
use std::io;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;
use std::task::ready;
use std::time::Duration;
use std::time::Instant;

use tokio::io::AsyncRead;
use tokio::io::ReadBuf;
use tokio::sync::watch;
use tokio::time::Sleep;

/// How long to keep polling for growth once the writer is reported finished: enough for a
/// final flush already in flight to land, without waiting out the full `idle_timeout`.
const WRITER_FINISHED_GRACE: Duration = Duration::from_secs(1);

/// External hint about the state of the process writing the log, e.g. from a daemon's
/// view of its running commands.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum WriterState {
    /// Nothing is known about the writer; `idle_timeout` alone decides when to give up.
    Unknown,
    /// The writer is known to still be running: keep polling through quiet stretches
    /// instead of reporting end-of-file at `idle_timeout`.
    Running,
    /// The writer is known to be finished: report end-of-file after a short grace
    /// period without growth instead of waiting out `idle_timeout`.
    Finished,
}

/// Controls how a tailing reader waits for an event log that is still being written.
#[derive(Clone, Debug)]
pub struct TailOptions {
    /// How long to wait before checking for new data again after reaching the end of the file.
    pub poll_interval: Duration,
    /// Report end-of-file if the file has not grown for this long. `None` waits forever.
    pub idle_timeout: Option<Duration>,
    /// External hint about the writer; see [`WriterState`]. The state may change in either
    /// direction while tailing (e.g. a command missing from one daemon snapshot can appear
    /// again in the next).
    pub writer_state: Option<watch::Receiver<WriterState>>,
}

/// A reader that does not treat the end of the file as the end of the data: the file is
/// expected to still be growing, so wait and check for more instead.
///
/// This only ever uses the already-open handle; the file is never re-opened, locked, or
/// otherwise held beyond that handle. The writer owns the file and remains free to delete
/// it (opening with the Rust standard library shares delete access on Windows); a deleted
/// file simply stops producing data, which ends the tail via `idle_timeout` (or via the
/// writer-state hint reporting the writer finished).
pub(crate) struct TailReader<T> {
    inner: T,
    options: TailOptions,
    sleep: Option<Pin<Box<Sleep>>>,
    /// When the file last stopped growing; `None` while data is flowing.
    idle_since: Option<Instant>,
}

impl<T: AsyncRead + Unpin> TailReader<T> {
    pub(crate) fn new(inner: T, options: TailOptions) -> Self {
        Self {
            inner,
            options,
            sleep: None,
            idle_since: None,
        }
    }
}

impl<T: AsyncRead + Unpin> AsyncRead for TailReader<T> {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        let this = self.get_mut();
        loop {
            if let Some(sleep) = &mut this.sleep {
                ready!(sleep.as_mut().poll(cx));
                this.sleep = None;
            }

            let filled_before = buf.filled().len();
            ready!(Pin::new(&mut this.inner).poll_read(cx, buf))?;
            if buf.filled().len() > filled_before {
                this.idle_since = None;
                return Poll::Ready(Ok(()));
            }

            let now = Instant::now();
            let idle_since = *this.idle_since.get_or_insert(now);
            let idle_timeout = match this.options.writer_state.as_ref().map(|s| *s.borrow()) {
                Some(WriterState::Finished) => Some(
                    this.options
                        .idle_timeout
                        .map_or(WRITER_FINISHED_GRACE, |t| t.min(WRITER_FINISHED_GRACE)),
                ),
                // A known-live writer that is merely quiet is not end-of-file.
                Some(WriterState::Running) => None,
                Some(WriterState::Unknown) | None => this.options.idle_timeout,
            };
            if let Some(idle_timeout) = idle_timeout
                && now.duration_since(idle_since) >= idle_timeout
            {
                return Poll::Ready(Ok(()));
            }
            this.sleep = Some(Box::pin(tokio::time::sleep(this.options.poll_interval)));
        }
    }
}
