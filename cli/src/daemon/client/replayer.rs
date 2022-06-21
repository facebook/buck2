/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;
use std::pin::Pin;
use std::time::SystemTime;

use futures::stream::BoxStream;
use futures::task::Poll;
use futures::Future;
use futures::Stream;
use pin_project::pin_project;
use tokio::time::Instant;
use tokio::time::Sleep;

use crate::commands::common::subscribers::event_log::EventLogPathBuf;
use crate::commands::common::subscribers::event_log::Invocation;
use crate::daemon::client::StreamValue;

#[pin_project]
struct Pending {
    #[pin]
    delay: Sleep,
    event: Option<buck2_data::BuckEvent>,
}

#[pin_project]
pub struct Replayer<T = BoxStream<'static, anyhow::Result<StreamValue>>> {
    #[pin]
    events: T,
    was_complete: bool,
    syncher: Syncher,
    #[pin]
    pending: Option<Pending>,
}

impl Replayer {
    pub(crate) async fn new(
        log_path: PathBuf,
        speed: Option<f64>,
    ) -> anyhow::Result<(Self, Invocation)> {
        let log_path = EventLogPathBuf::infer(log_path)?;
        let (invocation, events) = log_path.unpack_stream().await?;

        let syncher = Syncher::new(speed);

        let myself = Self {
            events: Box::pin(events),
            was_complete: false,
            pending: None,
            syncher,
        };

        Ok((myself, invocation))
    }

    pub(crate) fn speed(&self) -> f64 {
        self.syncher.speed
    }
}

/// Handle time drifting when replaying events - add pauses in between events to simulate the real deal.
struct Syncher {
    start: Option<(Instant, SystemTime)>,
    speed: f64,
}
impl Syncher {
    fn new(playback_speed: Option<f64>) -> Self {
        Self {
            start: None,
            speed: playback_speed.unwrap_or(1.0),
        }
    }

    /// Returns an appropriate delay for this event.
    ///
    /// The first event will be sent immediately. Each subsequent event will be sent with a delay
    /// based on its time since that first event.
    fn synch_playback_time(&mut self, event: &buck2_data::BuckEvent) -> anyhow::Result<Sleep> {
        let event_time = event.timestamp.as_ref().unwrap().clone().into();
        let (sync_start, log_start) = self.start.get_or_insert((Instant::now(), event_time));
        let log_offset_time = event_time.duration_since(*log_start)?;
        let sync_offset_time = log_offset_time.div_f64(self.speed);
        let sync_event_time = *sync_start + sync_offset_time;
        Ok(tokio::time::sleep_until(sync_event_time))
    }
}

impl<T> Stream for Replayer<T>
where
    T: Stream<Item = anyhow::Result<StreamValue>>,
{
    type Item = anyhow::Result<StreamValue>;

    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Option<Self::Item>> {
        // The basic idea is to hold on to the next event and a `sleep` until it's elapsed.
        // At this point, return the event.  Then grab a new one and a sleep and repeat the process.
        // Knowing whether the stream is done is dependent on whether the previous iteration either
        // erred or returned a result, so we keep track of that as well.

        let mut this = self.project();

        if *this.was_complete {
            return Poll::Ready(None);
        }

        if this.pending.is_none() {
            let event = futures::ready!(this.events.poll_next(cx));

            match event {
                Some(Ok(StreamValue::Event(event))) => {
                    let delay = this.syncher.synch_playback_time(&event)?;
                    this.pending.set(Some(Pending {
                        delay,
                        event: Some(event),
                    }));
                }
                // If the stream has errored out, finished, or contains a result, flag completion.
                _ => {
                    *this.was_complete = true;
                    return Poll::Ready(event);
                }
            }
        }

        // Unwrap is safe: we just checked this was Some.
        let pending = this.pending.as_mut().as_pin_mut().unwrap().project();

        futures::ready!(pending.delay.poll(cx));

        // Take the event and remove the Pending entry. This unwrap() is safe since if we have a
        // reference to this Pending the event cannnot have been taken yet, since we immediately
        // delete it on the next line.
        let event = pending.event.take().unwrap();
        this.pending.set(None);

        Poll::Ready(Some(Ok(StreamValue::Event(event))))
    }
}
