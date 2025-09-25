/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Duration;

use dupe::Dupe;
use tokio::time;
use tokio::time::Instant;
use tokio::time::Interval;
use tokio::time::MissedTickBehavior;

/// A simple wrapper around a [Interval] that tracks information about start/elapsed time and tick numbers. Note
/// that ticks are not necessarily sequential, some may be skipped (and this indicates that ticks are running
/// slower than requested).
pub(crate) struct Ticker {
    interval: Interval,
    start_time: Instant,
}

impl Ticker {
    pub(crate) fn new(ticks_per_second: u32) -> Self {
        let interval_duration = Duration::from_secs_f64(1.0 / (ticks_per_second as f64));
        let mut interval = time::interval(interval_duration);
        interval.set_missed_tick_behavior(MissedTickBehavior::Skip);
        Self {
            interval,
            start_time: Instant::now(),
        }
    }

    pub(crate) async fn tick(&mut self) -> Tick {
        let current = self.interval.tick().await;
        self.tick_at(current)
    }

    pub(crate) fn tick_now(&mut self) -> Tick {
        self.tick_at(Instant::now())
    }

    fn tick_at(&mut self, current: Instant) -> Tick {
        // For time::interval, the Instant is the target instant for that tick and so it's possible
        // on the first one for it to actually be earlier than our start time.
        let elapsed_time = current
            .checked_duration_since(self.start_time)
            .unwrap_or(Duration::ZERO);

        Tick {
            start_time: self.start_time.into_std(),
            elapsed_time,
        }
    }
}

/// Information about tick timing.
#[derive(Debug, Clone, Dupe, Copy)]
pub struct Tick {
    /// The time that the ticker was started.
    pub start_time: std::time::Instant,
    /// Elapsed time since the ticker was started for this tick.
    pub elapsed_time: Duration,
}

impl Tick {
    pub(crate) fn now() -> Tick {
        Self {
            start_time: std::time::Instant::now(),
            elapsed_time: Duration::ZERO,
        }
    }
}
