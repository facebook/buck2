/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_events::BuckEvent;

use crate::span_tracker::BuckEventSpanTracker;

pub struct EventObserver {
    span_tracker: BuckEventSpanTracker,
}

impl EventObserver {
    pub fn new() -> Self {
        Self {
            span_tracker: BuckEventSpanTracker::new(),
        }
    }

    pub fn observe(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        self.span_tracker.handle_event(event)
    }

    pub fn spans(&self) -> &BuckEventSpanTracker {
        &self.span_tracker
    }
}
