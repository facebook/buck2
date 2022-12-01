/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::BuckEvent;
use crate::ControlEvent;
use crate::EventSink;

/// A null EventSink that discards all messages sent to it.
pub struct NullEventSink;

impl NullEventSink {
    pub fn new() -> NullEventSink {
        NullEventSink
    }
}

impl Default for NullEventSink {
    fn default() -> Self {
        NullEventSink::new()
    }
}

impl EventSink for NullEventSink {
    fn send(&self, _: BuckEvent) {}

    fn send_control(&self, _: ControlEvent) {}
}
