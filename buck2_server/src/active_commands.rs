/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::sync::Mutex;

use buck2_events::trace::TraceId;
use once_cell::sync::Lazy;

static ACTIVE_COMMANDS: Lazy<Mutex<HashSet<TraceId>>> = Lazy::new(|| Mutex::new(HashSet::new()));

/// Return the active commands, if you know what they are
pub fn active_commands() -> Option<HashSet<TraceId>> {
    // Note that this function is accessed during panic, so have to be super careful
    Some(ACTIVE_COMMANDS.try_lock().ok()?.clone())
}

pub struct ActiveCommandDropGuard {
    trace_id: TraceId,
}

impl ActiveCommandDropGuard {
    pub fn new(trace_id: TraceId) -> Self {
        Self { trace_id }
    }
}

impl Drop for ActiveCommandDropGuard {
    fn drop(&mut self) {
        ACTIVE_COMMANDS.lock().unwrap().remove(&self.trace_id);
    }
}
