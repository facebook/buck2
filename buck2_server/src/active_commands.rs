/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Mutex;

use buck2_events::dispatch::EventDispatcher;
use buck2_events::trace::TraceId;
use gazebo::prelude::*;
use once_cell::sync::Lazy;

static ACTIVE_COMMANDS: Lazy<Mutex<HashMap<TraceId, ActiveCommandHandle>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

/// Return the active commands, if you know what they are
pub fn active_commands() -> Option<HashMap<TraceId, ActiveCommandHandle>> {
    // Note that this function is accessed during panic, so have to be super careful
    Some(ACTIVE_COMMANDS.try_lock().ok()?.clone())
}

/// Allows interactions with commands found via active_commands().
#[derive(Clone, Dupe)]
pub struct ActiveCommandHandle {
    dispatcher: EventDispatcher,
}

pub struct ActiveCommandDropGuard {
    trace_id: TraceId,
}

impl ActiveCommandDropGuard {
    pub fn new(event_dispatcher: &EventDispatcher) -> Self {
        let trace_id = event_dispatcher.trace_id().dupe();
        let result = {
            // Scope the guard so it's locked as little as possible
            let mut active_commands = ACTIVE_COMMANDS.lock().unwrap();

            let existing_active_commands = if active_commands.len() > 1 {
                Some(active_commands.clone())
            } else {
                None
            };

            active_commands.insert(
                trace_id.dupe(),
                ActiveCommandHandle {
                    dispatcher: event_dispatcher.dupe(),
                },
            );

            existing_active_commands
        };

        if let Some(commands) = result {
            // Notify our command it is running concurrently with others.
            event_dispatcher.instant_event(buck2_data::TagEvent {
                tags: commands
                    .keys()
                    .map(|cmd| format!("concurrent_commands:{}", cmd))
                    .collect(),
            });

            // Notify other commands that they are concurrent with ours.
            for cmd in commands.values() {
                cmd.dispatcher.instant_event(buck2_data::TagEvent {
                    tags: vec![format!("concurrent_commands:{}", trace_id)],
                });
            }
        }

        Self { trace_id }
    }
}

impl Drop for ActiveCommandDropGuard {
    fn drop(&mut self) {
        ACTIVE_COMMANDS.lock().unwrap().remove(&self.trace_id);
    }
}
