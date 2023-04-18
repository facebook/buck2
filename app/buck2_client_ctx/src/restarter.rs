/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::daemon::client::connect::DaemonConstraintsRequest;
use crate::daemon::client::BuckdClientConnector;

/// Monitor the state of our execution and decide whether we should restart the command we just
/// attempted to execute.
pub struct Restarter {
    pub reject_daemon: Option<String>,
    pub reject_materializer_state: Option<String>,
    pub enable_restarter: bool,
}

impl Restarter {
    pub fn new() -> Self {
        Self {
            reject_daemon: None,
            reject_materializer_state: None,
            enable_restarter: false,
        }
    }

    /// Observe our BuckdClientConnector after execution to decide whether we should be
    /// restarting.
    pub fn observe(&mut self, client: &BuckdClientConnector) {
        for obs in client.error_observers() {
            if obs.daemon_in_memory_state_is_corrupted() {
                self.reject_daemon = Some(client.daemon_constraints().daemon_id.clone());
            }

            if obs.daemon_materializer_state_is_corrupted() {
                self.reject_materializer_state = client
                    .daemon_constraints()
                    .extra
                    .as_ref()
                    .and_then(|e| e.materializer_state_identity.clone());
            }

            if obs.restarter_is_enabled() {
                self.enable_restarter = true;
            }
        }
    }

    pub fn should_restart(&self) -> bool {
        self.enable_restarter
            && (self.reject_daemon.is_some() || self.reject_materializer_state.is_some())
    }

    pub fn apply_to_constraints(&self, req: &mut DaemonConstraintsRequest) {
        req.reject_daemon = self.reject_daemon.clone();
        req.reject_materializer_state = self.reject_materializer_state.clone();
    }
}
