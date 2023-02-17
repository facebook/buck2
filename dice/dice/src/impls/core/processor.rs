/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use gazebo::variants::VariantName;

use crate::impls::core::internals::CoreState;
use crate::impls::core::state::CoreStateHandle;
use crate::impls::core::state::StateRequest;

pub(super) struct StateProcessor {
    state: CoreState,
    rx: tokio::sync::mpsc::UnboundedReceiver<StateRequest>,
}

impl StateProcessor {
    pub(super) fn spawn() -> CoreStateHandle {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        let state = CoreState {};

        std::thread::spawn(move || StateProcessor { state, rx }.event_loop());
        CoreStateHandle::new(tx)
    }

    fn event_loop(mut self) {
        loop {
            // Skip tokio scheduling.
            while let Ok(message) = self.rx.try_recv() {
                self.iteration(message);
            }
            if let Some(message) = self.rx.blocking_recv() {
                self.iteration(message);
            } else {
                break;
            }
        }
        debug!("Processor terminated");
    }

    #[cfg_attr(fbcode_build, instrument(skip_all, fields(kind = %message.variant_name())))]
    fn iteration(&mut self, message: StateRequest) {
        match message {
            StateRequest::Todo => {}
        }
    }
}
