/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use gazebo::variants::VariantName;

use crate::impls::core::graph::storage::ValueReusable;
use crate::impls::core::internals::CoreState;
use crate::impls::core::state::CoreStateHandle;
use crate::impls::core::state::StateRequest;
use crate::impls::ctx::SharedLiveTransactionCtx;

pub(super) struct StateProcessor {
    state: CoreState,
    rx: tokio::sync::mpsc::UnboundedReceiver<StateRequest>,
}

impl StateProcessor {
    pub(super) fn spawn() -> CoreStateHandle {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        let state = CoreState::new();

        std::thread::Builder::new()
            .name("buck2-dice".to_owned())
            .spawn(move || StateProcessor { state, rx }.event_loop())
            .unwrap();

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

    #[instrument(skip_all, fields(kind = %message.variant_name()))]
    fn iteration(&mut self, message: StateRequest) {
        match message {
            StateRequest::UpdateState { changes, resp } => {
                // ignore error if the requester dropped it.
                let _ = resp.send(self.state.update_state(changes));
            }
            StateRequest::CtxAtVersion {
                version,
                guard,
                resp,
            } => {
                let (version_epoch, cache) = self.state.ctx_at_version(version);

                let ctx = SharedLiveTransactionCtx::new(version, version_epoch, cache);
                let _ignored = resp.send((ctx, guard));
            }
            StateRequest::DropCtxAtVersion { version } => self.state.drop_ctx_at_version(version),
            StateRequest::CurrentVersion { resp } => {
                // ignore error if the requester dropped it.
                let _ = resp.send(self.state.current_version());
            }
            StateRequest::LookupKey { key, resp } => drop(resp.send(self.state.lookup_key(key))),
            StateRequest::UpdateComputed {
                key,
                epoch,
                storage,
                value,
                deps,
                resp,
                ..
            } => {
                // ignore error if the requester dropped it.
                drop(resp.send(self.state.update_computed(
                    key,
                    epoch,
                    storage,
                    value,
                    ValueReusable::EqualityBased,
                    deps,
                )));
            }
            StateRequest::UpdateMismatchAsUnchanged {
                key,
                epoch,
                storage,
                previous,
                resp,
                ..
            } => {
                // ignore error if the requester dropped it.
                drop(resp.send(self.state.update_computed(
                    key,
                    epoch,
                    storage,
                    previous.entry,
                    ValueReusable::VersionBased(previous.verified_versions),
                    previous.deps_to_validate,
                )));
            }
            StateRequest::GetTasksPendingCancellation { resp } => {
                let _ignored = resp.send(self.state.get_tasks_pending_cancellation());
            }
            StateRequest::UnstableDropEverything => self.state.unstable_drop_everything(),
            StateRequest::Metrics { resp } => {
                let _ignored = resp.send(self.state.metrics());
            }
            StateRequest::Introspection { resp } => {
                let _ignored = resp.send(self.state.introspection());
            }
        }
    }
}
