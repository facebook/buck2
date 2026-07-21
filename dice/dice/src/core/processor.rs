/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

#[allow(unused_imports)]
use gazebo::variants::VariantName;

use crate::core::graph::storage::ValueReusable;
use crate::core::internals::CoreState;
use crate::core::state::CoreStateHandle;
use crate::core::state::QueueCounters;
use crate::core::state::StateRequest;
use crate::impls::ctx::VersionEpochState;

pub(super) struct StateProcessor {
    state: CoreState,
    rx: tokio::sync::mpsc::UnboundedReceiver<StateRequest>,
    /// Shared with the matching `CoreStateHandle`; this thread bumps the
    /// `retired` counter after each successful receive.
    counters: Arc<QueueCounters>,
}

impl StateProcessor {
    pub(super) fn spawn() -> CoreStateHandle {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        let state = CoreState::new();
        let counters = Arc::new(QueueCounters::new());

        let processor_counters = counters.clone();
        std::thread::Builder::new()
            .name("buck2-dice".to_owned())
            .spawn(move || {
                StateProcessor {
                    state,
                    rx,
                    counters: processor_counters,
                }
                .event_loop()
            })
            .unwrap();

        CoreStateHandle::new(tx, counters)
    }

    fn event_loop(mut self) {
        loop {
            // Skip tokio scheduling.
            while let Ok(message) = self.rx.try_recv() {
                self.counters.record_retire();
                self.iteration(message);
            }
            if let Some(message) = self.rx.blocking_recv() {
                self.counters.record_retire();
                self.iteration(message);
            } else {
                break;
            }
        }
        debug!("Processor terminated");
    }

    #[cfg_attr(debug_assertions, instrument(skip_all, fields(kind = %message.variant_name())))]
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

                let ctx = VersionEpochState::new(version, version_epoch, cache);
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
                invalidation_paths,
                resp,
            } => {
                // ignore error if the requester dropped it.
                drop(resp.send(self.state.update_computed(
                    key,
                    epoch,
                    storage,
                    value,
                    ValueReusable::EqualityBased,
                    deps,
                    invalidation_paths,
                )));
            }
            StateRequest::UpdateMismatchAsUnchanged {
                key,
                epoch,
                storage,
                previous,
                invalidation_paths,
                resp,
            } => {
                // ignore error if the requester dropped it.
                drop(resp.send(self.state.update_computed(
                    key,
                    epoch,
                    storage,
                    previous.entry,
                    ValueReusable::VersionBased(previous.prev_verified_version),
                    previous.deps_to_validate,
                    invalidation_paths,
                )));
            }
            StateRequest::GetTasksPendingCancellation { resp } => {
                let _ignored = resp.send(self.state.get_tasks_pending_cancellation());
            }
            StateRequest::UnstableDropEverything => self.state.unstable_drop_everything(),
            StateRequest::PagedOutKeys { resp } => {
                drop(resp.send(Ok(self.state.paged_out_keys())));
            }
            StateRequest::EvictCachedValues { resp } => {
                self.state.evict_cached_values();
                let _ = resp.send(());
            }
            StateRequest::PagableStatus { resp } => {
                drop(resp.send(self.state.pagable_status()));
            }
            StateRequest::KeysToPageOut { resp } => {
                drop(resp.send(self.state.keys_to_page_out()));
            }
            StateRequest::EvictKeys { keys } => {
                self.state.evict_keys(keys);
            }
            StateRequest::MarkNonPageable { keys } => {
                self.state.mark_non_pageable(keys);
            }
            StateRequest::Rehydrate { key, value } => {
                self.state.rehydrate(key, value);
            }
            StateRequest::Metrics { resp } => {
                let _ignored = resp.send(self.state.metrics());
            }
            StateRequest::Introspection { resp } => {
                let _ignored = resp.send(self.state.introspection());
            }
            StateRequest::MakeAvailableForAllocative { resp } => {
                use std::sync::Arc;

                let (complete_tx, complete_rx) = tokio::sync::oneshot::channel();
                let state = std::mem::replace(&mut self.state, CoreState::new());
                let arc_state = Arc::new(state);
                drop(resp.send((Arc::clone(&arc_state), complete_tx)));
                drop(complete_rx.blocking_recv());
                // Correctness: Contract on `MakeAvailableForAllocative`
                let state =
                    Arc::into_inner(arc_state).expect("Other references to have been dropped");
                self.state = state;
            }
        }
    }
}
