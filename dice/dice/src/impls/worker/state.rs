/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The main worker thread for the dice task

use more_futures::cancellable_future::DisableCancellationGuard;
use more_futures::cancellation::CancellationContext;

use crate::impls::task::handle::DiceTaskHandle;
use crate::impls::value::DiceComputedValue;
use crate::result::CancellableResult;
use crate::result::Cancelled;

/// Represents when we are in a spawned dice task worker and are currently waiting for the previous
/// cancelled instance of this task to finish cancelling.
pub(crate) struct DiceWorkerStateAwaitingPrevious<'a> {
    internals: DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateAwaitingPrevious<'a> {
    pub(crate) fn new(handle: DiceTaskHandle<'a>) -> Self {
        debug!(msg = "Task started. Waiting for previously cancelled task if any");
        Self { internals: handle }
    }

    pub(crate) fn previously_finished(
        self,
        value: DiceComputedValue,
    ) -> DiceWorkerStateFinishedAndCached {
        debug!(msg = "previously cancelled task actually finished");

        self.internals.finished(value);

        DiceWorkerStateFinishedAndCached {}
    }

    pub(crate) fn previously_cancelled(self) -> DiceWorkerStateLookupNode<'a> {
        debug!(msg = "previously cancelled task was cancelled");

        self.internals.report_initial_lookup();

        DiceWorkerStateLookupNode {
            internals: self.internals,
        }
    }

    pub(crate) fn no_previous_task(self) -> DiceWorkerStateLookupNode<'a> {
        debug!(msg = "no previous task to wait for");

        self.internals.report_initial_lookup();

        DiceWorkerStateLookupNode {
            internals: self.internals,
        }
    }
}

/// Represents when we are currently looking up the current requested key from the core state, and
/// are waiting for it to respond.
pub(crate) struct DiceWorkerStateLookupNode<'a> {
    internals: DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateLookupNode<'a> {
    pub(crate) fn checking_deps(self) -> DiceWorkerStateCheckingDeps<'a> {
        self.internals.checking_deps();

        DiceWorkerStateCheckingDeps {
            internals: self.internals,
        }
    }

    pub(crate) fn lookup_dirtied(self) -> DiceWorkerStateComputing<'a> {
        self.internals.computing();

        DiceWorkerStateComputing {
            internals: self.internals,
        }
    }

    pub(crate) fn lookup_matches(
        self,
        value: DiceComputedValue,
    ) -> DiceWorkerStateFinishedAndCached {
        debug!(msg = "found existing entry with matching version in cache. reusing result.",);

        self.internals.finished(value);

        DiceWorkerStateFinishedAndCached {}
    }
}

/// When the spawned dice task worker is checking if the dependencies have changed since the last
/// time this node was verified, and are waiting for the results of the dependency re-computation.
pub(crate) struct DiceWorkerStateCheckingDeps<'a> {
    internals: DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateCheckingDeps<'a> {
    pub(crate) fn deps_not_match(self) -> DiceWorkerStateComputing<'a> {
        debug!(msg = "deps changed");
        self.internals.computing();

        DiceWorkerStateComputing {
            internals: self.internals,
        }
    }

    pub(crate) fn deps_match(self) -> CancellableResult<DiceWorkerStateFinished<'a>> {
        debug!(msg = "reusing previous value because deps didn't change. Updating caches");

        let guard = match self
            .internals
            .cancellation_ctx()
            .try_to_disable_cancellation()
        {
            Some(g) => g,
            None => {
                debug!("evaluation cancelled, skipping cache updates");
                return Err(Cancelled);
            }
        };

        Ok(DiceWorkerStateFinished {
            internals: self.internals,
            _prevent_cancellation: guard,
        })
    }

    #[cfg(test)]
    pub(crate) fn testing(k: crate::impls::key::DiceKey) -> Self {
        DiceWorkerStateCheckingDeps {
            internals: DiceTaskHandle::testing_new(k),
        }
    }
}

/// When the spawned dice worker is currently computing the requested Key.
pub(crate) struct DiceWorkerStateComputing<'a> {
    internals: DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateComputing<'a> {
    pub(crate) fn cancellation_ctx(&self) -> &CancellationContext {
        self.internals.cancellation_ctx()
    }

    pub(crate) fn finished(self) -> CancellableResult<DiceWorkerStateFinished<'a>> {
        debug!(msg = "evaluation finished. updating caches");

        let guard = match self
            .internals
            .cancellation_ctx()
            .try_to_disable_cancellation()
        {
            Some(g) => g,
            None => {
                debug!("evaluation cancelled, skipping cache updates");
                return Err(Cancelled);
            }
        };

        Ok(DiceWorkerStateFinished {
            internals: self.internals,
            _prevent_cancellation: guard,
        })
    }
}

/// When the spawned dice worker is finished checking dependencies or finished computing the key.
/// At this point, the value of the node is known. We are just waiting for core state to finish
/// updating the caches and return the correct instance of the value.
pub(crate) struct DiceWorkerStateFinished<'a> {
    internals: DiceTaskHandle<'a>,
    _prevent_cancellation: DisableCancellationGuard,
}

impl<'a> DiceWorkerStateFinished<'a> {
    pub(crate) fn cached(self, value: DiceComputedValue) -> DiceWorkerStateFinishedAndCached {
        debug!(msg = "Update caches complete");

        self.internals.finished(value);

        DiceWorkerStateFinishedAndCached {}
    }
}

/// When the spawned dice worker is done computing and saving the value to core state cache.
/// The final value is known.
pub(crate) struct DiceWorkerStateFinishedAndCached {}
