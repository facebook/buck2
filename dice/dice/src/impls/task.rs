/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::any::Any;

use dice_futures::owning_future::OwningFuture;
use dice_futures::spawn::spawn_dropcancel;
use dice_futures::spawner::Spawner;
use dupe::Dupe;
use futures::FutureExt;
use futures::future::BoxFuture;

use crate::impls::key::DiceKey;
use crate::impls::task::critical::CancellationState;
use crate::impls::task::dice::DiceTask;
use crate::impls::task::dice::DiceTaskInternal;
use crate::impls::task::handle::DiceTaskHandle;

pub(crate) mod critical;
pub(crate) mod dice;
pub(crate) mod handle;
pub(crate) mod promise;
mod state;
pub(crate) use state::DiceTaskState;

#[cfg(test)]
mod tests;

pub(crate) fn spawn_dice_task<S>(
    key: DiceKey,
    spawner: &dyn Spawner<S>,
    ctx: &S,
    f: impl for<'a, 'b> FnOnce(&'a mut DiceTaskHandle<'b>) -> BoxFuture<'a, Box<dyn Any + Send>> + Send,
) -> DiceTask {
    let internal = DiceTaskInternal::new(key, CancellationState::Pending);

    let (_fut, cancellation_handle) = spawn_dropcancel(
        {
            let internal = internal.dupe();
            |cancellations| {
                let handle = DiceTaskHandle::new(internal, cancellations);
                OwningFuture::new(handle, f).boxed()
            }
        },
        spawner,
        ctx,
    )
    .detach();

    internal.set_cancellation_handle(cancellation_handle);

    DiceTask::new(internal)
}

/// Unsafe as this creates a Task that must be completed explicitly otherwise polling will never
/// complete.
pub(crate) unsafe fn sync_dice_task(key: DiceKey) -> DiceTask {
    DiceTask::new(DiceTaskInternal::new(
        key,
        CancellationState::NotCancellable,
    ))
}

pub(crate) struct PreviouslyCancelledTask {
    previous: DiceTask,
}

impl PreviouslyCancelledTask {
    pub(crate) fn new(previous: DiceTask) -> Self {
        Self { previous }
    }

    pub(crate) fn await_termination(&self) -> crate::impls::task::dice::TerminationObserver {
        self.previous.await_termination()
    }

    pub(crate) fn get_finished_value(
        &self,
    ) -> Option<dice_error::result::CancellableResult<crate::impls::value::DiceComputedValue>> {
        self.previous.get_finished_value()
    }
}
