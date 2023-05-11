/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;

use dupe::Dupe;
use futures::future::BoxFuture;
use futures::future::Shared;
use more_futures::cancellation::future::TerminationObserver;
use more_futures::spawn::spawn_cancellable;
use more_futures::spawn::FutureAndCancellationHandle;
use more_futures::spawner::Spawner;

use crate::impls::task::dice::Cancellations;
use crate::impls::task::dice::DiceTask;
use crate::impls::task::dice::DiceTaskInternal;
use crate::impls::task::handle::DiceTaskHandle;

pub(crate) mod dice;
pub(crate) mod handle;
pub(crate) mod promise;
mod state;

#[cfg(test)]
mod tests;

pub(crate) fn spawn_dice_task<S>(
    spawner: &dyn Spawner<S>,
    ctx: &S,
    f: impl for<'a> FnOnce(DiceTaskHandle<'a>) -> BoxFuture<'a, Box<dyn Any + Send>> + Send + 'static,
) -> DiceTask {
    let internal = DiceTaskInternal::new();

    let span = debug_span!(parent: None, "spawned_dice_task",);

    // since the spawn is alive until cancelled via the handle, we can drop the spawn future itself
    let FutureAndCancellationHandle {
        cancellation_handle,
        ..
    } = spawn_cancellable(
        {
            let internal = internal.dupe();
            |cancellations| {
                let handle = DiceTaskHandle {
                    internal,
                    cancellations,
                };

                f(handle)
            }
        },
        spawner,
        ctx,
        span,
    );

    DiceTask {
        internal,
        cancellations: Cancellations::new(cancellation_handle),
    }
}

/// Unsafe as this creates a Task that must be completed explicitly otherwise polling will never
/// complete.
pub(crate) unsafe fn sync_dice_task() -> DiceTask {
    let internal = DiceTaskInternal::new();

    DiceTask {
        internal,
        cancellations: Cancellations::not_cancellable(),
    }
}

pub(crate) struct PreviouslyCancelledTask {
    pub(crate) previous: DiceTask,
    pub(crate) termination: Shared<TerminationObserver>,
}
