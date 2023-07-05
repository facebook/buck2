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
use futures::FutureExt;
use more_futures::owning_future::OwningFuture;
use more_futures::spawn::spawn_cancellable;
use more_futures::spawn::FutureAndCancellationHandle;
use more_futures::spawner::Spawner;

use crate::impls::key::DiceKey;
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
    key: DiceKey,
    spawner: &dyn Spawner<S>,
    ctx: &S,
    f: impl for<'a> FnOnce(&'a mut DiceTaskHandle<'_>) -> BoxFuture<'a, Box<dyn Any + Send>> + Send,
) -> DiceTask {
    let internal = DiceTaskInternal::new(key);

    // since the spawn is alive until cancelled via the handle, we can drop the spawn future itself
    let FutureAndCancellationHandle {
        cancellation_handle,
        ..
    } = spawn_cancellable(
        {
            let internal = internal.dupe();
            |cancellations| {
                let handle = DiceTaskHandle::new(internal, cancellations);

                OwningFuture::new(handle, f).boxed()
            }
        },
        spawner,
        ctx,
    );

    DiceTask {
        internal,
        cancellations: Cancellations::new(cancellation_handle),
    }
}

/// Unsafe as this creates a Task that must be completed explicitly otherwise polling will never
/// complete.
pub(crate) unsafe fn sync_dice_task(key: DiceKey) -> DiceTask {
    let internal = DiceTaskInternal::new(key);

    DiceTask {
        internal,
        cancellations: Cancellations::not_cancellable(),
    }
}

pub(crate) struct PreviouslyCancelledTask {
    pub(crate) previous: DiceTask,
}
