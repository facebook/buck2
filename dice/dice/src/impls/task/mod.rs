/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;

use futures::future::BoxFuture;
use more_futures::spawn::spawn_cancellable;
use more_futures::spawn::FutureAndCancellationHandle;
use more_futures::spawner::Spawner;

use crate::impls::task::dice::DiceTask;
use crate::impls::task::dice::DiceTaskInternal;
use crate::impls::task::handle::DiceTaskHandle;
use crate::impls::triomphe_dupe;

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
            let internal = triomphe_dupe(&internal);
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
        cancellation_handle: Some(cancellation_handle),
    }
}

/// Unsafe as this creates a Task that must be completed explicitly otherwise polling will never
/// complete.
pub(crate) unsafe fn sync_dice_task() -> DiceTask {
    let internal = DiceTaskInternal::new();

    DiceTask {
        internal,
        cancellation_handle: None,
    }
}
