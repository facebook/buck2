/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::task::Context;
use std::task::Waker;

use dupe::Dupe;
use parking_lot::Mutex;

pub(crate) struct SharedState {
    inner: Arc<SharedStateData>,
}

impl SharedState {
    pub(crate) fn new() -> (Self, Self) {
        let data = Arc::new(SharedStateData {
            state: Mutex::new(State::Pending),
            cancelled: AtomicBool::new(false),
        });
        (Self { inner: data.dupe() }, Self { inner: data })
    }

    pub(crate) fn cancel(&self) -> bool {
        // Store to the boolean first before we write to state.
        // This is because on `poll`, the future will update the state first then check the boolean.
        // This ordering ensures that either the `poll` has read our cancellation, and hence will
        // later notify the termination observer via the channel we store in `State::Cancelled`,
        // or that we will observe the terminated state of the future and directly notify the
        // `TerminationObserver` ourselves.
        self.inner.cancelled.store(true, Ordering::SeqCst);

        let future = std::mem::replace(&mut *self.inner.state.lock(), State::Cancelled);
        match future {
            State::Pending => {
                // When the future starts, it'll see its cancellation;
                true
            }
            State::Polled { waker } => {
                waker.wake();
                true
            }
            State::Cancelled => {
                // We were already cancelled, no need to so again.
                false
            }
            State::Exited => {
                // Nothing to do, that future is done.
                true
            }
        }
    }

    pub(crate) fn is_cancelled(&self) -> bool {
        self.inner.cancelled.load(Ordering::SeqCst)
    }

    pub(crate) fn set_waker(&self, cx: &mut Context<'_>) {
        // we only update the Waker once at the beginning of the poll. For the same tokio
        // runtime, this is always safe and behaves correctly, as such, this future is
        // restricted to be ran on the same tokio executor and never moved from one runtime to
        // another
        let mut lock = self.inner.state.lock();
        if let State::Pending = &*lock {
            *lock = State::Polled {
                waker: cx.waker().clone(),
            }
        };
    }

    pub(crate) fn set_exited(&self) -> bool {
        let mut lock = self.inner.state.lock();
        let state = std::mem::replace(&mut *lock, State::Exited);
        match state {
            State::Cancelled => true,
            _ => false,
        }
    }
}

struct SharedStateData {
    state: Mutex<State>,

    /// When set, this future has been cancelled and should attempt to exit as soon as possible.
    cancelled: AtomicBool,
}

enum State {
    /// This future has been constructed, but not polled yet.
    Pending,

    /// This future has been polled. A waker is available.
    Polled { waker: Waker },

    /// This future has already been cancelled.
    Cancelled,

    /// This future has already finished executing.
    Exited,
}
