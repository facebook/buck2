/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use dupe::Dupe;
use thiserror::Error;
use tokio::sync::OwnedRwLockWriteGuard;
use tokio::sync::RwLock;

#[derive(Debug, Error, Copy, Clone, Dupe)]
#[error("LivelinessObserver reports this session is shutting down")]
struct NotAlive;

#[async_trait]
pub trait LivelinessObserver: Send + Sync {
    /// Pending while we are alive. Ready when we aren't.
    async fn while_alive(&self);
}

impl dyn LivelinessObserver {
    pub async fn is_alive(&self) -> bool {
        futures::poll!(self.while_alive()).is_pending()
    }

    pub async fn require_alive(&self) -> anyhow::Result<()> {
        if !self.is_alive().await {
            return Err(NotAlive.into());
        }
        Ok(())
    }
}

/// A liveliness manager with an implementation backed by an RW Lock. The way it works is as
/// follows:
///
/// - The LivelinessGuard holds a RW lock with write access.
/// - The `while_alive()` implementation attempts to acquire the lock with read access.
/// - This means that while the guard hasn't been dropped, the manager is considered alive.
///
/// We also allow the LivelinessGuard be "forgotten", which drops it but still forces `while_alive`
/// to stay pending (we do this by tracking this with a state flag in the mutex).
///
/// In an ideal world, this would be a newtype, but that means it needs to contain an `Arc<RwLock>`
/// to support `try_write_owned()`, and now we have 2 Arcs unnecessarily.
type LivelinessObserverForGuard = RwLock<LivelinessObserverState>;

pub struct LivelinessGuard {
    guard: OwnedRwLockWriteGuard<LivelinessObserverState>,

    // A reference to the underyling manager to support `cancel`.
    manager: Arc<LivelinessObserverForGuard>,
}

impl LivelinessGuard {
    pub fn create() -> (Arc<dyn LivelinessObserver>, LivelinessGuard) {
        let manager = Arc::new(LivelinessObserverForGuard::new(
            LivelinessObserverState::AliveWhenLocked,
        ));

        let guard = manager
            .dupe()
            .try_write_owned()
            .expect("This lock was just created");

        (manager.dupe() as _, LivelinessGuard { guard, manager })
    }

    /// Declare that this liveliness manager is no longer alive. Dropping the guard does the same,
    /// but this allows potentially restoring it later.
    pub fn cancel(self) -> CancelledLivelinessGuard {
        CancelledLivelinessGuard {
            manager: self.manager,
        }
    }

    /// Declare that the underlying liveliness manager should stay alive forever, even when we drop
    /// this guard.
    pub fn forget(mut self) {
        *self.guard = LivelinessObserverState::ForeverAlive;
    }
}

#[derive(Debug)]
pub struct CancelledLivelinessGuard {
    manager: Arc<LivelinessObserverForGuard>,
}

impl CancelledLivelinessGuard {
    /// If the lock is available, re-acquire it, thus allowing things to stay alive again.
    pub fn restore(self) -> Option<LivelinessGuard> {
        let guard = self.manager.dupe().try_write_owned().ok()?;
        Some(LivelinessGuard {
            guard,
            manager: self.manager,
        })
    }
}

/// The state of this liveliness manager. By default, it's alive if and only if the LivelinessGuard
/// is holding the lock, but if `forget` was called, then we'll record that even upon a successful
/// lock acquisition, the manager should be considered alive.
#[derive(Debug)]
enum LivelinessObserverState {
    AliveWhenLocked,
    ForeverAlive,
}

#[async_trait]
impl LivelinessObserver for LivelinessObserverForGuard {
    async fn while_alive(&self) {
        match *self.read().await {
            LivelinessObserverState::AliveWhenLocked => {}
            LivelinessObserverState::ForeverAlive => futures::future::pending().await,
        }
    }
}

/// Always alive.
pub struct NoopLivelinessObserver;

impl NoopLivelinessObserver {
    pub fn create() -> Arc<dyn LivelinessObserver> {
        Arc::new(Self) as _
    }
}

#[async_trait]
impl LivelinessObserver for NoopLivelinessObserver {
    async fn while_alive(&self) {
        futures::future::pending().await
    }
}

#[async_trait]
impl LivelinessObserver for Arc<dyn LivelinessObserver> {
    async fn while_alive(&self) {
        self.as_ref().while_alive().await
    }
}

pub struct LivelinessAnd<A, B> {
    a: A,
    b: B,
}

#[async_trait]
impl<A, B> LivelinessObserver for LivelinessAnd<A, B>
where
    A: LivelinessObserver,
    B: LivelinessObserver,
{
    async fn while_alive(&self) {
        let a = self.a.while_alive();
        let b = self.b.while_alive();
        futures::pin_mut!(a);
        futures::pin_mut!(b);
        futures::future::select(a, b).await;
    }
}

pub trait LivelinessObserverExt: Sized {
    fn and<B>(self, b: B) -> LivelinessAnd<Self, B>;
}

impl<T> LivelinessObserverExt for T
where
    T: LivelinessObserver + Sized,
{
    fn and<B>(self, b: B) -> LivelinessAnd<Self, B> {
        LivelinessAnd { a: self, b }
    }
}

#[async_trait]
impl LivelinessObserver for more_futures::cancellable_future::CancellationObserver {
    async fn while_alive(&self) {
        self.dupe().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_guard_is_alive() {
        let (manager, guard) = LivelinessGuard::create();
        assert!(manager.is_alive().await);
        drop(guard);
        assert!(!manager.is_alive().await);
    }

    #[tokio::test]
    async fn test_and() {
        let (manager_a, guard) = LivelinessGuard::create();
        let manager_b = NoopLivelinessObserver::create();

        let manager = manager_a.and(manager_b);
        let manager = &manager as &dyn LivelinessObserver;

        assert!(manager.is_alive().await);
        drop(guard);
        assert!(!manager.is_alive().await);
    }

    #[tokio::test]
    async fn test_cancel_restore_forget() {
        let (manager, guard) = LivelinessGuard::create();
        assert!(manager.is_alive().await);

        let cancelled = guard.cancel();
        assert!(!manager.is_alive().await);

        let restored = cancelled.restore().expect("This is not currently held");
        assert!(manager.is_alive().await);

        restored.forget();
        assert!(manager.is_alive().await);
    }
}
