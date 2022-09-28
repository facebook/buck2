use std::sync::Arc;

use async_trait::async_trait;
use gazebo::prelude::*;
use thiserror::Error;
use tokio::sync::OwnedRwLockWriteGuard;
use tokio::sync::RwLock;

#[derive(Debug, Error, Copy, Clone, Dupe)]
#[error("LivelinessManager reports this session is shutting down")]
struct NotAlive;

#[async_trait]
pub trait LivelinessManager: Send + Sync {
    /// Pending while we are alive. Ready when we aren't.
    async fn while_alive(&self);
}

impl dyn LivelinessManager {
    pub async fn while_alive_owned(self: Arc<Self>) {
        self.while_alive().await
    }

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

/// While the associated LivelinessGuard is alive, `while_alive` will be Pending. When the guard is
/// dropped, that stops being the case.
#[derive(Clone, Dupe)]
pub struct GuardedLivelinessManager {
    mutex: Arc<RwLock<()>>,
}

pub struct LivelinessGuard {
    // This is only used for the fact that it releases the lock on drop.
    _guard: OwnedRwLockWriteGuard<LivelinessGuardMarker>,
}

impl LivelinessGuard {
    pub fn create() -> (Arc<dyn LivelinessManager>, LivelinessGuard) {
        let mutex = Arc::new(RwLock::new(LivelinessGuardMarker));

        let guard = mutex
            .dupe()
            .try_write_owned()
            .expect("This mutex was just created");

        (mutex as _, LivelinessGuard { _guard: guard })
    }
}

/// Just a type that can't be constructed elesewhere
struct LivelinessGuardMarker;

#[async_trait]
impl LivelinessManager for RwLock<LivelinessGuardMarker> {
    async fn while_alive(&self) {
        let _unused = self.read().await;
    }
}

/// Always alive.
pub struct NoopLivelinessManager;

impl NoopLivelinessManager {
    pub fn create() -> Arc<dyn LivelinessManager> {
        Arc::new(Self) as _
    }
}

#[async_trait]
impl LivelinessManager for NoopLivelinessManager {
    async fn while_alive(&self) {
        futures::future::pending().await
    }
}

#[async_trait]
impl LivelinessManager for Arc<dyn LivelinessManager> {
    async fn while_alive(&self) {
        self.as_ref().while_alive().await
    }
}

pub struct LivelinessAnd<A, B> {
    a: A,
    b: B,
}

#[async_trait]
impl<A, B> LivelinessManager for LivelinessAnd<A, B>
where
    A: LivelinessManager,
    B: LivelinessManager,
{
    async fn while_alive(&self) {
        let a = self.a.while_alive();
        let b = self.b.while_alive();
        futures::pin_mut!(a);
        futures::pin_mut!(b);
        futures::future::select(a, b).await;
    }
}

pub trait LivelinessManagerExt: Sized {
    fn and<B>(self, b: B) -> LivelinessAnd<Self, B>;
}

impl<T> LivelinessManagerExt for T
where
    T: LivelinessManager + Sized,
{
    fn and<B>(self, b: B) -> LivelinessAnd<Self, B> {
        LivelinessAnd { a: self, b }
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
        let manager_b = NoopLivelinessManager::create();

        let manager = manager_a.and(manager_b);
        let manager = &manager as &dyn LivelinessManager;

        assert!(manager.is_alive().await);
        drop(guard);
        assert!(!manager.is_alive().await);
    }
}
