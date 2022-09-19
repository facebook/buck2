use std::sync::Arc;

use async_trait::async_trait;
use futures::Future;
use gazebo::prelude::*;
use thiserror::Error;
use tokio::sync::OwnedRwLockWriteGuard;
use tokio::sync::RwLock;

#[derive(Error)]
#[error("LivelinessManager reports this session is shutting down")]
struct NotAlive;

#[async_trait]
pub trait LivelinessManager: Send + Sync {
    /// Pending while we are alive. Ready when we aren't.
    async fn while_alive(&self);
}

impl dyn LivelinessManager {
    pub fn while_alive_owned(self: Arc<Self>) -> impl Future<Output = ()> + Send + 'static {
        async move { self.while_alive().await }
    }

    pub async fn is_alive(&self) -> bool {
        futures::poll!(self.while_alive()).is_pending()
    }

    pub async fn require_alive(&self) -> anyhow::Result<()> {
        if !self.is_alive() {
            return Err(NotAlive);
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
}
