use std::sync::Arc;

use async_trait::async_trait;
use gazebo::prelude::*;
use tokio::sync::OwnedRwLockWriteGuard;
use tokio::sync::RwLock;

#[async_trait]
pub trait LivelinessManager: Send + Sync {
    /// Pending while we are alive. Ready when we aren't.
    async fn while_alive(&self);
}

impl dyn LivelinessManager {
    pub async fn while_alive_owned(self: Arc<Self>) {
        self.while_alive().await
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
