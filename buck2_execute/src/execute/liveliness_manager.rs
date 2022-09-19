use std::sync::Arc;

use async_trait::async_trait;

#[async_trait]
pub trait LivelinessManager: Send + Sync {
    /// Pending while we are alive. Ready when we aren't.
    async fn while_alive(&self);
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
