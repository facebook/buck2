use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;

pub trait ClaimManager: Send + Sync + 'static {
    fn try_claim(&self) -> bool;
}

impl dyn ClaimManager {
    pub fn new_simple() -> Arc<dyn ClaimManager> {
        Arc::new(AtomicBool::new(false))
    }
}

impl ClaimManager for AtomicBool {
    fn try_claim(&self) -> bool {
        !self.fetch_or(true, Ordering::SeqCst)
    }
}

#[derive(Debug)]
pub struct ClaimedRequest {}
