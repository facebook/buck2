use std::fmt;
use std::sync::Arc;

use async_trait::async_trait;
use derivative::Derivative;
use derive_more::Display;
use gazebo::prelude::*;
use tokio::sync::Mutex;
use tokio::sync::OwnedMutexGuard;

#[async_trait]
pub trait ClaimManager: Send + Sync + 'static {
    /// Acquire a claim. the Claim mamanger is normally internally mutable and will likely lock out
    /// other executions from acquiring a claim.
    ///
    /// Actions *must* acquire a claim before writing to outputs for an action or declaring
    /// output artifacts in the materializer.
    ///
    /// The expectation is that multiple actions that attempt to write to the same output directory
    /// should not be allowed to obtain claims concurrently.
    async fn claim(self: Box<Self>) -> Option<Box<dyn Claim>>;
}

pub trait Claim: Send + Sync + fmt::Debug + 'static {
    /// Release a claim. This can be done to make it available again after acquiring it. Once the
    /// claim is released, the claim holder should no longer write any output.
    fn release(self: Box<Self>) -> anyhow::Result<()>;
}

#[derive(Debug)]
pub enum ClaimStatus {
    // No claim has been attempted, or one was attempted but abandoned.
    NotClaimed,
    // A claim has been made.
    Claimed,
}

#[derive(Dupe, Clone)]
pub struct MutexClaimManager {
    mutex: Arc<Mutex<ClaimStatus>>,
}

impl MutexClaimManager {
    pub fn new() -> Self {
        Self {
            mutex: Arc::new(Mutex::new(ClaimStatus::NotClaimed)),
        }
    }
}

#[async_trait]
impl ClaimManager for MutexClaimManager {
    async fn claim(self: Box<Self>) -> Option<Box<dyn Claim>> {
        let mut guard = self.mutex.dupe().lock_owned().await;
        match *guard {
            ClaimStatus::NotClaimed => {}
            ClaimStatus::Claimed => return None,
        }
        *guard = ClaimStatus::Claimed;
        Some(box MutexClaim { guard })
    }
}

#[derive(Display, Derivative)]
#[display(fmt = "MutexClaim")]
#[derivative(Debug)]
pub struct MutexClaim {
    // No point in printing this as it will *always* be Claimed.
    #[derivative(Debug = "ignore")]
    guard: OwnedMutexGuard<ClaimStatus>,
}

impl Claim for MutexClaim {
    fn release(mut self: Box<Self>) -> anyhow::Result<()> {
        *self.guard = ClaimStatus::NotClaimed;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::task::Poll;

    use assert_matches::assert_matches;

    use super::*;

    #[tokio::test]
    async fn test_mutex_claim_release() {
        let claim_manager = MutexClaimManager::new();
        let claim = (box claim_manager.dupe())
            .claim()
            .await
            .expect("Have a claim");

        let claim2 = (box claim_manager.dupe()).claim();
        futures::pin_mut!(claim2);
        assert_matches!(futures::poll!(claim2.as_mut()), Poll::Pending);

        claim.release().expect("Can release claim");
        assert_matches!(futures::poll!(claim2.as_mut()), Poll::Ready(Some(..)));
    }
}
