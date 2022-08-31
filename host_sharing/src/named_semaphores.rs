// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

use dashmap::DashMap;
use futures_intrusive::sync::SharedSemaphore;

const SINGLE_WORKER: usize = 1;

#[derive(Default)]
pub struct NamedSemaphores {
    buckets: DashMap<String, SharedSemaphore>,
}

impl NamedSemaphores {
    pub fn new() -> NamedSemaphores {
        Default::default()
    }

    pub fn get(&self, name: &str) -> SharedSemaphore {
        if let Some(bucket_semaphore) = self.buckets.get(name) {
            bucket_semaphore.clone()
        } else {
            // Fairness on this semaphore doesn't control the order in which waiters are woken up, but
            // simply whether we delay wakeups to wait for waiters that want > 1 permit. Since we only
            // ever request a single permit here, the fairness doesn't matter.
            //
            // Since a fair semaphore will only ever wake a single waiter, that code path is a bit
            // faster and doesn't require any additional looping. Therefore despite the fact that a
            // fair and unfair semaphore whould have the same end result, we use a fair semaphore
            // here as a small performance optimization.
            let bucket_semaphore = SharedSemaphore::new(true, SINGLE_WORKER);
            self.buckets
                .insert(name.to_owned(), bucket_semaphore.clone());
            bucket_semaphore
        }
    }
}

#[cfg(test)]
mod tests {
    use super::NamedSemaphores;

    #[test]
    fn test_named_semaphore_works() {
        let named_semaphore = NamedSemaphores::new();
        let identifier = "testing".to_owned();
        {
            let first_permit = named_semaphore.get(&identifier);
            assert!(first_permit.try_acquire(1).is_some());
        }
        {
            let second_permit = named_semaphore.get(&identifier);
            assert!(second_permit.try_acquire(1).is_some());
        }
    }

    #[test]
    fn test_multiple_locks_fail() {
        let named_semaphore = NamedSemaphores::new();
        let identifier = "testing".to_owned();
        let first_permit = named_semaphore.get(&identifier);
        let first_permit_acquire = first_permit.try_acquire(1);
        assert!(first_permit_acquire.is_some());
        let second_permit = named_semaphore.get(&identifier);
        assert!(second_permit.try_acquire(1).is_none());
    }
}
