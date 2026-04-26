/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

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
        // Fast path: a read-locked shard lookup, no allocation, no write lock.
        if let Some(bucket_semaphore) = self.buckets.get(name) {
            return bucket_semaphore.clone();
        }
        // Slow path: atomically insert-if-absent. Using `entry().or_insert_with(..)`
        // (rather than a separate `get` followed by `insert`) is required for
        // correctness: the previous check-then-act pattern was racy and could let
        // two concurrent callers each install a different `SharedSemaphore` for
        // the same name, breaking the "OnePerToken" exclusivity guarantee that
        // `HostSharingBroker` relies on.
        //
        // Fairness on this semaphore doesn't control the order in which waiters
        // are woken up, but simply whether we delay wakeups to wait for waiters
        // that want > 1 permit. Since we only ever request a single permit here,
        // the fairness doesn't matter for correctness.
        //
        // Since a fair semaphore will only ever wake a single waiter, that code
        // path is a bit faster and doesn't require any additional looping.
        // Therefore despite the fact that a fair and unfair semaphore would have
        // the same end result, we use a fair semaphore here as a small
        // performance optimization.
        self.buckets
            .entry(name.to_owned())
            .or_insert_with(|| SharedSemaphore::new(true, SINGLE_WORKER))
            .value()
            .clone()
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

    // Regression test: `NamedSemaphores::get` previously did a non-atomic
    // check-then-act on the underlying DashMap (a `get` followed by an
    // `insert`), which let two concurrent callers each install a different
    // `SharedSemaphore` for the same name. That silently broke the
    // "OnePerToken" exclusivity guarantee. This test races N threads on the
    // same name and asserts that all returned clones share one underlying
    // semaphore: only the first `try_acquire(1)` may succeed.
    #[test]
    fn test_get_is_atomic_under_contention() {
        use std::sync::Arc;
        use std::sync::Barrier;

        const N: usize = 32;
        // Repeat to make the (probabilistic) race more likely to fire on
        // the racy implementation; deterministic on the fixed one.
        for _ in 0..32 {
            let named = Arc::new(NamedSemaphores::new());
            let barrier = Arc::new(Barrier::new(N));

            let handles: Vec<_> = (0..N)
                .map(|_| {
                    let named = Arc::clone(&named);
                    let barrier = Arc::clone(&barrier);
                    std::thread::spawn(move || {
                        barrier.wait();
                        named.get("contended")
                    })
                })
                .collect();

            let semaphores: Vec<_> =
                handles.into_iter().map(|h| h.join().unwrap()).collect();

            let _held = semaphores[0]
                .try_acquire(1)
                .expect("first acquire must succeed");
            for (i, s) in semaphores[1..].iter().enumerate() {
                assert!(
                    s.try_acquire(1).is_none(),
                    "all clones must share the same underlying semaphore \
                     (clone #{} acquired a permit while clone #0 was holding one)",
                    i + 1,
                );
            }
        }
    }
}
