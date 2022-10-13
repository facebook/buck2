use async_condvar_fair::Condvar;
use futures::future::Future;
use futures::future::FutureExt;
use parking_lot::Mutex;

/// A semaphore-like structure that locks up and stops emitting permits *at all* if the number of
/// requests exceeds the permit limit. This puts an upper bound on callers entering the critical
/// section at the capacity, but unlike a semaphore, the lower bound is zero, not the capacity.
pub struct LowPassFilter {
    /// Keeps track of the outstanding requests.
    state: Mutex<LowPassFilterState>,

    /// This is used to notify waiters when a new permit can be handed out.
    cv: Condvar,

    /// The (configured) limit for allowed concurrent accessors. If this is exceeded, no further
    /// permits will be issued until the accessors count goes back below the capacity. Permits that
    /// were issued aren't relinquished, but as they are released, no replacement permits will be
    /// issued until the accessors count goes below the capacity.
    capacity: usize,
}

struct LowPassFilterState {
    /// The total weight that has been requested through `access()`. This may be greater than the
    /// capacity.
    accessors: usize,
}

impl LowPassFilterState {
    fn can_dispatch_more(&self, capacity: usize) -> bool {
        self.accessors <= capacity
    }
}

impl LowPassFilter {
    pub fn new(capacity: usize) -> Self {
        Self {
            state: Mutex::new(LowPassFilterState { accessors: 0 }),
            cv: Condvar::new(),
            capacity,
        }
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Request a permit to enter the critical section.
    ///
    /// To make things predictable, we synchronously increment the accessors count. This ensures
    /// that there is no ambiguity about whether an accessor has already incremented the count or
    /// not depending on whether it was polled.
    #[allow(clippy::await_holding_lock)] // wait() actually releases the lock.
    #[allow(clippy::manual_async_fn)] // so you don't need to poll before access() takes effect
    pub fn access(&self, weight: usize) -> impl Future<Output = LowPassFilterGuard<'_>> {
        let go = {
            let mut state = self.state.lock();
            state.accessors += weight;
            state.can_dispatch_more(self.capacity)
        };

        // This needs to be created *here* and not at the return point so that if this future gets
        // dropped while we await (or possible en before we await), the guard will re-acquire the
        // mutex and update the accessors count.
        let guard = LowPassFilterGuard {
            filter: self,
            weight,
        };

        if go {
            return futures::future::ready(guard).left_future();
        }

        async move {
            let mut state = self.state.lock();
            loop {
                if state.can_dispatch_more(self.capacity) {
                    return guard;
                }
                state = self.cv.wait(state).await;
            }
        }
        .right_future()
    }
}

pub struct LowPassFilterGuard<'a> {
    filter: &'a LowPassFilter,
    weight: usize,
}

impl Drop for LowPassFilterGuard<'_> {
    /// When a guard is released, lower the accessors count, and if we are now low enough to
    /// proceed, notify a task. Note that this happens regardless of whether the owner of the
    /// LowPassFilterGuard had entered the critical section or not, since there is no difference
    /// between dropping a permit and dropping an outstading request for access.
    fn drop(&mut self) {
        let mut state = self.filter.state.lock();
        state.accessors -= self.weight;
        if state.can_dispatch_more(self.filter.capacity) {
            self.filter.cv.notify_one();
        }
    }
}

#[cfg(test)]
mod tests {
    use futures::future::FutureExt;

    use super::*;

    #[tokio::test]
    async fn test_access() {
        let filter = LowPassFilter::new(1);
        filter.access(1).await;
    }

    #[tokio::test]
    async fn test_access_weight() {
        let filter = LowPassFilter::new(2);
        let t0 = filter.access(2).await;

        let t1 = filter.access(2);
        futures::pin_mut!(t1);
        assert!(futures::poll!(t1.as_mut()).is_pending());

        drop(t0);
        assert!(futures::poll!(t1.as_mut()).is_ready());
    }

    #[tokio::test]
    async fn test_access_does_not_require_poll() {
        let filter = LowPassFilter::new(1);
        let t1 = filter.access(1).await;

        let t2 = filter.access(1);
        futures::pin_mut!(t2);
        assert!(futures::poll!(t2.as_mut()).is_pending());

        drop(t1);
        assert!(futures::poll!(t2.as_mut()).is_ready());
    }

    #[tokio::test]
    async fn test_access_many() {
        let filter = LowPassFilter::new(2);
        let _t1 = filter.access(1).await;
        let _t2 = filter.access(1).await;
    }

    #[tokio::test]
    async fn test_release() {
        let filter = LowPassFilter::new(1);
        let t0 = filter.access(1).await;
        let t1 = filter.access(1);
        futures::pin_mut!(t1);

        assert!(futures::poll!(t1.as_mut()).is_pending());

        drop(t0);
        assert!(futures::poll!(t1.as_mut()).is_ready());
    }

    #[tokio::test]
    async fn test_release_cancel() {
        let filter = LowPassFilter::new(1);
        let t0 = filter.access(1).await;

        // NOTE: We don't use pin_mut like above since we want to exercise cancellation here so we
        // need to be able to drop those futures.
        let mut t1 = filter.access(1).boxed();
        let mut t2 = filter.access(1).boxed();

        // We have 3 accessors, so we block.
        assert!(futures::poll!(t1.as_mut()).is_pending());
        assert!(futures::poll!(t2.as_mut()).is_pending());

        // We still have 2 accessors, so we block.
        drop(t0);
        assert!(futures::poll!(t1.as_mut()).is_pending());
        assert!(futures::poll!(t2.as_mut()).is_pending());

        // Now that we have only 1 accessor, we can proceed.
        drop(t1);
        assert!(futures::poll!(t2.as_mut()).is_ready());
    }
}
