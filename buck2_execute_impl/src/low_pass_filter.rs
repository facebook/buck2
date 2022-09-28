use async_condvar_fair::Condvar;
use futures::future::Future;
use futures::future::FutureExt;
use parking_lot::Mutex;

struct LowPassFilterState {
    accessors: usize,
    capacity: usize,
}

impl LowPassFilterState {
    fn dispatch_more(&self) -> bool {
        self.accessors <= self.capacity
    }
}

pub struct LowPassFilter {
    state: Mutex<LowPassFilterState>,
    cv: Condvar,
}

impl LowPassFilter {
    pub fn new(capacity: usize) -> Self {
        Self {
            state: Mutex::new(LowPassFilterState {
                accessors: 0,
                capacity,
            }),
            cv: Condvar::new(),
        }
    }

    /// To make things predictable, we synchronously increment the accessors count. This ensures
    /// that there is no ambiguity about whether an accessor has already incremented the count or
    /// not depending on whether it was polled.
    #[allow(clippy::await_holding_lock)] // wait() actually releases the lock.
    #[allow(clippy::manual_async_fn)] // so you don't need to poll before access() takes effect
    pub fn access(&self) -> impl Future<Output = LowPassFilterGuard<'_>> {
        let go = {
            let mut state = self.state.lock();
            state.accessors += 1;
            state.dispatch_more()
        };

        // This needs to be created *here* and not at the return point so that if this future gets
        // dropped while we await (or possible en before we await), the guard will re-acquire the
        // mutex and update the accessors count.
        let guard = LowPassFilterGuard { filter: self };

        if go {
            return futures::future::ready(guard).left_future();
        }

        async move {
            let mut state = self.state.lock();
            loop {
                if state.dispatch_more() {
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
}

impl Drop for LowPassFilterGuard<'_> {
    fn drop(&mut self) {
        let mut state = self.filter.state.lock();
        state.accessors -= 1;
        if state.dispatch_more() {
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
        filter.access().await;
    }

    #[tokio::test]
    async fn test_access_does_not_require_poll() {
        let filter = LowPassFilter::new(1);
        let t1 = filter.access().await;

        let t2 = filter.access();
        futures::pin_mut!(t2);
        assert!(futures::poll!(t2.as_mut()).is_pending());

        drop(t1);
        assert!(futures::poll!(t2.as_mut()).is_ready());
    }

    #[tokio::test]
    async fn test_access_many() {
        let filter = LowPassFilter::new(2);
        let _t1 = filter.access().await;
        let _t2 = filter.access().await;
    }

    #[tokio::test]
    async fn test_release() {
        let filter = LowPassFilter::new(1);
        let t0 = filter.access().await;
        let t1 = filter.access();
        futures::pin_mut!(t1);

        assert!(futures::poll!(t1.as_mut()).is_pending());

        drop(t0);
        assert!(futures::poll!(t1.as_mut()).is_ready());
    }

    #[tokio::test]
    async fn test_release_cancel() {
        let filter = LowPassFilter::new(1);
        let t0 = filter.access().await;

        // NOTE: We don't use pin_mut like above since we want to exercise cancellation here so we
        // need to be able to drop those futures.
        let mut t1 = filter.access().boxed();
        let mut t2 = filter.access().boxed();

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
