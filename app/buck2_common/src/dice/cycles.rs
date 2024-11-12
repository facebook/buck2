/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_futures::cancellation::CancellationContext;
use buck2_util::cycle_detector::CycleDescriptor;
use buck2_util::cycle_detector::LazyCycleDetector;
use buck2_util::cycle_detector::LazyCycleDetectorGuard;
use derive_more::Display;
use dice::DiceComputations;
use dice::DynKey;
use dice::Key;
use dice::UserCycleDetector;
use dice::UserCycleDetectorGuard;
use futures::Future;
use tracing::debug;

/// Additional requirement for a CycleDescriptor to be used for defining a Dice UserCycleDetector through
/// the CycleDetectorAdapter. Simply requires converting the Dice Key to the CycleDescriptor::Key type.
pub trait CycleAdapterDescriptor: CycleDescriptor {
    /// Will be provided a &DynKey for a Dice Key implementation.
    fn to_key(key: &DynKey) -> Option<Self::Key>;
}

/// This allows using the LazyCycleDetector as a Dice UserCycleDetector. All it needs is an implementation of the normal
/// CycleDescriptor for the LazyCycleDetector and a CycleAdapterDescriptor to convert dice keys to the CycleDescriptor::Key.
#[derive(Debug)]
pub struct CycleDetectorAdapter<D: CycleAdapterDescriptor> {
    inner: LazyCycleDetector<D>,
}

pub struct CycleGuardResult<R, E>(buck2_error::Result<Result<R, E>>);

impl<R, E> CycleGuardResult<R, E> {
    /// Converts the result from GuardThis into an buck2_error::Result.
    ///
    /// This is a separate function to get the borrowing of the &mut DiceComputations, (which in
    /// guard_this will have been borrowed by the passed in future).
    pub async fn into_result(
        self,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Result<R, E>> {
        match &self.0 {
            Ok(Ok(_)) => {}
            _ => {
                // The cycle detector either hit an error or it detected a cycle. In either case, we
                // want to make sure that dice doesn't cache this node. To do that, we add a dep on our
                // PoisonedDueToDetectedCycle key. We shouldn't hit a dice error, but we know we're already
                // returning an error so just ignore it.
                let _unused = ctx.compute(&PoisonedDueToDetectedCycleKey).await;
            }
        }
        self.0
    }
}

pub struct CycleGuard<D: CycleAdapterDescriptor>(Option<Arc<CycleAdapterGuard<D>>>);

impl<D: CycleAdapterDescriptor> CycleGuard<D> {
    pub fn new(ctx: &DiceComputations<'_>) -> buck2_error::Result<Self> {
        Ok(Self(ctx.cycle_guard()?))
    }

    /// Use this to wrap a computation waiting on dependencies where you want to detect cycles. All dice keys involved
    /// in the cycle must be supported by the CycleAdapterDescriptor implementation. If the cycle is FooKey(1) ->
    /// BarKey(a) -> FooKey(1) and the descriptor only support FooKey, the cycle won't be detected.
    ///
    /// The guard_this generally just needs to appear on at least one of the edges in the cycle. For example,
    /// FooKey/BarKey example above, a descriptor that supports both keys but only guards on the BarKey->FooKey
    /// edge would probably be okay (that edge would detect the cycle and get an error, which would
    /// then propagate up to the waiting fookey).
    ///
    /// It's probably the case that the keys in the cycle should treat the cycle error case as invalid (in the sense
    /// of Dice Key::validity()).
    pub async fn guard_this<R: Send, Fut: Future<Output = R> + Send>(
        &self,
        fut: Fut,
    ) -> CycleGuardResult<R, D::Error> {
        #[allow(clippy::redundant_closure_call)]
        let res: buck2_error::Result<Result<R, D::Error>> = (|| async move {
            match &self.0 {
                Some(v) => v.guard.guard_this(fut).await,
                None => Ok(Ok(fut.await)),
            }
        })()
        .await;
        CycleGuardResult(res)
    }
}

/// This is a simple type we can use that will mark any dice node that depends on it as invalid.
/// This is used to ensure we don't cache an error from the cycle detector (the cycle detector allows
/// flow of data that is potentially not tracked by dice, and while we may be able to identify those
/// and fix them it'll still be fragile and its best to just make sure they aren't cached).
#[derive(Allocative, Debug, Display, Clone, PartialEq, Eq, Hash)]
#[display("poisoned_due_to_detected_cycle")]
struct PoisonedDueToDetectedCycleKey;

#[async_trait]
impl Key for PoisonedDueToDetectedCycleKey {
    type Value = ();

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        true
    }

    fn validity(_x: &Self::Value) -> bool {
        false
    }
}

impl<D: CycleAdapterDescriptor> CycleDetectorAdapter<D> {
    pub fn new() -> Self {
        Self {
            inner: LazyCycleDetector::new(),
        }
    }
}

impl<D: CycleAdapterDescriptor> CycleDescriptor for CycleDetectorAdapter<D> {
    type Key = D::Key;
    type Error = D::Error;

    fn cycle_error(cycle: Vec<&Self::Key>) -> Self::Error {
        D::cycle_error(cycle)
    }
}

impl<D: CycleAdapterDescriptor> UserCycleDetector for CycleDetectorAdapter<D> {
    fn start_computing_key(&self, key: &DynKey) -> Option<Arc<dyn UserCycleDetectorGuard>> {
        match D::to_key(key) {
            None => None,
            Some(v) => Some(Arc::new(CycleAdapterGuard {
                guard: self.inner.start(v),
            })),
        }
    }

    fn finished_computing_key(&self, key: &DynKey) {
        if let Some(v) = D::to_key(key) {
            debug!("finish computing key {}", v);
            self.inner.finish(v);
        }
    }
}

pub struct CycleAdapterGuard<D: CycleAdapterDescriptor> {
    guard: LazyCycleDetectorGuard<D>,
}

impl<D: CycleAdapterDescriptor> UserCycleDetectorGuard for CycleAdapterGuard<D> {
    fn add_edge(&self, key: &DynKey) {
        if let Some(k) = D::to_key(key) {
            self.guard.add_edge(k);
        }
    }

    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

/// Allows having multiple cycle detectors (e.g. for different keys/phases) as the one Dice UserCycleDetector.
pub struct PairDiceCycleDetector<A: UserCycleDetector, B: UserCycleDetector>(pub A, pub B);

impl<A: UserCycleDetector, B: UserCycleDetector> UserCycleDetector for PairDiceCycleDetector<A, B> {
    fn start_computing_key(&self, key: &DynKey) -> Option<Arc<dyn UserCycleDetectorGuard>> {
        // Right now, only one of the inner detectors is allowed to claim a key. We could feasibly change that, but it's a bit trickier.
        if let Some(v) = self.0.start_computing_key(key) {
            return Some(v);
        }
        if let Some(v) = self.1.start_computing_key(key) {
            return Some(v);
        }
        None
    }

    fn finished_computing_key(&self, key: &DynKey) {
        self.0.finished_computing_key(key);
        self.1.finished_computing_key(key);
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;

    use allocative::Allocative;
    use async_trait::async_trait;
    use dice::CancellationContext;
    use dice::DiceComputations;
    use dice::DynKey;
    use dice::Key;
    use dice::UserCycleDetector;
    use dice::UserCycleDetectorGuard;

    use crate::dice::cycles::PairDiceCycleDetector;

    #[test]
    fn pair_cycle_detector() {
        #[derive(Allocative, Debug, derive_more::Display, Clone, PartialEq, Eq, Hash)]
        struct K;
        #[async_trait]
        impl Key for K {
            type Value = ();

            async fn compute(
                &self,
                _ctx: &mut DiceComputations,
                _cancellations: &CancellationContext,
            ) -> Self::Value {
                unreachable!()
            }

            fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
                false
            }
        }

        struct TestingGuard;

        impl UserCycleDetectorGuard for TestingGuard {
            fn add_edge(&self, _key: &DynKey) {
                unreachable!("testing")
            }

            fn type_name(&self) -> &'static str {
                unreachable!("testing")
            }
        }

        #[derive(Debug, Default)]
        struct ReceivesStartAndFinish {
            got_start: AtomicBool,
            got_finish: AtomicBool,
        }

        impl UserCycleDetector for ReceivesStartAndFinish {
            fn start_computing_key(
                &self,
                _key: &DynKey,
            ) -> Option<Arc<dyn UserCycleDetectorGuard>> {
                self.got_start.store(true, Ordering::SeqCst);
                Some(Arc::new(TestingGuard))
            }

            fn finished_computing_key(&self, _key: &DynKey) {
                assert!(self.got_start.load(Ordering::SeqCst));
                self.got_finish.store(true, Ordering::SeqCst);
            }
        }

        #[derive(Debug, Default)]
        struct ReceivesOnlyFinish {
            got_finish: AtomicBool,
        }

        impl UserCycleDetector for ReceivesOnlyFinish {
            fn start_computing_key(
                &self,
                _key: &DynKey,
            ) -> Option<Arc<dyn UserCycleDetectorGuard>> {
                panic!("shouldn't be called")
            }

            fn finished_computing_key(&self, _key: &DynKey) {
                self.got_finish.store(true, Ordering::SeqCst);
            }
        }

        let detector = PairDiceCycleDetector(
            ReceivesStartAndFinish::default(),
            ReceivesOnlyFinish::default(),
        );

        assert!(detector.start_computing_key(&DynKey::from_key(K)).is_some());

        detector.finished_computing_key(&DynKey::from_key(K));

        assert!(detector.0.got_start.load(Ordering::SeqCst));
        assert!(detector.0.got_finish.load(Ordering::SeqCst));
        assert!(detector.1.got_finish.load(Ordering::SeqCst));
    }
}
