/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::fmt::Debug;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_util::cycle_detector::CycleDescriptor;
use buck2_util::cycle_detector::LazyCycleDetector;
use buck2_util::cycle_detector::LazyCycleDetectorGuard;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dice::UserCycleDetector;
use dice::UserCycleDetectorGuard;
use futures::Future;
use more_futures::cancellation::CancellationContext;

/// Additional requirement for a CycleDescriptor to be used for defining a Dice UserCycleDetector through
/// the CycleDetectorAdapter. Simply requires converting the Dice Key to the CycleDescriptor::Key type.
pub trait CycleAdapterDescriptor: CycleDescriptor {
    /// Will be provided a &dyn Any for a Dice Key implementation.
    fn to_key(key: &dyn Any) -> Option<Self::Key>;
}

/// This allows using the LazyCycleDetector as a Dice UserCycleDetector. All it needs is an implementation of the normal
/// CycleDescriptor for the LazyCycleDetector and a CycleAdapterDescriptor to convert dice keys to the CycleDescriptor::Key.
#[derive(Debug)]
pub struct CycleDetectorAdapter<D: CycleAdapterDescriptor> {
    inner: LazyCycleDetector<D>,
}

#[async_trait]
pub trait CycleGuard<E> {
    async fn guard_this<R: Send, Fut: Future<Output = R> + Send>(
        ctx: &DiceComputations,
        fut: Fut,
    ) -> anyhow::Result<Result<R, E>>;
}

#[async_trait]
impl<D: CycleAdapterDescriptor> CycleGuard<D::Error> for D {
    /// Use this to wrap futures waiting on dependencies where you want to detect cycles. All dice keys involved
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
    async fn guard_this<R: Send, Fut: Future<Output = R> + Send>(
        ctx: &DiceComputations,
        fut: Fut,
    ) -> anyhow::Result<Result<R, D::Error>> {
        match ctx.cycle_guard::<CycleAdapterGuard<D>>()? {
            Some(v) => match v.guard.guard_this(fut).await {
                Ok(Ok(v)) => Ok(Ok(v)),
                v => {
                    // The cycle detector either hit an error or it detected a cycle. In either case, we
                    // want to make sure that dice doesn't cache this node. To do that, we add a dep on our
                    // PoisonedDueToDetectedCycle key. We shouldn't hit a dice error, but we know we're already
                    // returning an error so just ignore it.
                    let _unused = ctx.compute(&PoisonedDueToDetectedCycleKey).await;
                    v
                }
            },
            None => Ok(Ok(fut.await)),
        }
    }
}

/// This is a simple type we can use that will mark any dice node that depends on it as invalid.
/// This is used to ensure we don't cache an error from the cycle detector (the cycle detector allows
/// flow of data that is potentially not tracked by dice, and while we may be able to identify those
/// and fix them it'll still be fragile and its best to just make sure they aren't cached).
#[derive(Allocative, Debug, Display, Clone, PartialEq, Eq, Hash)]
#[display(fmt = "poisoned_due_to_detected_cycle")]
struct PoisonedDueToDetectedCycleKey;

#[async_trait]
impl Key for PoisonedDueToDetectedCycleKey {
    type Value = ();

    async fn compute(
        &self,
        _ctx: &DiceComputations,
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
    fn start_computing_key(&self, key: &dyn Any) -> Option<Box<dyn UserCycleDetectorGuard>> {
        match D::to_key(key) {
            None => None,
            Some(v) => Some(Box::new(CycleAdapterGuard {
                guard: self.inner.start(v),
            })),
        }
    }

    fn finished_computing_key(&self, key: &dyn Any) {
        if let Some(v) = D::to_key(key) {
            self.inner.finish(v);
        }
    }
}

pub struct CycleAdapterGuard<D: CycleAdapterDescriptor> {
    guard: LazyCycleDetectorGuard<D>,
}

impl<D: CycleAdapterDescriptor> UserCycleDetectorGuard for CycleAdapterGuard<D> {
    fn add_edge(&self, key: &dyn Any) {
        if let Some(k) = D::to_key(key) {
            self.guard.add_edge(k);
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

/// Allows having multiple cycle detectors (e.g. for different keys/phases) as the one Dice UserCycleDetector.
pub struct PairDiceCycleDetector<A: UserCycleDetector, B: UserCycleDetector>(pub A, pub B);

impl<A: UserCycleDetector, B: UserCycleDetector> UserCycleDetector for PairDiceCycleDetector<A, B> {
    fn start_computing_key(&self, key: &dyn Any) -> Option<Box<dyn UserCycleDetectorGuard>> {
        // Right now, only one of the inner detectors is allowed to claim a key. We could feasibly change that, but it's a bit trickier.
        if let Some(v) = self.0.start_computing_key(key) {
            return Some(v);
        }
        if let Some(v) = self.1.start_computing_key(key) {
            return Some(v);
        }
        None
    }

    fn finished_computing_key(&self, key: &dyn Any) {
        self.0.start_computing_key(key);
        self.1.start_computing_key(key);
    }
}
