/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;
#[cfg(fbcode_build)]
use std::sync::Barrier;
use std::sync::Mutex;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use assert_matches::assert_matches;
use async_trait::async_trait;
use derivative::Derivative;
use derive_more::Display;
use dice_error::DiceErrorImpl;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::FutureExt;
use tokio::sync::oneshot;

use crate::Dice;
use crate::DiceData;
use crate::DynKey;
use crate::UserCycleDetector;
use crate::UserCycleDetectorGuard;
use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::injected::InjectedKey;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::versions::VersionNumber;

#[derive(Clone, Dupe, Debug, Display, Eq, Hash, PartialEq, Allocative)]
#[display("{:?}", self)]
struct Foo(i32);

#[async_trait]
impl InjectedKey for Foo {
    type Value = i32;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[derive(Clone, Dupe, Debug, Derivative, Allocative, Display)]
#[derivative(PartialEq, Eq, Hash)]
#[display("{:?}", self)]
#[allocative(skip)]
struct KeyThatRuns {
    #[derivative(Hash = "ignore", PartialEq = "ignore")]
    barrier1: Arc<tokio::sync::Semaphore>,
    #[derivative(Hash = "ignore", PartialEq = "ignore")]
    barrier2: Arc<tokio::sync::Semaphore>,
    #[derivative(Hash = "ignore", PartialEq = "ignore")]
    is_ran: Arc<AtomicBool>,
}

#[async_trait]
impl Key for KeyThatRuns {
    type Value = ();

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        self.barrier1.add_permits(1);
        let _s = self.barrier2.acquire().await.unwrap();
        self.is_ran.store(true, Ordering::SeqCst);
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        true
    }
}

#[tokio::test]
async fn set_injected_multiple_times_per_commit() -> anyhow::Result<()> {
    let dice = Dice::builder().build(DetectCycles::Disabled);

    {
        let mut ctx = dice.updater();
        ctx.changed_to(vec![(Foo(0), 0)])?;
        ctx.changed_to(vec![(Foo(1), 1)])?;

        let mut ctx = ctx.commit().await;
        assert_eq!(ctx.compute(&Foo(0)).await?, 0);
        assert_eq!(ctx.compute(&Foo(1)).await?, 1);
    }

    {
        let mut ctx = dice.updater();
        ctx.changed_to(vec![(Foo(0), 0)])?;

        assert_matches!(
            ctx.changed_to(vec![(Foo(0), 1)]),
            Err(err) => assert_matches!(&*err.0, DiceErrorImpl::DuplicateChange(_))
        );
    }

    Ok(())
}

#[tokio::test]
async fn set_injected_with_no_change_no_new_ctx() -> anyhow::Result<()> {
    let dice = Dice::builder().build(DetectCycles::Disabled);

    {
        let mut ctx = dice.updater();
        ctx.changed_to(vec![(Foo(0), 0)])?;

        let ctx = ctx.commit().await.0;

        assert_eq!(ctx.get_version(), VersionNumber::new(1));
    }

    {
        let mut ctx = dice.updater();
        ctx.changed_to(vec![(Foo(0), 0)])?;

        let ctx = ctx.commit().await.0;
        assert_eq!(ctx.get_version(), VersionNumber::new(1));
    }

    Ok(())
}

#[cfg(fbcode_build)]
#[test]
fn dice_computations_are_parallel() {
    let n_thread = 10;

    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(n_thread + 1)
        .build()
        .unwrap();
    let barrier = Arc::new(Barrier::new(n_thread));

    #[derive(Clone, Debug, Display, Derivative, Allocative)]
    #[derivative(Hash, PartialEq, Eq)]
    #[display("{:?}", self)]
    struct Blocking {
        index: usize,
        #[derivative(PartialEq = "ignore", Hash = "ignore")]
        #[allocative(skip)]
        barrier: Arc<Barrier>,
    }

    #[async_trait]
    impl Key for Blocking {
        type Value = usize;

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            self.barrier.wait();
            1
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    rt.block_on(async move {
        let dice = Dice::builder().build(DetectCycles::Disabled);
        let mut sum = 0;

        let dice = &dice;
        let barrier = &barrier;

        let futs = (0..n_thread)
            .map(|i| async move {
                dice.updater()
                    .commit()
                    .await
                    .compute(&Blocking {
                        index: i,
                        barrier: barrier.dupe(),
                    })
                    .await
                    .unwrap()
            })
            .collect::<Vec<_>>();

        futures::future::join_all(futs)
            .await
            .iter()
            .for_each(|res| sum += res);

        assert_eq!(sum, n_thread);
    })
}

#[tokio::test]
async fn different_data_per_compute_ctx() {
    struct U(usize);

    #[derive(Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Allocative)]
    #[display("{:?}", self)]
    struct DataRequest(u8);
    #[async_trait]
    impl Key for DataRequest {
        type Value = usize;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            ctx.per_transaction_data().data.get::<U>().unwrap().0
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    let dice = Dice::builder().build(DetectCycles::Disabled);
    let per_cmd_data0 = {
        let mut d = UserComputationData::new();
        d.data.set(U(0));
        d
    };
    let per_cmd_data1 = {
        let mut d = UserComputationData::new();
        d.data.set(U(1));
        d
    };

    let mut ctx0 = dice.updater_with_data(per_cmd_data0).commit().await;

    let mut ctx1 = dice.updater_with_data(per_cmd_data1).commit().await;

    let request0 = ctx0.compute(&DataRequest(0));
    let request1 = ctx1.compute(&DataRequest(1));

    assert_eq!(request0.await.unwrap(), 0);
    assert_eq!(request1.await.unwrap(), 1);
}

#[test]
fn invalid_update() {
    #[derive(Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Allocative)]
    struct Invalid;

    #[async_trait]
    impl Key for Invalid {
        type Value = ();

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            unimplemented!("not needed for test")
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            unimplemented!("not needed for test")
        }

        fn validity(_x: &Self::Value) -> bool {
            false
        }
    }

    let dice = Dice::new(DiceData::new());
    let mut updater = dice.updater();

    assert!(updater.changed_to([(Invalid, ())]).is_err());
}

#[derive(Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative)]
#[display("{:?}", self)]
struct Fib(u8);

#[async_trait]
impl Key for Fib {
    type Value = Result<u64, Arc<anyhow::Error>>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        if self.0 > 93 {
            return Err(Arc::new(anyhow::anyhow!("that's too big")));
        }
        if self.0 < 2 {
            return Ok(self.0 as u64);
        }
        let (a, b) = {
            ctx.compute2(
                |ctx| ctx.compute(&Fib(self.0 - 2)).boxed(),
                |ctx| ctx.compute(&Fib(self.0 - 1)).boxed(),
            )
            .await
        };
        match (a, b) {
            (Ok(a), Ok(b)) => Ok(a? + b?),
            _ => Err(Arc::new(anyhow::anyhow!("some dice error"))),
        }
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum CycleDetectorEvents {
    Start(Fib),
    Finish(Fib),
    Edge(Fib, Fib),
}

#[derive(Debug, Clone)]
struct CycleDetector {
    events: Arc<Mutex<Vec<CycleDetectorEvents>>>,
}

struct CycleDetectorGuard {
    key: Fib,
    events: Arc<Mutex<Vec<CycleDetectorEvents>>>,
}

impl UserCycleDetector for CycleDetector {
    fn start_computing_key(&self, key: &DynKey) -> Option<Arc<dyn UserCycleDetectorGuard>> {
        let f = key.downcast_ref::<Fib>().unwrap();
        self.events
            .lock()
            .unwrap()
            .push(CycleDetectorEvents::Start(*f));
        Some(Arc::new(CycleDetectorGuard {
            key: *f,
            events: self.events.dupe(),
        }))
    }

    fn finished_computing_key(&self, key: &DynKey) {
        let f = key.downcast_ref::<Fib>().unwrap();
        self.events
            .lock()
            .unwrap()
            .push(CycleDetectorEvents::Finish(*f));
    }
}

impl UserCycleDetectorGuard for CycleDetectorGuard {
    fn add_edge(&self, key: &DynKey) {
        let f = key.downcast_ref::<Fib>().unwrap();
        self.events
            .lock()
            .unwrap()
            .push(CycleDetectorEvents::Edge(self.key, *f))
    }

    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

#[test]
fn user_cycle_detector_receives_events() -> anyhow::Result<()> {
    let dice = Dice::builder().build(DetectCycles::Disabled);

    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(async {
        let events = Arc::new(Mutex::new(Vec::new()));
        let user_data = UserComputationData {
            cycle_detector: Some(Arc::new(CycleDetector {
                events: events.dupe(),
            })),
            ..Default::default()
        };
        let mut ctx = dice.updater_with_data(user_data).commit().await;
        let res = ctx.compute(&Fib(20)).await?.expect("should succeed");
        assert_eq!(res, 6765);

        let check_events = move |i, expected_edges| {
            let mut started = false;
            let mut finished = false;
            let mut edges = Vec::new();
            for ev in events.lock().unwrap().iter() {
                match ev {
                    CycleDetectorEvents::Start(Fib(v)) if i == *v => {
                        started = true;
                        assert!(!finished);
                    }
                    CycleDetectorEvents::Finish(Fib(v)) if i == *v => {
                        assert!(!finished);
                        finished = true;
                        assert!(started);
                    }
                    CycleDetectorEvents::Edge(Fib(v), Fib(j)) if i == *v => {
                        assert!(started);
                        assert!(!finished);
                        edges.push(*j)
                    }
                    _ => {
                        // ignore
                    }
                }
            }
            assert!(finished);
            edges.sort();
            assert_eq!(edges, expected_edges);
        };

        check_events(0, vec![]);
        check_events(1, vec![]);
        for i in 2..=20 {
            check_events(i, vec![i - 2, i - 1]);
        }

        Ok(())
    })
}

/// Test that dropping request cancel execution. Test scenario is:
/// The test has 1 barrier and 1 semaphore:
/// - barrier is used by the task to signal that the task is started, and that the task doesn't progress
///   to complete.
/// - semaphore ensures that the task doesn't run until the main thread drops the request
/// 1. we spawn a task
/// 2. assert task is spawned
/// 3. drop the request future
/// 4. ensure the task is cancelled since the task will panic if it proceeds beyond the semaphore
#[tokio::test]
async fn dropping_request_future_cancels_execution() {
    #[derive(Debug)]
    struct DropSignal(Option<oneshot::Sender<()>>);

    impl Drop for DropSignal {
        fn drop(&mut self) {
            self.0.take().unwrap().send(()).unwrap();
        }
    }

    #[derive(Clone, Dupe, Debug, Derivative, Allocative, Display)]
    #[derivative(PartialEq, Eq, Hash)]
    #[display("{:?}", self)]
    #[allocative(skip)]
    struct KeyThatShouldntRun {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        barrier1: Arc<tokio::sync::Barrier>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        barrier2: Arc<tokio::sync::Semaphore>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        drop_signal: Arc<Mutex<Option<DropSignal>>>,
    }

    #[async_trait]
    impl Key for KeyThatShouldntRun {
        type Value = ();

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            let _drop_signal = self.drop_signal.lock().unwrap().take();

            self.barrier1.wait().await;
            let _guard = self.barrier2.acquire().await.unwrap();

            panic!("shouldn't run")
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            true
        }
    }

    let barrier1 = Arc::new(tokio::sync::Barrier::new(2));
    let barrier2 = Arc::new(tokio::sync::Semaphore::new(0));

    let (tx, rx) = oneshot::channel();
    let drop_signal = DropSignal(Some(tx));

    let dice = Dice::builder().build(DetectCycles::Disabled);

    let mut ctx = dice.updater().commit().await;

    let key = KeyThatShouldntRun {
        barrier1: barrier1.dupe(),
        barrier2: barrier2.dupe(),
        drop_signal: Arc::new(Mutex::new(Some(drop_signal))),
    };
    let req = ctx.compute(&key);

    // ensure that the key starts computing
    barrier1.wait().await;

    drop(req);

    barrier2.add_permits(1);

    rx.await.unwrap();
}

/// Test that dropping request cancel execution. Test scenario is:
/// The test has 2 barriers:
/// - first is used by the task to signal that the task is started, and that the task doesn't progress
///   to complete.
/// - second ensures that the task doesn't run until the main thread drops the one of the request
/// 1. we spawn a task, but have two requests attached to it
/// 2. assert task is spawned
/// 3. drop one of the request future
/// 4. ensure the task is still finishes and runs by checking the `is-ran` atomic
#[tokio::test]
async fn dropping_request_future_doesnt_cancel_if_multiple_requests_active() {
    let barrier1 = Arc::new(tokio::sync::Semaphore::new(0));
    let barrier2 = Arc::new(tokio::sync::Semaphore::new(0));
    let is_ran = Arc::new(AtomicBool::new(false));

    let key = &KeyThatRuns {
        barrier1: barrier1.dupe(),
        barrier2: barrier2.dupe(),
        is_ran: is_ran.dupe(),
    };

    let dice = Dice::builder().build(DetectCycles::Disabled);

    let mut ctx = dice.updater().commit().await.0.0;
    let (req1, req2) = ctx.compute2(
        |ctx| ctx.compute(key).boxed(),
        |ctx| ctx.compute(key).boxed(),
    );

    // ensure that the key starts computing
    let _b = barrier1.acquire().await;

    drop(req1);

    barrier2.add_permits(1);

    // req2 still succeed
    req2.await.unwrap();

    assert!(is_ran.load(Ordering::SeqCst));
}

#[tokio::test]
async fn user_cycle_detector_is_present_modern() -> anyhow::Result<()> {
    user_cycle_detector_is_present(Dice::builder().build(DetectCycles::Disabled)).await
}

async fn user_cycle_detector_is_present(dice: Arc<Dice>) -> anyhow::Result<()> {
    #[derive(Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative)]
    #[display("{:?}", self)]
    struct AccessCycleGuardKey;

    #[async_trait]
    impl Key for AccessCycleGuardKey {
        type Value = ();

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            assert!(
                ctx.cycle_guard::<AccessCycleDetectorGuard>()
                    .unwrap()
                    .is_some()
            );
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            true
        }
    }

    #[derive(Debug, Clone)]
    struct AccessCycleDetector;

    struct AccessCycleDetectorGuard;

    impl UserCycleDetector for AccessCycleDetector {
        fn start_computing_key(&self, _key: &DynKey) -> Option<Arc<dyn UserCycleDetectorGuard>> {
            Some(Arc::new(AccessCycleDetectorGuard))
        }

        fn finished_computing_key(&self, _key: &DynKey) {}
    }

    impl UserCycleDetectorGuard for AccessCycleDetectorGuard {
        fn add_edge(&self, _key: &DynKey) {}

        fn type_name(&self) -> &'static str {
            std::any::type_name::<Self>()
        }
    }

    let user_data = UserComputationData {
        cycle_detector: Some(Arc::new(AccessCycleDetector)),
        ..Default::default()
    };
    let mut ctx = dice.updater_with_data(user_data).commit().await;
    Ok(ctx.compute(&AccessCycleGuardKey).await?)
}

#[tokio::test]
async fn test_dice_usable_after_cancellations() {
    let dice = Dice::builder().build(DetectCycles::Disabled);

    let mut ctx = dice.updater().commit().await;

    let barrier1 = Arc::new(tokio::sync::Semaphore::new(0));
    let barrier2 = Arc::new(tokio::sync::Semaphore::new(0));
    let is_ran = Arc::new(AtomicBool::new(false));

    let key = KeyThatRuns {
        barrier1: barrier1.dupe(),
        barrier2: barrier2.dupe(),
        is_ran: is_ran.dupe(),
    };

    let req1 = ctx.compute(&key);

    // ensure that the key starts computing
    let _b = barrier1.acquire().await;

    // cancel and await for cancellation
    drop(req1);
    drop(ctx);

    dice.wait_for_idle().await;

    assert!(!is_ran.load(Ordering::Acquire));

    let mut ctx = dice.updater().commit().await;

    // req2 still succeed. Note that due to dice caching, even if we make a new key, the same
    // instance would be used, so just use the same one.
    let req2 = ctx.compute(&key);

    barrier2.add_permits(1);

    req2.await.unwrap();

    assert!(is_ran.load(Ordering::SeqCst));
}

#[tokio::test]
async fn test_is_idle_respects_active_transactions() {
    let dice = Dice::builder().build(DetectCycles::Disabled);

    let mut ctx = dice.updater().commit().await;

    let barrier1 = Arc::new(tokio::sync::Semaphore::new(0));
    let barrier2 = Arc::new(tokio::sync::Semaphore::new(0));
    let is_ran = Arc::new(AtomicBool::new(false));

    let key = KeyThatRuns {
        barrier1: barrier1.dupe(),
        barrier2: barrier2.dupe(),
        is_ran: is_ran.dupe(),
    };

    let req1 = ctx.compute(&key);

    // FIXME(JakobDegen): This is a pretty silly behavior for a function called `is_idle`, dice is
    // obviously not idle.
    assert!(dice.is_idle().await);
    dice.wait_for_idle().await;

    barrier2.add_permits(1);
    req1.await.unwrap();

    assert!(dice.is_idle().await);
    dice.wait_for_idle().await;
}
