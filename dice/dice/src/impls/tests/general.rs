/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::sync::Barrier;
use std::sync::Mutex;

use allocative::Allocative;
use assert_matches::assert_matches;
use async_trait::async_trait;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use more_futures::cancellation::CancellationContext;

use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::error::DiceErrorImpl;
use crate::api::injected::InjectedKey;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::impls::dice::DiceModern;
use crate::versions::VersionNumber;
use crate::DiceData;
use crate::UserCycleDetector;
use crate::UserCycleDetectorGuard;

#[derive(Clone, Dupe, Debug, Display, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "{:?}", self)]
struct Foo(i32);

#[async_trait]
impl InjectedKey for Foo {
    type Value = i32;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[tokio::test]
async fn set_injected_multiple_times_per_commit() -> anyhow::Result<()> {
    let dice = DiceModern::builder().build(DetectCycles::Disabled);

    {
        let mut ctx = dice.updater();
        ctx.changed_to(vec![(Foo(0), 0)])?;
        ctx.changed_to(vec![(Foo(1), 1)])?;

        let ctx = ctx.commit().await;
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
    let dice = DiceModern::builder().build(DetectCycles::Disabled);

    {
        let mut ctx = dice.updater();
        ctx.changed_to(vec![(Foo(0), 0)])?;

        let ctx = ctx.commit().await;

        assert_eq!(ctx.get_version(), VersionNumber::new(1));
    }

    {
        let mut ctx = dice.updater();
        ctx.changed_to(vec![(Foo(0), 0)])?;

        let ctx = ctx.commit().await;
        assert_eq!(ctx.get_version(), VersionNumber::new(1));
    }

    Ok(())
}

#[derive(Clone, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative)]
#[display(fmt = "{:?}", self)]
struct K(i32);

#[async_trait]
impl Key for K {
    type Value = Result<K, Arc<anyhow::Error>>;

    async fn compute(
        &self,
        ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let mut sum = self.0;
        for i in 0..self.0 {
            sum += ctx
                .compute(&K(i))
                .await
                .map_err(|e| Arc::new(anyhow::anyhow!(e)))??
                .0;
        }
        Ok(K(sum))
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

#[test]
fn dice_computations_are_parallel() {
    let n_thread = 10;

    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(n_thread)
        .max_blocking_threads(n_thread)
        .build()
        .unwrap();
    let barrier = Arc::new(Barrier::new(n_thread));

    #[derive(Clone, Debug, Display, Derivative, Allocative)]
    #[derivative(Hash, PartialEq, Eq)]
    #[display(fmt = "{:?}", self)]
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
            _ctx: &DiceComputations,
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
        let dice = DiceModern::builder().build(DetectCycles::Disabled);
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
    #[display(fmt = "{:?}", self)]
    struct DataRequest(u8);
    #[async_trait]
    impl Key for DataRequest {
        type Value = usize;

        async fn compute(
            &self,
            ctx: &DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            ctx.per_transaction_data().data.get::<U>().unwrap().0
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    let dice = DiceModern::builder().build(DetectCycles::Disabled);
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

    let ctx0 = dice.updater_with_data(per_cmd_data0).commit().await;

    let ctx1 = dice.updater_with_data(per_cmd_data1).commit().await;

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
            _ctx: &DiceComputations,
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

    let dice = DiceModern::new(DiceData::new());
    let mut updater = dice.updater();

    assert!(updater.changed_to([(Invalid, ())]).is_err());
}

#[derive(Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative)]
#[display(fmt = "{:?}", self)]
struct Fib(u8);

#[async_trait]
impl Key for Fib {
    type Value = Result<u64, Arc<anyhow::Error>>;

    async fn compute(
        &self,
        ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        if self.0 > 93 {
            return Err(Arc::new(anyhow::anyhow!("that's too big")));
        }
        if self.0 < 2 {
            return Ok(self.0 as u64);
        }
        let (a, b) =
            futures::future::join(ctx.compute(&Fib(self.0 - 2)), ctx.compute(&Fib(self.0 - 1)))
                .await;
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
    fn start_computing_key(
        &self,
        key: &dyn std::any::Any,
    ) -> Option<Box<dyn UserCycleDetectorGuard>> {
        let f = key.downcast_ref::<Fib>().unwrap();
        self.events
            .lock()
            .unwrap()
            .push(CycleDetectorEvents::Start(*f));
        Some(Box::new(CycleDetectorGuard {
            key: *f,
            events: self.events.clone(),
        }))
    }

    fn finished_computing_key(&self, key: &dyn std::any::Any) {
        let f = key.downcast_ref::<Fib>().unwrap();
        self.events
            .lock()
            .unwrap()
            .push(CycleDetectorEvents::Finish(*f));
    }
}

impl UserCycleDetectorGuard for CycleDetectorGuard {
    fn add_edge(&self, key: &dyn std::any::Any) {
        let f = key.downcast_ref::<Fib>().unwrap();
        self.events
            .lock()
            .unwrap()
            .push(CycleDetectorEvents::Edge(self.key, *f))
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

#[test]
fn user_cycle_detector_receives_events() -> anyhow::Result<()> {
    let dice = DiceModern::builder().build(DetectCycles::Disabled);

    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(async {
        let events = Arc::new(Mutex::new(Vec::new()));
        let user_data = UserComputationData {
            cycle_detector: Some(Arc::new(CycleDetector {
                events: events.clone(),
            })),
            ..Default::default()
        };
        let ctx = dice.updater_with_data(user_data).commit().await;
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
