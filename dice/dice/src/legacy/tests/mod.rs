/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod demo;
mod projection;

use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Barrier;
use std::sync::Mutex;
use std::time::Duration;

use assert_matches::assert_matches;
use async_trait::async_trait;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use futures::future::FutureExt;
use futures::future::Shared;
use more_futures::cancellation::CancellationContext;
use tokio::sync::oneshot;
use tokio::time::timeout;

use super::*;
use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::error::DiceErrorImpl;
use crate::api::injected::InjectedKey;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::legacy::ctx::testing::DiceCtxExt;
use crate::legacy::incremental::evaluator::testing::EvaluatorUnreachable;
use crate::legacy::incremental::testing::DependencyExt;
use crate::legacy::incremental::testing::IncrementalEngineExt;
use crate::legacy::incremental::testing::VersionedCacheResultAssertsExt;
use crate::legacy::incremental::versions::MinorVersion;
use crate::versions::VersionNumber;
use crate::HashSet;
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
    let dice = DiceLegacy::builder().build(DetectCycles::Enabled);

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
    let dice = DiceLegacy::builder().build(DetectCycles::Enabled);

    {
        let mut ctx = dice.updater();
        ctx.changed_to(vec![(Foo(0), 0)])?;

        let ctx = ctx.commit().await;

        assert_eq!(ctx.0.0.get_version(), VersionNumber::new(1));
    }

    {
        let mut ctx = dice.updater();
        ctx.changed_to(vec![(Foo(0), 0)])?;

        let ctx = ctx.commit().await;
        assert_eq!(ctx.0.0.get_version(), VersionNumber::new(1));
    }

    Ok(())
}

#[tokio::test]
async fn updates_caches_only_on_ctx_finalize_in_order() -> anyhow::Result<()> {
    let dice = DiceLegacy::builder().build(DetectCycles::Enabled);

    {
        let mut ctx = dice.updater();

        // now we write something and commit
        ctx.changed_to(vec![(Foo(1), 1)])?;
        let vg = dice.global_versions.current();
        dice.find_cache::<Foo>()
            .get_maybe_cached(Foo(1), vg.version, *vg.minor_version_guard)
            .assert_none();

        ctx.commit().await;

        // committing the context commits the value
        let vg = dice.global_versions.current();
        assert_eq!(
            *dice
                .find_cache::<Foo>()
                .get_maybe_cached(Foo(1), vg.version, *vg.minor_version_guard)
                .assert_match()
                .val(),
            1
        );
    }

    {
        let mut ctx = dice.updater();
        let mut ctx1 = dice.updater();
        // even if we do a change on this ctx first.
        ctx.changed_to(vec![(Foo(2), 2)])?;
        ctx1.changed_to(vec![(Foo(3), 3)])?;

        // as long as we commit ctx1 first, it's values are committed first, in linear
        // history
        ctx1.commit().await;

        let vg = dice.global_versions.current();
        assert_eq!(
            *dice
                .find_cache::<Foo>()
                .get_maybe_cached(Foo(3), vg.version, *vg.minor_version_guard)
                .assert_match()
                .val(),
            3
        );

        dice.find_cache::<Foo>()
            .get_maybe_cached(Foo(2), vg.version, *vg.minor_version_guard)
            .assert_none();

        ctx.commit().await;

        // only now is 'ctx' committed
        let vg = dice.global_versions.current();
        assert_eq!(
            *dice
                .find_cache::<Foo>()
                .get_maybe_cached(Foo(2), vg.version, *vg.minor_version_guard)
                .assert_match()
                .val(),
            2
        );
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
fn ctx_tracks_deps_properly() -> anyhow::Result<()> {
    let dice = DiceLegacy::builder().build(DetectCycles::Enabled);

    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(async {
        let ctx = dice.updater().commit().await;
        let res = ctx
            .compute(&K(5))
            .await?
            .map_err(|e| anyhow::anyhow!(format!("{:#}", e)))?;
        assert_eq!(res, K(31));

        // introspect the caches for dependency info
        fn assert_cached_deps(dice: &Arc<DiceLegacy>, k: i32) {
            let vg = dice.global_versions.current();
            let cached = dice
                .find_cache::<K>()
                .get_cached(K(k), vg.version, *vg.minor_version_guard)
                .dupe();

            let expected_deps = (0..k)
                .map(K)
                .map(DependencyExt::<EvaluatorUnreachable<_, K>>::testing_raw)
                .collect::<Vec<_>>();

            // TODO(bobyf) better assert the versions stored in deps
            let meta = cached.read_meta();
            let deps = meta.deps.debug_deps().read();
            let (version, deps) = deps.as_ref().expect("No deps");

            assert_eq!(*version, VersionNumber::new(0));

            let deps = deps.iter().collect::<HashSet<_>>();
            let expected_deps = expected_deps.iter().collect::<HashSet<_>>();

            assert_eq!(deps, expected_deps);
        }

        assert_cached_deps(&dice, 5);
        assert_cached_deps(&dice, 4);
        assert_cached_deps(&dice, 3);
        assert_cached_deps(&dice, 2);
        assert_cached_deps(&dice, 1);
        assert_cached_deps(&dice, 0);

        Ok(())
    })
}

#[test]
fn ctx_tracks_rdeps_properly() -> anyhow::Result<()> {
    let dice = DiceLegacy::builder().build(DetectCycles::Enabled);

    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(1)
        .max_blocking_threads(1)
        .build()
        .unwrap();
    rt.block_on(async {
        let ctx = dice.updater().commit().await;
        let res = ctx
            .compute(&K(5))
            .await?
            .map_err(|e| anyhow::anyhow!(format!("{:#}", e)))?;
        assert_eq!(res, K(31));

        // introspect the caches for dependency info
        fn assert_cached_rdeps(dice: &Arc<DiceLegacy>, k: i32) {
            let vg = dice.global_versions.current();

            let cached = dice
                .find_cache::<K>()
                .get_cached(K(k), vg.version, *vg.minor_version_guard)
                .dupe();

            let mut expected_deps = ((k + 1)..6)
                .map(K)
                .map(|k| {
                    Arc::as_ptr(
                        &dice
                            .find_cache::<K>()
                            .get_cached(k, VersionNumber::new(0), *vg.minor_version_guard)
                            .into_dyn(),
                    )
                })
                .collect::<HashSet<_>>();

            for rdep in cached.read_meta().rdeps.rdeps().rdeps.iter() {
                assert!(
                    expected_deps.remove(&Arc::as_ptr(&rdep.0.0.upgrade().unwrap())),
                    "Extra rdeps"
                )
            }
            assert!(
                expected_deps.is_empty(),
                "Missing {} rdeps",
                expected_deps.len()
            )
        }

        assert_cached_rdeps(&dice, 0);
        assert_cached_rdeps(&dice, 4);
        assert_cached_rdeps(&dice, 3);
        assert_cached_rdeps(&dice, 2);
        assert_cached_rdeps(&dice, 1);
        assert_cached_rdeps(&dice, 0);

        Ok(())
    })
}

// ignore this for now. Need to change ctx to better represent lifetimes and ownership
// to support this
//
// #[test]
// fn compute_that_requests_changes() {
//     let dice = Dice::new();
//
//     #[derive(Clone, Debug, Eq, PartialEq, Hash)]
//     struct InvalidatingKey;
//
//     #[async_trait]
//     impl Key for InvalidatingKey {
//         type Value = i32;
//
//         async fn compute(&self, ctx: &Arc<DiceCtx>) -> Self::Value {
//             ctx.changed_to(vec![(InvalidatingKey, 1)]);
//             ctx.commit();
//             0
//         }
//     }
//
//     let mut rt = tokio::runtime::Runtime::new().unwrap();
//     rt.block_on(async {
//         assert_eq!(dice.ctx().compute(&InvalidatingKey).await, 0);
//         assert_eq!(dice.ctx().compute(&InvalidatingKey).await, 1);
//     });
// }

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
        let dice = DiceLegacy::builder().build(DetectCycles::Enabled);
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

    let dice = DiceLegacy::builder().build(DetectCycles::Enabled);
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

#[tokio::test]
async fn invalid_results_are_not_cached() -> anyhow::Result<()> {
    #[derive(Clone, Dupe, Debug, Display, Derivative, Allocative)]
    #[derivative(Hash, PartialEq, Eq)]
    #[display(fmt = "{:?}", self)]
    struct AlwaysTransient(#[derivative(PartialEq = "ignore", Hash = "ignore")] Arc<AtomicBool>);

    #[async_trait]
    impl Key for AlwaysTransient {
        type Value = usize;

        async fn compute(
            &self,
            _ctx: &DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            self.0.store(true, Ordering::SeqCst);
            1
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }

        fn validity(_x: &Self::Value) -> bool {
            false
        }
    }

    let dice = DiceLegacy::builder().build(DetectCycles::Enabled);
    let is_ran = Arc::new(AtomicBool::new(false));
    {
        let ctx = dice.updater().commit().await;
        ctx.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(is_ran.load(Ordering::SeqCst));

        // same ctx, so should reuse the result and
        is_ran.store(false, Ordering::SeqCst);
        ctx.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(!is_ran.load(Ordering::SeqCst));

        // simultaneously ctx should also re-use the result
        let ctx1 = dice.updater().commit().await;
        is_ran.store(false, Ordering::SeqCst);
        ctx1.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(!is_ran.load(Ordering::SeqCst));
    }

    {
        // new context should re-run
        let ctx = dice.updater().commit().await;
        is_ran.store(false, Ordering::SeqCst);
        ctx.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(is_ran.load(Ordering::SeqCst));

        // same ctx, so should reuse the result and
        is_ran.store(false, Ordering::SeqCst);
        ctx.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(!is_ran.load(Ordering::SeqCst));
    }

    Ok(())
}

#[tokio::test]
async fn demo_with_transient() -> anyhow::Result<()> {
    #[derive(Clone, Dupe, Debug, Display, Derivative, Allocative)]
    #[derivative(Hash, PartialEq, Eq)]
    #[display(fmt = "{:?}", self)]
    struct MaybeTransient(
        usize,
        #[derivative(PartialEq = "ignore", Hash = "ignore")] Arc<AtomicBool>,
    );

    #[async_trait]
    impl Key for MaybeTransient {
        type Value = Result<usize, bool>;

        async fn compute(
            &self,
            ctx: &DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            if self.0 == 0 {
                if !self.1.load(Ordering::SeqCst) {
                    Err(true)
                } else {
                    Ok(1)
                }
            } else {
                let mut sum = 0;
                for i in 0..self.0 {
                    if let Ok(v) = ctx
                        .compute(&MaybeTransient(i, self.1.dupe()))
                        .await
                        .unwrap()
                    {
                        sum += v;
                    } else {
                        return Err(false);
                    }
                }
                Ok(sum)
            }
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }

        fn validity(x: &Self::Value) -> bool {
            // intermediate nodes won't be directly invalid, but rely on the children to
            // propagate transient-ness
            if let Err(x) = x { !*x } else { true }
        }
    }

    let dice = DiceLegacy::builder().build(DetectCycles::Enabled);

    let ctx = dice.updater().commit().await;
    let validity = Arc::new(AtomicBool::new(false));

    assert!(
        ctx.compute(&MaybeTransient(10, validity.dupe()))
            .await?
            .is_err(),
    );

    validity.store(true, Ordering::SeqCst);
    assert!(
        ctx.compute(&MaybeTransient(10, validity.dupe()))
            .await?
            .is_err(),
    );

    drop(ctx);

    let ctx = dice.updater().commit().await;
    assert_eq!(
        ctx.compute(&MaybeTransient(10, validity.dupe())).await?,
        Ok(512)
    );

    Ok(())
}

#[tokio::test]
async fn test_wait_for_idle() -> anyhow::Result<()> {
    #[derive(Clone, Debug, Display, Derivative, Allocative)]
    #[derivative(Hash, PartialEq, Eq)]
    #[display(fmt = "{:?}", self)]
    struct TestKey {
        id: usize,

        #[allocative(skip)]
        #[derivative(PartialEq = "ignore", Hash = "ignore")]
        channel: Shared<oneshot::Receiver<()>>,
    }

    impl Dupe for TestKey {}

    #[async_trait]
    impl Key for TestKey {
        type Value = ();

        async fn compute(
            &self,
            _ctx: &DiceComputations,
            cancellations: &CancellationContext,
        ) -> Self::Value {
            cancellations
                .critical_section(|| self.channel.clone())
                .await
                .unwrap()
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            false
        }
    }

    let dice = DiceLegacy::builder().build(DetectCycles::Enabled);

    let ctx = dice.updater().commit().await;

    let (tx, rx) = oneshot::channel();
    let rx = rx.shared();

    let key = TestKey { id: 1, channel: rx };
    let handle = ctx.temporary_spawn(move |ctx: crate::DiceTransaction, _| {
        async move { ctx.compute(&key).await }.boxed()
    });

    let idle = dice.wait_for_idle();
    futures::pin_mut!(idle);

    assert_matches!(timeout(Duration::from_secs(1), &mut idle).await, Err(..));
    assert!(!dice.is_idle());

    drop(handle);
    drop(ctx);
    assert_matches!(timeout(Duration::from_secs(1), &mut idle).await, Err(..));
    assert!(!dice.is_idle());

    tx.send(()).unwrap();
    assert_matches!(timeout(Duration::from_secs(1), &mut idle).await, Ok(..));
    assert!(dice.is_idle());

    // Still idle.
    let stays_idle = async {
        dice.wait_for_idle().await;
        dice.wait_for_idle().await;
    };
    assert_matches!(timeout(Duration::from_secs(1), stays_idle).await, Ok(..));

    Ok(())
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
    let dice = DiceLegacy::builder().build(DetectCycles::Disabled);

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

#[tokio::test]
async fn compute_and_update_uses_proper_version_numbers() -> anyhow::Result<()> {
    let dice = DiceLegacy::builder().build(DetectCycles::Enabled);

    {
        let ctx = dice.updater().commit().await;
        assert_eq!(ctx.0.0.get_version(), VersionNumber::new(0));
        assert_eq!(ctx.0.0.get_minor_version(), MinorVersion::testing_new(0));
    }

    {
        // second context that didn't have any writes should still be the same version
        let ctx = dice.updater().commit().await;
        assert_eq!(ctx.0.0.get_version(), VersionNumber::new(0));
        assert_eq!(ctx.0.0.get_minor_version(), MinorVersion::testing_new(1));

        // now we write something and commit
        let mut ctx = dice.updater();
        ctx.changed_to(vec![(Foo(1), 1)])?;
        // current version shouldn't be updated
        assert_eq!(
            ctx.existing_state().await.0.get_version(),
            VersionNumber::new(0)
        );
        assert_eq!(
            ctx.existing_state().await.0.get_minor_version(),
            MinorVersion::testing_new(1)
        );

        let mut ctx1 = dice.updater();
        // previous ctx isn't dropped, so versions shouldn't be committed yet.
        assert_eq!(
            ctx1.existing_state().await.0.get_version(),
            VersionNumber::new(0)
        );
        assert_eq!(
            ctx1.existing_state().await.0.get_minor_version(),
            MinorVersion::testing_new(1)
        );

        // if we update on the new context, nothing committed
        ctx1.changed_to(vec![(Foo(2), 2)])?;
        assert_eq!(
            ctx1.existing_state().await.0.get_version(),
            VersionNumber::new(0)
        );
        assert_eq!(
            ctx1.existing_state().await.0.get_minor_version(),
            MinorVersion::testing_new(1)
        );

        // drop a context
        ctx1.commit().await;
        // we should only have committed once, and in increasing order
        let vg = dice.global_versions.current();
        assert_eq!(
            (vg.version, *vg.minor_version_guard),
            (VersionNumber::new(1), MinorVersion::testing_new(1))
        );

        ctx.commit().await;
        // both versions finalized.
        let vg = dice.global_versions.current();
        assert_eq!(
            (vg.version, *vg.minor_version_guard),
            (VersionNumber::new(2), MinorVersion::testing_new(1))
        );
        assert!(dice.map.read().engines().iter().all(|engine| {
            engine
                .introspect()
                .versions_currently_running()
                .first()
                .is_none()
        }));
    }

    {
        let mut ctx = dice.updater();
        assert_eq!(
            ctx.existing_state().await.0.get_version(),
            VersionNumber::new(2)
        );
        assert_eq!(
            ctx.existing_state().await.0.get_minor_version(),
            MinorVersion::testing_new(2)
        );

        ctx.changed_to(vec![(Foo(3), 3)])?;
        assert_eq!(
            ctx.existing_state().await.0.get_version(),
            VersionNumber::new(2)
        );
        assert_eq!(
            ctx.existing_state().await.0.get_minor_version(),
            MinorVersion::testing_new(2)
        );

        ctx.commit().await;
        let vg = dice.global_versions.current();
        assert_eq!(
            (vg.version, *vg.minor_version_guard),
            (VersionNumber::new(3), MinorVersion::testing_new(1))
        );
        assert!(dice.map.read().engines().iter().all(|engine| {
            engine
                .introspect()
                .versions_currently_running()
                .first()
                .is_none()
        }));
    }

    Ok(())
}

#[test]
fn test_active_transaction_count() {
    let dice = Arc::new(DiceLegacy::new(DiceData::new(), DetectCycles::Enabled));
    assert_eq!(0, dice.metrics().active_transaction_count);
    let ctx = dice.updater().commit();
    assert_eq!(1, dice.metrics().active_transaction_count);
    drop(ctx);
    assert_eq!(0, dice.metrics().active_transaction_count);
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

    let dice = DiceLegacy::builder().build(DetectCycles::Enabled);
    let mut updater = dice.updater();

    assert!(updater.changed_to([(Invalid, ())]).is_err());
}
