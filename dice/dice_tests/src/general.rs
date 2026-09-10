/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::DiceKeyDyn;
use dice::EqualityBehavior;
use dice::InjectedKey;
use dice::Key;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use pagable::Pagable;
use pagable::pagable_typetag;

// dice graph storage needs to not reuse deps just because the value hasn't changed
#[tokio::test]
async fn test_dice_recompute_doesnt_reuse_wrong_deps() -> anyhow::Result<()> {
    #[derive(
        Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
    )]
    #[display("{:?}", self)]
    #[pagable_typetag(DiceKeyDyn)]
    struct Leaf(u32);

    impl InjectedKey for Leaf {
        type Value = u32;
        fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
            dice::NoValueSerialize::<Self::Value>::new()
        }
        fn equality_behavior() -> EqualityBehavior<Self::Value> {
            EqualityBehavior::Compare(|x, y| *x == *y)
        }
    }

    #[derive(
        Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
    )]
    #[display("{:?}", self)]
    #[pagable_typetag(DiceKeyDyn)]
    struct Derived;

    #[async_trait]
    impl Key for Derived {
        type Value = u32;

        fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
            dice::NoValueSerialize::<Self::Value>::new()
        }

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> u32 {
            let x = *ctx.compute(&Leaf(0)).await.unwrap();
            *ctx.compute(&Leaf(x)).await.unwrap()
        }

        fn equality_behavior() -> EqualityBehavior<Self::Value> {
            EqualityBehavior::Compare(|x, y| *x == *y)
        }
    }

    let dice = Dice::builder().build(DetectCycles::Enabled);

    let mut updater = dice.updater();
    updater.changed_to([(Leaf(0), 1), (Leaf(1), 100), (Leaf(2), 200)])?;
    let ctx1 = updater.commit().await;

    let mut updater = dice.updater();
    updater.changed_to([(Leaf(0), 1), (Leaf(1), 300), (Leaf(2), 200)])?;
    let ctx2 = updater.commit().await;

    let mut updater = dice.updater();
    updater.changed_to([(Leaf(0), 2), (Leaf(1), 400), (Leaf(2), 100)])?;
    let ctx3 = updater.commit().await;

    assert_eq!(*ctx1.compute(&Derived).await.unwrap(), 100);
    assert_eq!(*ctx3.compute(&Derived).await.unwrap(), 100);
    assert_eq!(*ctx2.compute(&Derived).await.unwrap(), 300);

    Ok(())
}

#[tokio::test]
async fn test_dice_clear_doesnt_break_ongoing_computation() -> anyhow::Result<()> {
    #[derive(
        Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
    )]
    #[display("{:?}", self)]
    #[pagable_typetag(DiceKeyDyn)]
    struct Fib(u32);

    #[async_trait]
    impl Key for Fib {
        type Value = Option<u32>;
        fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
            dice::NoValueSerialize::<Self::Value>::new()
        }

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Option<u32> {
            Some(match self.0 {
                0 => 1,
                1 => 1,
                n => {
                    (*ctx.compute(&Fib(n - 1)).await.ok()?)?
                        + (*ctx.compute(&Fib(n - 2)).await.ok()?)?
                }
            })
        }

        fn equality_behavior() -> EqualityBehavior<Self::Value> {
            EqualityBehavior::Compare(|_x, _y| false)
        }
    }

    let dice = Dice::builder().build(DetectCycles::Enabled);
    let updater = dice.updater();
    let ctx1 = updater.commit().await;

    ctx1.compute(&Fib(3)).await?;

    let updater = dice.updater();
    updater.unstable_take();

    let res = ctx1.compute(&Fib(10)).await;

    assert!(res.is_err(), "Expected `Err(_)`, got `{res:?}`");

    Ok(())
}

#[test]
fn test_dice_clear_doesnt_cause_inject_compute() {
    // Detecting that a dice compute panicked is actually kinda tricky, in normal flow
    // that is a hard error but in tests it instead just looks to dice like the node is cancelled.
    // We detect it by configuring the runtime to shutdown and panic itself if any task panics, but
    // that only works right now with the current_thread runtime.
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .unhandled_panic(tokio::runtime::UnhandledPanic::ShutdownRuntime)
        .build()
        .unwrap();

    // Spawn the root task
    rt.block_on(async {
        #[derive(
            Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
        )]
        #[display("{:?}", self)]
        #[pagable_typetag(DiceKeyDyn)]
        struct Node;

        #[async_trait]
        impl Key for Node {
            type Value = u32;
            fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
                dice::NoValueSerialize::<Self::Value>::new()
            }

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellations: &CancellationContext,
            ) -> u32 {
                drop(ctx.compute(&Leaf).await);
                1
            }

            fn equality_behavior() -> EqualityBehavior<Self::Value> {
                EqualityBehavior::Compare(|_x, _y| false)
            }
        }

        #[derive(
            Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
        )]
        #[display("{:?}", self)]
        #[pagable_typetag(DiceKeyDyn)]
        struct Leaf;

        impl InjectedKey for Leaf {
            type Value = u32;
            fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
                dice::NoValueSerialize::<Self::Value>::new()
            }

            fn equality_behavior() -> EqualityBehavior<Self::Value> {
                EqualityBehavior::Compare(|_x, _y| false)
            }
        }

        let dice = Dice::builder().build(DetectCycles::Enabled);
        let mut updater = dice.updater();
        drop(updater.changed_to([(Leaf, 1)]));
        let ctx1 = updater.commit().await;
        let fut = ctx1.compute(&Node);

        let updater = dice.updater();
        updater.unstable_take();

        drop(fut.await);
    });
}

/// Regression: a dep dropped from a key's compute on a later run must not
/// leave a stale reverse-dep edge behind. Once `K` stops depending on
/// `Right`, later changes to `Right` must not invalidate `K` or drag it
/// through a dep-check on the next lookup.
#[tokio::test]
async fn dropped_dep_does_not_leave_stale_rdep_edge() -> anyhow::Result<()> {
    use std::sync::Arc;
    use std::sync::Mutex;

    use allocative::Allocative;
    use dice::DiceEvent;
    use dice::DiceEventListener;
    use dice::UserComputationData;
    use dupe::Dupe;
    use pagable::Pagable;
    use pagable::pagable_typetag;

    #[derive(
        Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
    )]
    #[display("Selector")]
    #[pagable_typetag(DiceKeyDyn)]
    struct Selector;
    impl InjectedKey for Selector {
        type Value = bool;
        fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
            dice::NoValueSerialize::<Self::Value>::new()
        }
        fn equality_behavior() -> dice::EqualityBehavior<Self::Value> {
            dice::EqualityBehavior::Compare(|x, y| x == y)
        }
    }

    #[derive(
        Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
    )]
    #[display("Left")]
    #[pagable_typetag(DiceKeyDyn)]
    struct Left;
    impl InjectedKey for Left {
        type Value = u32;
        fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
            dice::NoValueSerialize::<Self::Value>::new()
        }
        fn equality_behavior() -> dice::EqualityBehavior<Self::Value> {
            dice::EqualityBehavior::Compare(|x, y| x == y)
        }
    }

    #[derive(
        Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
    )]
    #[display("Right")]
    #[pagable_typetag(DiceKeyDyn)]
    struct Right;
    impl InjectedKey for Right {
        type Value = u32;
        fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
            dice::NoValueSerialize::<Self::Value>::new()
        }
        fn equality_behavior() -> dice::EqualityBehavior<Self::Value> {
            dice::EqualityBehavior::Compare(|x, y| x == y)
        }
    }

    /// `K = Left + Right` when `Selector` is true, else `K = Left`. The
    /// dep set includes `Right` only in the first branch, so flipping
    /// `Selector` shrinks `K`'s deps.
    #[derive(
        Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
    )]
    #[display("K")]
    #[pagable_typetag(DiceKeyDyn)]
    struct K;

    #[async_trait]
    impl Key for K {
        type Value = u32;
        fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
            dice::NoValueSerialize::<Self::Value>::new()
        }
        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> u32 {
            let sel = *ctx.compute(&Selector).await.unwrap();
            let l = *ctx.compute(&Left).await.unwrap();
            if sel {
                l + *ctx.compute(&Right).await.unwrap()
            } else {
                l
            }
        }
        fn equality_behavior() -> dice::EqualityBehavior<Self::Value> {
            dice::EqualityBehavior::Compare(|x, y| x == y)
        }
    }

    /// Records the sequence of `DiceEvent`s so the test can assert what
    /// dice did (or did not do) for a given key on a given lookup.
    #[derive(Allocative)]
    struct Recorder {
        #[allocative(skip)]
        events: Arc<Mutex<Vec<DiceEvent>>>,
    }
    impl DiceEventListener for Recorder {
        fn event(&self, ev: DiceEvent) {
            self.events.lock().unwrap().push(ev);
        }
    }

    let dice = Dice::builder().build(DetectCycles::Disabled);

    // v1: Selector=true, Left=1, Right=10. K depends on {Selector, Left, Right}.
    let mut updater = dice.updater();
    updater.changed_to([(Selector, true)])?;
    updater.changed_to([(Left, 1u32)])?;
    updater.changed_to([(Right, 10u32)])?;
    let ctx = updater.commit().await;
    assert_eq!(*ctx.compute(&K).await.unwrap(), 11);
    drop(ctx);

    // v2: flip Selector to false, forcing K's recompute. K now depends on
    // {Selector, Left} only — Right has been dropped from the dep set.
    let mut updater = dice.updater();
    updater.changed_to([(Selector, false)])?;
    let ctx = updater.commit().await;
    assert_eq!(*ctx.compute(&K).await.unwrap(), 1);
    drop(ctx);

    // v3: change Right. With the fix, K is not in Right's rdep set, so
    // Right's change must not dirty K. On lookup, K's slot is Match — no
    // ComputeStarted, no CheckDepsStarted for K.
    let events = Arc::new(Mutex::new(Vec::new()));
    let mut data = UserComputationData::new();
    data.tracker = Arc::new(Recorder {
        events: events.clone(),
    });
    let mut updater = dice.updater_with_data(data);
    updater.changed_to([(Right, 999u32)])?;
    let ctx = updater.commit().await;
    assert_eq!(*ctx.compute(&K).await.unwrap(), 1);

    let saw_k_touched = events.lock().unwrap().iter().any(|ev| {
        matches!(
            ev,
            DiceEvent::ComputeStarted { key_type: "K" }
                | DiceEvent::CheckDepsStarted { key_type: "K" }
        )
    });
    assert!(
        !saw_k_touched,
        "K should not be recomputed nor dep-checked at v3 (Right is no longer its dep)",
    );

    Ok(())
}
