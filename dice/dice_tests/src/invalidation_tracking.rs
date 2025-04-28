/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_futures::cancellation::CancellationContext;
use derive_more::Display;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::DiceKeyTrackedInvalidationPaths;
use dice::DiceTrackedInvalidationPath;
use dice::InjectedKey;
use dice::InvalidationSourcePriority;
use dice::Key;
use dupe::Dupe;
use futures::future::FutureExt;
use gazebo::prelude::*;
use gazebo::variants::VariantName;

#[derive(Allocative, Clone, Copy, Debug, Display, Eq, PartialEq, Hash)]
struct NormalInjected(u32);

#[derive(Allocative, Clone, Copy, Debug, Display, Eq, PartialEq, Hash)]
struct HighInjected(u32);

#[derive(Allocative, Clone, Copy, Debug, Display, Eq, PartialEq, Hash)]
struct NormalChanged(u32);

#[derive(Allocative, Clone, Copy, Debug, Display, Eq, PartialEq, Hash)]
struct HighChanged(u32);

impl InjectedKey for NormalInjected {
    type Value = u32;
    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        false
    }
}

impl InjectedKey for HighInjected {
    type Value = u32;
    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        false
    }

    fn invalidation_source_priority() -> InvalidationSourcePriority {
        InvalidationSourcePriority::High
    }
}

#[async_trait]
impl Key for NormalChanged {
    type Value = u32;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        ctx.compute(&NormalInjected(self.0)).await.unwrap()
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        false
    }
}

#[async_trait]
impl Key for HighChanged {
    type Value = u32;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        ctx.compute(&HighInjected(self.0)).await.unwrap()
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        false
    }

    fn invalidation_source_priority() -> InvalidationSourcePriority {
        InvalidationSourcePriority::High
    }
}

#[derive(Allocative, Debug, Clone, Dupe)]
struct CapturedInvalidationPaths(#[allocative(skip)] Arc<DiceKeyTrackedInvalidationPaths>);

impl CapturedInvalidationPaths {
    fn new(paths: DiceKeyTrackedInvalidationPaths) -> Self {
        Self(Arc::new(paths))
    }
}

enum ExpectedInvalidation {
    Clean,
    #[allow(unused)]
    Unknown,
    Invalidated(Vec<(String, usize)>),
}

impl ExpectedInvalidation {
    fn item<K: Key>(k: K, v: usize) -> (String, usize) {
        (format!("{}({})", K::key_type_name(), k), v)
    }
}

#[track_caller]
fn assert_invalidations(
    normal: ExpectedInvalidation,
    high: ExpectedInvalidation,
    actual: CapturedInvalidationPaths,
) {
    assert_single_invalidation(normal, &actual.0.normal_priority_path, "normal");
    assert_single_invalidation(high, &actual.0.high_priority_path, "high");
}

#[track_caller]
fn assert_single_invalidation(
    expected: ExpectedInvalidation,
    actual: &DiceTrackedInvalidationPath,
    priority: &str,
) {
    match (expected, actual) {
        (ExpectedInvalidation::Clean, DiceTrackedInvalidationPath::Clean) => {}
        (ExpectedInvalidation::Clean, _) => {
            panic!(
                "{} invalidation mismatch. Expected DiceTrackedInvalidationPath::Clean, got DiceTrackedInvalidationPath::{}",
                priority,
                actual.variant_name()
            )
        }
        (ExpectedInvalidation::Unknown, DiceTrackedInvalidationPath::Unknown) => {}
        (ExpectedInvalidation::Unknown, _) => {
            panic!(
                "{} invalidation mismatch. Expected DiceTrackedInvalidationPath::Unknown, got DiceTrackedInvalidationPath::{}",
                priority,
                actual.variant_name()
            )
        }
        (
            ExpectedInvalidation::Invalidated(expected),
            DiceTrackedInvalidationPath::Invalidated(actual),
        ) => {
            assert_eq!(
                expected,
                actual.get_invalidation_path().map(|v| (
                    format!("{}({})", v.key.key_type_name(), v.key),
                    v.version.value()
                ))
            )
        }
        (ExpectedInvalidation::Invalidated(..), _) => {
            panic!(
                "{} invalidation mismatch. Expected DiceTrackedInvalidationPath::Invalidated, got DiceTrackedInvalidationPath::{}",
                priority,
                actual.variant_name()
            )
        }
    }
}

#[test]
fn test_compute_tracks_invalidations() -> anyhow::Result<()> {
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
        let dice = {
            let builder = Dice::modern();
            builder.build(DetectCycles::Enabled)
        };

        // We want to set up the invalidation sources such that the FooInjected are all invalidated at `2 * self.0` (i.e NormalInjected(4)
        // is invalidated at 40) and the FooChanged forms are at `2 * self.0 + 1`

        for i in 0..10 {
            let mut updater = dice.updater();
            updater.changed_to([(NormalInjected(i), i)])?;
            updater.changed_to([(HighInjected(i), i)])?;
            updater.commit().await;

            let mut updater = dice.updater();
            updater.changed([NormalChanged(i)])?;
            updater.changed([HighChanged(i)])?;
            updater.commit().await;
        }

        #[derive(Allocative, Clone, Copy, Debug, Display, Eq, PartialEq, Hash)]
        struct Top(u32);

        #[async_trait]
        impl Key for Top {
            type Value = CapturedInvalidationPaths;
            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellations: &CancellationContext,
            ) -> Self::Value {
                match self.0 {
                    0 => {}
                    1 => {
                        ctx.compute(&HighInjected(0)).await.unwrap();
                        ctx.compute(&NormalInjected(1)).await.unwrap();
                    }
                    2 => {
                        ctx.compute(&HighChanged(1)).await.unwrap();
                        ctx.compute(&NormalChanged(0)).await.unwrap();
                    }
                    3 => {
                        ctx.compute(&HighChanged(0)).await.unwrap();
                        ctx.compute2(
                            |ctx| {
                                async move { ctx.compute(&NormalInjected(1)).await.unwrap() }
                                    .boxed()
                            },
                            |ctx| {
                                async move { ctx.compute(&NormalChanged(1)).await.unwrap() }.boxed()
                            },
                        )
                        .await;
                    }
                    n if n < 8 => {
                        ctx.compute(&Top(n - 1)).await.unwrap();
                    }
                    k => {
                        panic!("don't request such a big key {}", k)
                    }
                }

                CapturedInvalidationPaths::new(ctx.get_invalidation_paths())
            }

            fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
                false
            }
        }

        let mut ctx = dice.updater().commit().await;

        assert_invalidations(
            ExpectedInvalidation::Clean,
            ExpectedInvalidation::Clean,
            ctx.compute(&Top(0)).await?,
        );
        assert_invalidations(
            ExpectedInvalidation::Invalidated(vec![ExpectedInvalidation::item(
                NormalInjected(1),
                3,
            )]),
            ExpectedInvalidation::Invalidated(vec![ExpectedInvalidation::item(HighInjected(0), 1)]),
            ctx.compute(&Top(1)).await?,
        );
        assert_invalidations(
            ExpectedInvalidation::Invalidated(vec![ExpectedInvalidation::item(HighChanged(1), 4)]),
            ExpectedInvalidation::Invalidated(vec![ExpectedInvalidation::item(HighChanged(1), 4)]),
            ctx.compute(&Top(2)).await?,
        );
        assert_invalidations(
            ExpectedInvalidation::Invalidated(vec![ExpectedInvalidation::item(
                NormalChanged(1),
                4,
            )]),
            ExpectedInvalidation::Invalidated(vec![ExpectedInvalidation::item(HighChanged(0), 2)]),
            ctx.compute(&Top(3)).await?,
        );

        assert_invalidations(
            ExpectedInvalidation::Invalidated(vec![
                ExpectedInvalidation::item(NormalChanged(1), 4),
                ExpectedInvalidation::item(Top(3), 4),
                ExpectedInvalidation::item(Top(4), 4),
                ExpectedInvalidation::item(Top(5), 4),
                ExpectedInvalidation::item(Top(6), 4),
            ]),
            ExpectedInvalidation::Invalidated(vec![
                ExpectedInvalidation::item(HighChanged(0), 2),
                ExpectedInvalidation::item(Top(3), 2),
                ExpectedInvalidation::item(Top(4), 2),
                ExpectedInvalidation::item(Top(5), 2),
                ExpectedInvalidation::item(Top(6), 2),
            ]),
            ctx.compute(&Top(7)).await?,
        );

        Ok(())
    })
}

#[test]
fn test_compute_tracks_invalidations_over_versions() -> anyhow::Result<()> {
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
        let dice = {
            let builder = Dice::modern();
            builder.build(DetectCycles::Enabled)
        };

        #[derive(Allocative, Clone, Copy, Debug, Display, Eq, PartialEq, Hash)]
        struct Top(u32);

        #[async_trait]
        impl Key for Top {
            type Value = CapturedInvalidationPaths;
            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellations: &CancellationContext,
            ) -> Self::Value {
                ctx.compute(&HighChanged(self.0)).await.unwrap();

                CapturedInvalidationPaths::new(ctx.get_invalidation_paths())
            }

            fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
                false
            }
        }

        let mut updater = dice.updater();
        updater.changed_to([(HighInjected(0), 0)])?;
        updater.commit().await;

        let mut ctx = dice.updater().commit().await;
        assert_invalidations(
            ExpectedInvalidation::Invalidated(vec![
                ExpectedInvalidation::item(HighInjected(0), 1),
                ExpectedInvalidation::item(HighChanged(0), 1),
            ]),
            ExpectedInvalidation::Invalidated(vec![
                ExpectedInvalidation::item(HighInjected(0), 1),
                ExpectedInvalidation::item(HighChanged(0), 1),
            ]),
            ctx.compute(&Top(0)).await?,
        );

        let mut updater = dice.updater();
        updater.changed_to([(HighInjected(0), 1)])?;
        updater.commit().await;

        let mut ctx = dice.updater().commit().await;
        assert_invalidations(
            ExpectedInvalidation::Invalidated(vec![
                ExpectedInvalidation::item(HighInjected(0), 2),
                ExpectedInvalidation::item(HighChanged(0), 2),
            ]),
            ExpectedInvalidation::Invalidated(vec![
                ExpectedInvalidation::item(HighInjected(0), 2),
                ExpectedInvalidation::item(HighChanged(0), 2),
            ]),
            ctx.compute(&Top(0)).await?,
        );

        let mut updater = dice.updater();
        updater.changed([HighChanged(0)])?;
        updater.commit().await;

        let mut ctx = dice.updater().commit().await;
        assert_invalidations(
            ExpectedInvalidation::Invalidated(vec![ExpectedInvalidation::item(HighChanged(0), 3)]),
            ExpectedInvalidation::Invalidated(vec![ExpectedInvalidation::item(HighChanged(0), 3)]),
            ctx.compute(&Top(0)).await?,
        );

        Ok(())
    })
}
