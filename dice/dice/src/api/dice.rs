/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! The dynamic incremental caching computation engine that powers buckv2.
//!
//! The computation engine will output values corresponding to given `Key`s,
//! reusing previously computed values when possible. `Key`s computations are
//! allowed to request other `Key`s via a `ComputationContext`.
//!
//! Example:
//! ```
//! mod c {
//!
//!     /// declaring computations:
//!     use async_trait::async_trait;
//!     use derive_more::Display;
//!     use dice::{Key, InjectedKey, DiceComputations, DiceDataBuilder, DiceData, DiceTransactionUpdater};
//!     use std::sync::Arc;
//!     use allocative::Allocative;
//! use more_futures::cancellation::CancellationContext;
//!
//!     /// A configuration computation that consists of values that are pre-computed outside of DICE
//!     pub struct InjectConfigs<'compute>(&'compute DiceComputations);
//!
//!     impl<'compute> InjectConfigs<'compute> {
//!         pub async fn get_config(&self) -> usize {
//!             self.0.compute(&ConfigKey).await.unwrap()
//!         }
//!     }
//!
//!     #[derive(Clone, Debug, Display, Eq, Hash, PartialEq, Allocative)]
//!     #[display(fmt = "{:?}", self)]
//!     struct ConfigKey;
//!
//!     #[async_trait]
//!     impl InjectedKey for ConfigKey {
//!         type Value = usize;
//!
//!         fn equality(x: &Self::Value,y: &Self::Value) -> bool {
//!             x == y
//!         }
//!     }
//!
//!     pub struct MyComputation<'compute>(&'compute DiceComputations);
//!
//!     impl<'compute> MyComputation<'compute> {
//!         // declaring a computation function
//!         pub async fn compute_a(&self, a: usize, s: String) -> Arc<String> {
//!             #[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
//!             #[display(fmt = "{:?}", self)]
//!             struct ComputeA(usize, String);
//!
//!             #[async_trait]
//!             impl Key for ComputeA {
//!                 type Value = Arc<String>;
//!
//!                 async fn compute(&self, ctx: &DiceComputations, _cancellations: &CancellationContext) -> Self::Value {
//!                     // request for other computations on the self
//!                     let n = ctx.my_computation().compute_b(self.0).await;
//!                     Arc::new(self.1.repeat(n))
//!                 }
//!
//!                 fn equality(x: &Self::Value,y: &Self::Value) -> bool {
//!                     x == y
//!                 }
//!             }
//!
//!             self.0.compute(&ComputeA(a, s)).await.unwrap()
//!         }
//!
//!         // second computation function
//!         pub async fn compute_b(&self, a: usize) -> usize {
//!                 self.0.compute(&ComputeB(a)).await.unwrap()
//!         }
//!     }
//!
//!     #[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
//!     #[display(fmt = "{:?}", self)]
//!     struct ComputeB(usize);
//!
//!     #[async_trait]
//!     impl Key for ComputeB {
//!         type Value = usize;
//!
//!         async fn compute(&self, ctx: &DiceComputations, cancellations: &CancellationContext) -> Self::Value {
//!             self.0 + ctx.injected_configs().get_config().await + ctx.global_data().static_data().len()
//!         }
//!
//!         fn equality(x: &Self::Value,y: &Self::Value) -> bool {
//!             x == y
//!         }
//!     }
//!
//!     // trait to register the computation to DICE
//!     pub trait HasMyComputation {
//!         fn my_computation(&self) -> MyComputation;
//!     }
//!
//!     // attach the declared computation to DICE via the context
//!     impl HasMyComputation for DiceComputations {
//!         fn my_computation(&self) -> MyComputation {
//!             MyComputation(self)
//!         }
//!     }
//!
//!     // trait to register the precomputed configs to DICE
//!     pub trait HasInjectedConfig {
//!         fn injected_configs(&self) -> InjectConfigs;
//!     }
//!
//!     impl HasInjectedConfig for DiceComputations {
//!         fn injected_configs(&self) -> InjectConfigs {
//!             InjectConfigs(self)
//!         }
//!     }
//!
//!     pub trait SetInjectedConfig {
//!         fn inject_config(&mut self, i: usize);
//!     }
//!
//!     impl SetInjectedConfig for DiceTransactionUpdater {
//!         /// ways to inject the precomputed values to dice
//!         fn inject_config(&mut self, i: usize) {
//!             self.changed_to(vec![(ConfigKey, i)]).unwrap()
//!         }
//!     }
//!
//!     pub trait StaticData {
//!         fn static_data(&self) -> &String;
//!     }
//!
//!     impl StaticData for DiceData {
//!         fn static_data(&self) -> &String {
//!             self.get::<String>().unwrap()
//!         }
//!     }
//!
//!     pub trait SetStaticData {
//!         fn set_static(&mut self, s: String);
//!     }
//!
//!     impl SetStaticData for DiceDataBuilder {
//!         fn set_static(&mut self, s: String) {
//!             self.set(s);
//!         }
//!     }
//! }
//!
//! /// how to use computations
//! use dice::{Dice, DetectCycles};
//! use std::sync::Arc;
//! use c::*;
//!
//! let mut rt = tokio::runtime::Runtime::new().unwrap();
//! let mut builder = Dice::builder();
//! builder.set_static("len4".into());
//! let engine = builder.build(DetectCycles::Disabled);
//!
//! // inject config
//! let mut ctx = engine.updater();
//! ctx.inject_config(0);
//!
//! let ctx = rt.block_on(ctx.commit());
//!
//! // request the computation from DICE
//! rt.block_on(async {
//!     assert_eq!("aaaaaaaa", &*ctx.my_computation().compute_a(4, "a".into()).await);
//! });
//!
//! let mut ctx = engine.updater();
//! ctx.inject_config(2);
//!
//! let ctx = rt.block_on(ctx.commit());
//!
//! // request the computation from DICE
//! rt.block_on(async {
//!     assert_eq!("aaaaaaaaaa", &*ctx.my_computation().compute_a(4, "a".into()).await);
//! });
//! ```

use std::fmt::Debug;
use std::io::Write;
use std::sync::Arc;

use allocative::Allocative;
use futures::future::Future;
use serde::Serializer;

use crate::api::cycles::DetectCycles;
use crate::api::transaction::DiceTransactionUpdater;
use crate::api::user_data::UserComputationData;
use crate::api::which::WhichSpawner;
use crate::metrics::Metrics;
use crate::DiceDataBuilderImpl;
use crate::DiceImplementation;
use crate::WhichDice;

/// An incremental computation engine that executes arbitrary computations that
/// maps `Key`s to values.
#[derive(Allocative, Debug)]
pub struct Dice {
    pub(crate) implementation: DiceImplementation,
}

impl Dice {
    pub fn builder() -> DiceDataBuilder {
        DiceDataBuilder(DiceDataBuilderImpl::new_legacy())
    }

    pub fn modern() -> DiceDataBuilder {
        DiceDataBuilder(DiceDataBuilderImpl::new_modern())
    }

    pub(crate) fn new(implementation: DiceImplementation) -> Arc<Self> {
        Arc::new(Self { implementation })
    }

    pub fn updater(self: &Arc<Dice>) -> DiceTransactionUpdater {
        self.implementation.updater()
    }

    pub fn updater_with_data(
        self: &Arc<Dice>,
        extra: UserComputationData,
    ) -> DiceTransactionUpdater {
        self.implementation.updater_with_data(extra)
    }

    pub fn serialize_tsv(
        &self,
        nodes: impl Write,
        edges: impl Write,
        nodes_currently_running: impl Write,
    ) -> anyhow::Result<()> {
        self.implementation
            .serialize_tsv(nodes, edges, nodes_currently_running)
    }

    pub fn serialize_serde<S>(&self, serializer: S) -> Result<(), S::Error>
    where
        S: Serializer,
    {
        self.implementation.serialize_serde(serializer)
    }

    pub fn detect_cycles(&self) -> &DetectCycles {
        self.implementation.detect_cycles()
    }

    pub fn which_dice(&self) -> WhichDice {
        match self.implementation {
            DiceImplementation::Legacy(_) => WhichDice::Legacy,
            DiceImplementation::Modern(_) => WhichDice::Modern,
        }
    }

    pub fn metrics(&self) -> Metrics {
        self.implementation.metrics()
    }

    /// Wait until all active versions have exited.
    pub fn wait_for_idle(&self) -> impl Future<Output = ()> + 'static {
        self.implementation.wait_for_idle()
    }

    /// true when there are no active tasks nor transactions alive
    pub fn is_idle(&self) -> bool {
        self.implementation.is_idle()
    }
}

pub struct DiceDataBuilder(DiceDataBuilderImpl);

impl DiceDataBuilder {
    pub fn set<K: Send + Sync + 'static>(&mut self, val: K) {
        self.0.set(val);
    }

    pub fn build(self, detect_cycles: DetectCycles) -> Arc<Dice> {
        self.build_with_which_spawner(detect_cycles, WhichSpawner::ExplicitCancel)
    }

    pub fn build_with_which_spawner(
        self,
        detect_cycles: DetectCycles,
        which_spawner: WhichSpawner,
    ) -> Arc<Dice> {
        self.0.build(detect_cycles, which_spawner)
    }
}

pub mod testing {
    use crate::api::cycles::DetectCycles;
    use crate::api::key::Key;
    use crate::api::transaction::DiceTransactionUpdater;
    use crate::api::user_data::UserComputationData;
    use crate::Dice;
    use crate::DiceDataBuilder;

    /// Testing utility that can be used to build a specific `DiceComputation` where certain keys
    /// of computation mocked to return a specific result.
    ///
    /// TODO(bobyf): ideally, we want something where we don't have to use the specific keys
    /// but rather the computation function, like `mock.expect(|c| c.other_compute(4), "4 res")`
    pub struct DiceBuilder {
        builder: DiceDataBuilder,
        mocked: Vec<Box<dyn FnOnce(&mut DiceTransactionUpdater) -> anyhow::Result<()>>>,
    }

    impl DiceBuilder {
        pub fn new() -> Self {
            let builder = Dice::builder();

            Self {
                builder,
                mocked: Vec::new(),
            }
        }

        pub fn set_data(mut self, setter: impl FnOnce(&mut DiceDataBuilder)) -> Self {
            setter(&mut self.builder);
            self
        }

        /// mocks the call of compute for the key `expected_k` so that it returns `expected_res`
        pub fn mock_and_return<K>(mut self, expected_k: K, expected_res: K::Value) -> Self
        where
            K: Key,
        {
            self.mocked.push(Box::new(move |ctx| {
                Ok(ctx.changed_to(vec![(expected_k, expected_res)])?)
            }));
            self
        }

        pub fn build(self, extra: UserComputationData) -> anyhow::Result<DiceTransactionUpdater> {
            let dice = self.builder.build(DetectCycles::Enabled);
            let mut ctx = dice.updater_with_data(extra);

            self.mocked.into_iter().try_for_each(|f| f(&mut ctx))?;
            Ok(ctx)
        }
    }
}
