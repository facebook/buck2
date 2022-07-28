/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! The distributed incremental caching computation engine that powers buckv2.
//!
//! The computation engine will output values corresponding to given 'Key's,
//! reusing previously computed values when possible. 'Key's computations are
//! allowed to request other 'Key's via a 'ComputationContext'.
//!
//! Example:
//! ```
//! mod c {
//!
//!     /// declaring computations:
//!     use async_trait::async_trait;
//!     use derive_more::Display;
//!     use dice::{Key, InjectedKey, DiceComputations, DiceDataBuilder, data::DiceData };
//!     use std::sync::Arc;
//!
//!     /// A configuration computation that consists of values that are pre-computed outside of DICE
//!     pub struct InjectConfigs<'compute>(&'compute DiceComputations);
//!
//!     impl<'compute> InjectConfigs<'compute> {
//!         /// ways to inject the precomputed values to dice
//!         pub fn inject(&self, i: usize) {
//!             self.0.changed_to(vec![(ConfigKey, i)])
//!         }
//!
//!         pub async fn get_config(&self) -> usize {
//!             self.0.compute(&ConfigKey).await.unwrap()
//!         }
//!     }
//!
//!     #[derive(Clone, Debug, Display, Eq, Hash, PartialEq)]
//!     #[display(fmt = "{:?}", self)]
//!     struct ConfigKey;
//!
//!     #[async_trait]
//!     impl InjectedKey for ConfigKey {
//!         type Value = usize;
//!
//!         fn compare(x: &Self::Value,y: &Self::Value) -> bool {
//!             x == y
//!         }
//!     }
//!
//!     pub struct MyComputation<'compute>(&'compute DiceComputations);
//!
//!     impl<'compute> MyComputation<'compute> {
//!         // declaring a computation function
//!         pub async fn compute_a(&self, a: usize, s: String) -> Arc<String> {
//!             #[derive(Clone, Display, Debug, Eq, Hash, PartialEq)]
//!             #[display(fmt = "{:?}", self)]
//!             struct ComputeA(usize, String);
//!
//!             #[async_trait]
//!             impl Key for ComputeA {
//!                 type Value = Arc<String>;
//!
//!                 async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
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
//!
//!         // computations can choose to expose specific compute functions as invalidatable,
//!         // while leaving others (e.g. compute_a) not invalidatable from a user perspective
//!         pub fn changed_b(&self, a: usize) {
//!             self.0.changed(vec![ComputeB(a)])
//!         }
//!     }
//!
//!     #[derive(Clone, Display, Debug, Eq, Hash, PartialEq)]
//!     #[display(fmt = "{:?}", self)]
//!     struct ComputeB(usize);
//!
//!     #[async_trait]
//!     impl Key for ComputeB {
//!         type Value = usize;
//!
//!         async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
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
//! use dice::{Dice, cycles::DetectCycles};
//! use std::sync::Arc;
//! use c::*;
//!
//! let mut rt = tokio::runtime::Runtime::new().unwrap();
//! let mut builder = Dice::builder();
//! builder.set_static("len4".into());
//! let engine = builder.build(DetectCycles::Disabled);
//!
//! // inject config
//! let ctx = engine.ctx();
//! ctx.injected_configs().inject(0);
//!
//! let ctx = ctx.commit();
//!
//! // request the computation from DICE
//! rt.block_on(async {
//!     assert_eq!("aaaaaaaa", &*ctx.my_computation().compute_a(4, "a".into()).await);
//! });
//!
//! let ctx = engine.ctx();
//! ctx.injected_configs().inject(2);
//!
//! let ctx = ctx.commit();
//!
//! // request the computation from DICE
//! rt.block_on(async {
//!     assert_eq!("aaaaaaaaaa", &*ctx.my_computation().compute_a(4, "a".into()).await);
//! });
//! ```

#![feature(async_closure)]
#![feature(box_syntax)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(test)]
// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]
// This sometimes flag false positives where proc-macros expand pass by value into pass by refs
#![allow(clippy::trivially_copy_pass_by_ref)]

#[macro_use]
extern crate maplit;

#[macro_use]
extern crate gazebo;

#[macro_use]
extern crate tracing;

use std::fmt::Debug;
use std::io::Write;
use std::sync::Arc;
use std::sync::RwLock;
use std::sync::Weak;

use async_trait::async_trait;
use bincode::Options;
use gazebo::prelude::*;
use indexmap::IndexSet;

use crate::ctx::ComputationData;
use crate::ctx::DiceComputationImpl;
use crate::cycles::DetectCycles;
use crate::cycles::RequestedKey;
use crate::data::DiceData;
use crate::incremental::versions::VersionTracker;
use crate::incremental::IncrementalEngine;
use crate::incremental::StorageType;
use crate::map::DiceMap;

pub mod cycles;
pub mod data;
mod dice_future;
mod dice_task;
mod future_handle;
mod incremental;
mod injected;
pub mod introspection;
pub(crate) mod key;
mod map;
pub(crate) mod opaque;
pub(crate) mod projection;
mod sync_handle;

#[cfg(test)]
mod tests;

// ctx contains pub data that we don't want to expose, so we hide the whole mod but expose just the
// data we want to expose
mod ctx;

pub use ctx::DiceComputations;
pub use ctx::DiceEvent;
pub use ctx::DiceTracker;
pub use ctx::DiceTransaction;
pub use ctx::UserComputationData;
pub use injected::InjectedKey;
use itertools::Itertools;
pub use key::Key;
pub use opaque::OpaqueValue;
pub use projection::DiceProjectionComputations;
pub use projection::ProjectionKey;
use thiserror::Error;

use crate::future_handle::WeakDiceFutureHandle;
use crate::incremental::evaluator::Evaluator;
use crate::incremental::graph::storage_properties::StorageProperties;
use crate::incremental::graph::GraphNode;
use crate::incremental::transaction_ctx::TransactionCtx;
use crate::incremental::IncrementalComputeProperties;
use crate::incremental::ValueWithDeps;
use crate::introspection::serialize_dense_graph;
use crate::introspection::serialize_graph;
use crate::key::StoragePropertiesForKey;
use crate::projection::ProjectionKeyProperties;

#[derive(Clone, Dupe, Debug, Error)]
#[error(transparent)]
pub struct DiceError(Arc<DiceErrorImpl>);

impl DiceError {
    pub fn cycle(
        trigger: Arc<dyn RequestedKey>,
        cyclic_keys: IndexSet<Arc<dyn RequestedKey>>,
    ) -> Self {
        DiceError(Arc::new(DiceErrorImpl::Cycle {
            trigger,
            cyclic_keys,
        }))
    }

    pub fn duplicate(key: Arc<dyn RequestedKey>) -> Self {
        DiceError(Arc::new(DiceErrorImpl::DuplicateChange(key)))
    }
}

#[derive(Debug, Error)]
enum DiceErrorImpl {
    #[error("Cyclic computation detect when computing key `{}`, which forms a cycle in computation chain: `{}`", trigger, cyclic_keys.iter().join(","))]
    Cycle {
        trigger: Arc<dyn RequestedKey>,
        cyclic_keys: IndexSet<Arc<dyn RequestedKey>>,
    },
    #[error("Key `{0}` was marked as changed multiple times on the same transaction.")]
    DuplicateChange(Arc<dyn RequestedKey>),
}

pub type DiceResult<T> = Result<T, DiceError>;

/// An incremental computation engine that executes arbitrary computations that
/// maps 'Key's to values.
pub struct Dice {
    data: DiceData,
    pub(crate) map: Arc<RwLock<DiceMap>>,
    global_versions: Arc<VersionTracker>,
    detect_cycles: DetectCycles,
}

impl Debug for Dice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Dice")
            .field("detect_cycles", &self.detect_cycles)
            .finish_non_exhaustive()
    }
}

impl Dice {
    pub fn builder() -> DiceDataBuilder {
        DiceDataBuilder::new()
    }

    fn new(data: DiceData, detect_cycles: DetectCycles) -> Arc<Self> {
        Arc::new(Dice {
            data,
            map: Arc::new(RwLock::new(DiceMap::new())),
            global_versions: VersionTracker::new(),
            detect_cycles,
        })
    }

    /// returns a new context for starting computations
    pub fn ctx(self: &Arc<Dice>) -> DiceTransaction {
        self.with_ctx_data(UserComputationData::new())
    }

    pub fn with_ctx_data(self: &Arc<Dice>, extra: UserComputationData) -> DiceTransaction {
        DiceTransaction(self.make_ctx(ComputationData::new(extra, self.detect_cycles)))
    }

    fn make_ctx(self: &Arc<Dice>, extra: ComputationData) -> DiceComputations {
        DiceComputations(Arc::new(DiceComputationImpl::new_transaction(
            self.dupe(),
            self.global_versions.current(),
            self.global_versions.write(),
            extra,
        )))
    }

    /// finds the computation index for the given key
    fn find_cache<K>(self: &Arc<Dice>) -> Arc<IncrementalEngine<StoragePropertiesForKey<K>>>
    where
        K: Key,
    {
        if let Some(cache) = self
            .map
            .read()
            .unwrap()
            .find_cache_opt::<StoragePropertiesForKey<K>>()
        {
            return cache;
        }

        self.map
            .write()
            .unwrap()
            .find_cache(|| IncrementalEngine::new(StoragePropertiesForKey::<K>::new(self)))
    }

    fn find_projection_cache<P: ProjectionKey>(
        self: &Arc<Dice>,
    ) -> Arc<IncrementalEngine<ProjectionKeyProperties<P>>>
    where
        P: ProjectionKey,
    {
        if let Some(cache) = self
            .map
            .read()
            .unwrap()
            .find_cache_opt::<ProjectionKeyProperties<P>>()
        {
            return cache;
        }

        self.map
            .write()
            .unwrap()
            .find_cache(|| IncrementalEngine::new(ProjectionKeyProperties::<P>::new(self)))
    }

    fn unstable_take(self: &Arc<Dice>) -> DiceMap {
        debug!(msg = "clearing all Dice state");
        let mut map = self.map.write().unwrap();
        std::mem::replace(&mut map, DiceMap::new())
    }

    pub fn serialize_tsv(&self, nodes: impl Write, edges: impl Write) -> anyhow::Result<()> {
        serialize_graph(&self.to_introspectable(), nodes, edges)
    }

    pub fn serialize_serde(&self, out: impl Write) -> anyhow::Result<()> {
        let mut writer = bincode::Serializer::new(
            out,
            bincode::config::DefaultOptions::new()
                .with_fixint_encoding()
                .allow_trailing_bytes(),
        );

        serialize_dense_graph(&self.to_introspectable(), &mut writer)?;

        Ok(())
    }
}

pub struct DiceDataBuilder(DiceData);

impl DiceDataBuilder {
    fn new() -> Self {
        Self(DiceData::new())
    }

    pub fn set<K: Send + Sync + 'static>(&mut self, val: K) {
        self.0.set(val);
    }

    pub fn build(self, detect_cycles: DetectCycles) -> Arc<Dice> {
        Dice::new(self.0, detect_cycles)
    }
}

#[derive(Clone, Dupe)]
struct Eval(Weak<Dice>);

#[async_trait]
impl<K: Key> IncrementalComputeProperties for StoragePropertiesForKey<K> {
    type DiceTask = WeakDiceFutureHandle<Self>;

    async fn recompute(
        key: &Self::Key,
        engine: &Arc<IncrementalEngine<Self>>,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
    ) -> DiceResult<GraphNode<StoragePropertiesForKey<K>>> {
        engine
            .eval_entry_versioned(key, transaction_ctx, extra.subrequest(key)?)
            .await
    }
}

#[async_trait]
impl<K: Key> Evaluator for StoragePropertiesForKey<K> {
    async fn eval(
        &self,
        k: &K,
        transaction_ctx: Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> ValueWithDeps<K::Value> {
        let ctx = DiceComputationImpl::from_transaction_ctx(
            self.dice
                .upgrade()
                .expect("Dice holds DiceMap so it should still be alive here"),
            transaction_ctx,
            extra,
        );

        let ctx = DiceComputations(ctx);

        let value = k.compute(&ctx).await;

        let both_deps = ctx.0.finalize();

        ValueWithDeps { value, both_deps }
    }
}

pub mod testing {
    use crate::ctx::DiceTransaction;
    use crate::ctx::UserComputationData;
    use crate::cycles::DetectCycles;
    use crate::Dice;
    use crate::DiceDataBuilder;
    use crate::Key;

    /// Testing utility that can be used to build a specific 'DiceComputation' where certain keys
    /// of computation mocked to return a specific result.
    ///
    /// TODO(bobyf): ideally, we want something where we don't have to use the specific keys
    /// but rather the computation function, like `mock.expect(|c| c.other_compute(4), "4 res")`
    pub struct DiceBuilder {
        builder: DiceDataBuilder,
        mocked: Vec<Box<dyn FnOnce(&DiceTransaction)>>,
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

        /// mocks the call of compute for the key 'expected_k' so that it returns 'expected_res'
        pub fn mock_and_return<K>(mut self, expected_k: K, expected_res: K::Value) -> Self
        where
            K: Key,
        {
            self.mocked
                .push(box move |ctx| ctx.changed_to(vec![(expected_k, expected_res)]));
            self
        }

        pub fn build(self, extra: UserComputationData) -> DiceTransaction {
            let dice = self.builder.build(DetectCycles::Enabled);
            let ctx = dice.with_ctx_data(extra);

            self.mocked.into_iter().for_each(|f| f(&ctx));
            ctx.commit()
        }
    }
}
