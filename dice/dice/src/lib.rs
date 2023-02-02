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
//!             #[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
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
//!     pub trait SetInjectedConfig {
//!         fn inject_config(&self, i: usize);
//!     }
//!
//!     impl SetInjectedConfig for DiceTransactionUpdater {
//!         /// ways to inject the precomputed values to dice
//!         fn inject_config(&self, i: usize) {
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
//! let ctx = engine.updater();
//! ctx.inject_config(0);
//!
//! let ctx = ctx.commit();
//!
//! // request the computation from DICE
//! rt.block_on(async {
//!     assert_eq!("aaaaaaaa", &*ctx.my_computation().compute_a(4, "a".into()).await);
//! });
//!
//! let ctx = engine.updater();
//! ctx.inject_config(2);
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
#![feature(map_try_insert)]
#![feature(map_entry_replace)]
// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]
// This sometimes flag false positives where proc-macros expand pass by value into pass by refs
#![allow(clippy::trivially_copy_pass_by_ref)]

#[macro_use]
extern crate gazebo;

#[macro_use]
extern crate tracing;

pub mod cycles;
mod dice_future;
mod dice_task;
mod future_handle;
mod incremental;
pub mod introspection;
mod key;
mod legacy;
mod map;
pub(crate) mod metrics;
pub(crate) mod opaque;
pub(crate) mod projection;
mod sync_handle;

#[cfg(test)]
mod tests;

// ctx contains pub data that we don't want to expose, so we hide the whole mod but expose just the
// data we want to expose
mod ctx;

mod api;

use std::fmt::Debug;
use std::io::Write;
use std::sync::Arc;

use allocative::Allocative;
pub use fnv::FnvHashMap as HashMap;
pub use fnv::FnvHashSet as HashSet;
use futures::future::Future;
use serde::Serializer;

pub use crate::api::computations::DiceComputations;
pub use crate::api::data::DiceData;
pub use crate::api::dice::Dice;
pub use crate::api::dice::DiceDataBuilder;
pub use crate::api::error::DiceError;
pub use crate::api::error::DiceResult;
pub use crate::api::events::DiceEvent;
pub use crate::api::events::DiceEventListener;
pub use crate::api::injected::InjectedKey;
pub use crate::api::key::Key;
pub use crate::api::transaction::DiceTransaction;
pub use crate::api::transaction::DiceTransactionUpdater;
pub use crate::api::user_data::UserComputationData;
use crate::cycles::DetectCycles;
use crate::future_handle::WeakDiceFutureHandle;
use crate::incremental::graph::GraphNode;
use crate::incremental::transaction_ctx::TransactionCtx;
pub use crate::incremental::versions::VersionNumber;
use crate::incremental::ValueWithDeps;
use crate::key::StoragePropertiesForKey;
use crate::legacy::DiceLegacy;
use crate::metrics::Metrics;
pub use crate::opaque::OpaqueValue;
pub use crate::projection::DiceProjectionComputations;
pub use crate::projection::ProjectionKey;

#[derive(Allocative, Debug)]
pub(crate) enum DiceImplementation {
    Legacy(Arc<DiceLegacy>),
}

impl DiceImplementation {
    pub fn updater(&self) -> DiceTransactionUpdater {
        match self {
            DiceImplementation::Legacy(dice) => dice.updater(),
        }
    }

    pub fn updater_with_data(&self, extra: UserComputationData) -> DiceTransactionUpdater {
        match self {
            DiceImplementation::Legacy(dice) => dice.updater_with_data(extra),
        }
    }

    pub fn serialize_tsv(
        &self,
        nodes: impl Write,
        edges: impl Write,
        nodes_currently_running: impl Write,
    ) -> anyhow::Result<()> {
        match self {
            DiceImplementation::Legacy(dice) => {
                dice.serialize_tsv(nodes, edges, nodes_currently_running)
            }
        }
    }

    pub fn serialize_serde<S>(&self, serializer: S) -> Result<(), S::Error>
    where
        S: Serializer,
    {
        match self {
            DiceImplementation::Legacy(dice) => dice.serialize_serde(serializer),
        }
    }

    pub fn detect_cycles(&self) -> &DetectCycles {
        match self {
            DiceImplementation::Legacy(dice) => dice.detect_cycles(),
        }
    }

    pub fn metrics(&self) -> Metrics {
        match self {
            DiceImplementation::Legacy(dice) => dice.metrics(),
        }
    }

    /// Wait until all active versions have exited.
    pub fn wait_for_idle(&self) -> impl Future<Output = ()> + 'static {
        match self {
            DiceImplementation::Legacy(dice) => dice.wait_for_idle(),
        }
    }
}

pub mod testing {
    pub use crate::api::dice::testing::DiceBuilder;
}
