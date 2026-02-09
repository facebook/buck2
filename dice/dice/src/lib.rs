/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
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
//! use dice_futures::cancellation::CancellationContext;
//!
//!     /// A configuration computation that consists of values that are pre-computed outside of DICE
//!     pub struct InjectConfigs<'compute, 'd>(&'compute mut DiceComputations<'d>);
//!
//!     impl<'compute, 'd> InjectConfigs<'compute, 'd> {
//!         pub async fn get_config(&mut self) -> usize {
//!             self.0.compute(&ConfigKey).await.unwrap()
//!         }
//!     }
//!
//!     #[derive(Clone, Debug, Display, Eq, Hash, PartialEq, Allocative)]
//!     #[display("{:?}", self)]
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
//!     pub struct MyComputation<'compute, 'd>(pub &'compute mut DiceComputations<'d>);
//!
//!     impl<'compute, 'd> MyComputation<'compute, 'd> {
//!         // declaring a computation function
//!         pub async fn compute_a(&mut self, a: usize, s: String) -> Arc<String> {
//!             #[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
//!             #[display("{:?}", self)]
//!             struct ComputeA(usize, String);
//!
//!             #[async_trait]
//!             impl Key for ComputeA {
//!                 type Value = Arc<String>;
//!
//!                 async fn compute(&self, ctx: &mut DiceComputations, _cancellations: &CancellationContext) -> Self::Value {
//!                     // request for other computations on the self
//!                     let n = MyComputation(ctx).compute_b(self.0).await;
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
//!         pub async fn compute_b(&mut self, a: usize) -> usize {
//!                 self.0.compute(&ComputeB(a)).await.unwrap()
//!         }
//!     }
//!
//!     #[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
//!     #[display("{:?}", self)]
//!     struct ComputeB(usize);
//!
//!     #[async_trait]
//!     impl Key for ComputeB {
//!         type Value = usize;
//!
//!         async fn compute(&self, ctx: &mut DiceComputations, cancellations: &CancellationContext) -> Self::Value {
//!             self.0 + InjectConfigs(ctx).get_config().await + ctx.global_data().static_data().len()
//!         }
//!
//!         fn equality(x: &Self::Value,y: &Self::Value) -> bool {
//!             x == y
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
//! let mut ctx = rt.block_on(ctx.commit());
//!
//! // request the computation from DICE
//! rt.block_on(async {
//!     assert_eq!("aaaaaaaa", &*MyComputation(&mut ctx).compute_a(4, "a".into()).await);
//! });
//!
//! let mut ctx = engine.updater();
//! ctx.inject_config(2);
//!
//! let mut ctx = rt.block_on(ctx.commit());
//!
//! // request the computation from DICE
//! rt.block_on(async {
//!     assert_eq!("aaaaaaaaaa", &*MyComputation(&mut ctx).compute_a(4, "a".into()).await);
//! });
//! ```

#![feature(fn_traits)]
#![feature(test)]
#![feature(map_try_insert)]
// This sometimes flag false positives where proc-macros expand pass by value into pass by refs
#![allow(clippy::trivially_copy_pass_by_ref)]

#[macro_use]
extern crate gazebo;

#[macro_use]
extern crate tracing;

mod api;
pub(crate) mod arc;
mod ctx;
pub(crate) mod future;
mod impls;
pub mod introspection;
mod legacy;
pub(crate) mod metrics;
pub(crate) mod owned;
pub(crate) mod stats;
mod transaction;
mod transaction_update;
mod versions;

pub use dice_futures::cancellation::CancellationContext; // expose cancellation context as api
pub use dice_futures::cancellation::CancellationHandle; // expose cancellation handle as api
pub use dice_futures::spawn::CancellableJoinHandle; // expose cancellation context as api
pub use dice_futures::spawn::WeakFutureError; // expose future errors as api
pub(crate) type HashMap<K, V> = std::collections::HashMap<K, V, fxhash::FxBuildHasher>;
pub(crate) type HashSet<K> = std::collections::HashSet<K, fxhash::FxBuildHasher>;

pub use crate::api::activation_tracker::ActivationData;
pub use crate::api::activation_tracker::ActivationTracker;
pub use crate::api::computations::DiceComputations;
pub use crate::api::computations::DiceComputationsData;
pub use crate::api::computations::LinearRecomputeDiceComputations;
pub use crate::api::cycles::DetectCycles;
pub use crate::api::data::DiceData;
pub use crate::api::demand::Demand;
pub use crate::api::dyn_key::DynKey;
pub use crate::api::events::DiceEvent;
pub use crate::api::events::DiceEventListener;
pub use crate::api::injected::InjectedKey;
pub use crate::api::invalidation_tracking::DiceInvalidationPath;
pub use crate::api::invalidation_tracking::DiceKeyTrackedInvalidationPaths;
pub use crate::api::invalidation_tracking::DiceTrackedInvalidationPath;
pub use crate::api::invalidation_tracking::InvalidationPathEntry;
pub use crate::api::key::InvalidationSourcePriority;
pub use crate::api::key::Key;
pub use crate::api::opaque::OpaqueValue;
pub use crate::api::projection::DiceProjectionComputations;
pub use crate::api::projection::ProjectionKey;
pub use crate::api::transaction::DiceEquality;
pub use crate::api::transaction::DiceTransaction;
pub use crate::api::transaction::DiceTransactionUpdater;
pub use crate::api::user_data::UserComputationData;
pub use crate::api::user_data::UserCycleDetector;
pub use crate::api::user_data::UserCycleDetectorGuard;
pub use crate::impls::dice::Dice;
pub use crate::impls::dice::DiceDataBuilder;
pub use crate::introspection::serialize_dense_graph;
pub use crate::introspection::serialize_graph;
pub use crate::stats::GlobalStats;
use crate::transaction_update::DiceTransactionUpdaterImpl;

pub mod testing {
    pub use crate::api::dice::testing::DiceBuilder;
}
