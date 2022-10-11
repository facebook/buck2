// Copyright Ian Jackson and contributors
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*;

pub use tokio::sync::Mutex      as TestMutex;
pub use tokio::sync::MutexGuard as TestMutexGuard;
pub use super::SendTestFuture   as TestFuture;
pub use crate::define_test_with_tokio as define_test;
pub use cases::pair_for_wait::for_wait;
lock_async!{ .lock().await }
#[path="cases.rs"] pub mod cases;

#[macro_export]
macro_rules! define_test_with_tokio {
  { $case:ident, $is_short:expr, $tasks:expr } => { paste! {
    #[tokio::test]
    async fn [< $case _tokio >](){
      use [< $case _tokio_generic >] as call;
      select_debug!(true, call, .await)
    }

    async fn [< $case _tokio_generic >]<D:DebugWrite>(mut d: D) {
      let tasks: Vec<TasksMaker<_>> = $tasks;
      for tasks in tasks.into_iter() {
        futures_util::future::join_all(tasks.1(&mut d)).await;
      }
    }
  } };
}
