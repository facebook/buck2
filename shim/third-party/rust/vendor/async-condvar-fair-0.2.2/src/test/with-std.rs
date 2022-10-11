// Copyright Ian Jackson and contributors
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*;

pub use std::sync::Mutex        as TestMutex;
pub use std::sync::MutexGuard   as TestMutexGuard;
pub use super::NoSendTestFuture as TestFuture;
pub use crate::define_test_with_parking as define_test;
pub use cases::pair_for_wait::for_wait;
lock_async!{ .lock().unwrap() }
#[path="cases.rs"] pub mod cases;

#[macro_export]
macro_rules! define_test_with_std {
  { $case:ident, $is_short:expr, $tasks:expr } => { paste! {
    #[test]
    fn [< $case _smol >](){
      use [< $case _std_generic >] as call;
      select_debug!(true, call,)
    }

    fn [< $case _std_generic >]<D:DebugWrite>(mut d: D) {
      let tasks: Vec<TasksMaker<_>> = $tasks;
      for tasks in tasks.into_iter() {
        let _: Vec<()> = smol::block_on(
          futures_util::future::join_all(tasks.1(&mut d))
        );
      }
    }
  } };
}
