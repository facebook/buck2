// Copyright Ian Jackson and contributors
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*;

pub use smol::lock::Mutex      as TestMutex;
pub use smol::lock::MutexGuard as TestMutexGuard;
pub use super::SendTestFuture  as TestFuture;
pub use crate::define_test_with_smol as define_test;
pub use cases::guard_for_wait::for_wait;
lock_async!{ .lock().await }
#[path="cases.rs"] pub mod cases;

#[macro_export]
macro_rules! define_test_with_smol {
  { $case:ident, $is_short:expr, $tasks:expr } => { paste! {
    #[test]
    fn [< $case _smol >](){
      use [< $case _smol_generic >] as call;
      select_debug!(true, call,)
    }

    fn [< $case _smol_generic >]<D:DebugWrite>(mut d: D) {
      let tasks: Vec<TasksMaker<_>> = $tasks;
      for tasks in tasks.into_iter() {
        let _: Vec<()> = smol::block_on(
          futures_util::future::join_all(tasks.1(&mut d))
        );
      }
    }
  } };
}
