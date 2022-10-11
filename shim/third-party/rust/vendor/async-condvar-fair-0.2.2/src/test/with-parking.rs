// Copyright Ian Jackson and contributors
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*;

// We have to say super::parking_lot because there's also a crate
// parking_lot, so just `use parking_lot::...` is ambiguous.
pub use super::parking_lot::Mutex      as TestMutex;
pub use super::parking_lot::MutexGuard as TestMutexGuard;
pub use super::SendTestFuture   as TestFuture;
pub use crate::define_test_with_parking as define_test;
pub use cases::guard_for_wait::for_wait;
lock_async!{ .lock() }
#[path="cases.rs"] pub mod cases;
