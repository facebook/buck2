/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::RefCell;
use std::panic::AssertUnwindSafe;
use std::panic::catch_unwind;
use std::panic::resume_unwind;

thread_local! {
    pub(super) static MOCK_REVISION: RefCell<Option<String>> =
        const { RefCell::new(None) };
}

pub(super) fn with_test_revision<T>(rev: Option<String>, f: impl FnOnce() -> T) -> T {
    MOCK_REVISION.with(|r| *r.borrow_mut() = rev);
    let result = catch_unwind(AssertUnwindSafe(f));
    MOCK_REVISION.with(|r| *r.borrow_mut() = None);
    match result {
        Ok(v) => v,
        Err(e) => resume_unwind(e),
    }
}
