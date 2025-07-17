/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Duration;

fn get() -> Option<u64> {
    // Only used in testing so unwrap is fine
    buck2_core::buck2_env!("BUCK2_SELF_TEST_TIMEOUT_S", type=u64, applicability=testing).unwrap()
}

/// If running in a self-test of buck2, returns a duration greater than the timeout of the test.
///
/// This should be used to ensure that buck2 properly cleans itself up in case the test does not
/// shut down cleanly.
pub fn until_post_test_shutdown() -> Option<Duration> {
    get().map(|s| Duration::from_secs(s + 30))
}

/// If running in a self-test of buck2, may adjust timeouts downward.
///
/// Many operations in buck2 either do not have timeouts or have timeouts that exceed the top-level
/// timeout for a test of buck2 itself. As a result, if these operations hang, it results in
/// hard-to-debug test hangs, instead of error messages indicating what is hanging. Passing an
/// appropriate timeout through this function mitigates that by ensuring fine-grained timeouts fire
/// before global timeouts.
pub fn maybe_cap_timeout(t: Option<Duration>) -> Option<Duration> {
    let fuse_timeout = get().map(|s| Duration::from_secs(s / 2));
    [t, fuse_timeout].into_iter().flatten().min()
}
