/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_test_api::data::TestStatus;

#[derive(Default)]
pub struct TestState {
    pub discovered: u64,
    pub pass: u64,
    pub fail: u64,
    pub fatal: u64,
    pub timeout: u64,
    pub skipped: u64,
    pub omitted: u64,
    pub retry: u64,
    pub unknown: u64,
    pub listing_success: u64,
    pub listing_failed: u64,
}

impl TestState {
    pub(crate) fn update(&mut self, result: &buck2_data::TestResult) -> anyhow::Result<()> {
        let status = TestStatus::try_from(result.status)?;
        let counter = match status {
            TestStatus::PASS => &mut self.pass,
            TestStatus::FAIL => &mut self.fail,
            TestStatus::FATAL => &mut self.fatal,
            TestStatus::SKIP => &mut self.skipped,
            TestStatus::OMITTED => &mut self.omitted,
            TestStatus::TIMEOUT => &mut self.timeout,
            TestStatus::UNKNOWN => &mut self.unknown,
            TestStatus::RERUN => &mut self.retry,
            TestStatus::LISTING_SUCCESS => &mut self.listing_success,
            TestStatus::LISTING_FAILED => &mut self.listing_failed,
        };
        *counter += 1;

        Ok(())
    }

    pub fn not_executed(&self) -> u64 {
        self.skipped + self.omitted
    }
}
