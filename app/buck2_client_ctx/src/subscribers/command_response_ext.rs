/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::TestResponse;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = TestExecutor)]
enum TestError {
    #[error("Test execution completed but the tests failed")]
    #[buck2(tag = Input)]
    TestFailed,
    #[error("Test execution completed but tests were skipped")]
    #[buck2(tag = Input)]
    TestSkipped,
    #[error("Test listing failed")]
    #[buck2(tag = Input)]
    ListingFailed,
    #[error("Fatal error encountered during test execution")]
    Fatal,
}

pub(crate) trait TestResponseExt {
    fn error_report_for_test_errors(&self) -> Vec<buck2_data::ErrorReport>;
}

impl TestResponseExt for TestResponse {
    fn error_report_for_test_errors(&self) -> Vec<buck2_data::ErrorReport> {
        let mut errors = self.errors.clone();

        if let Some(status) = &self.test_statuses {
            if let Some(failed) = &status.failed {
                if failed.count > 0 {
                    errors.push(buck2_data::ErrorReport::from(&TestError::TestFailed.into()));
                }
            }
            if let Some(fatal) = &status.fatals {
                if fatal.count > 0 {
                    errors.push(buck2_data::ErrorReport::from(&TestError::Fatal.into()));
                }
            }
            if let Some(listing_failed) = &status.listing_failed {
                if listing_failed.count > 0 {
                    errors.push(buck2_data::ErrorReport::from(
                        &TestError::ListingFailed.into(),
                    ));
                }
            }
            // If a test was skipped due to condition not being met a non-zero exit code will be returned,
            // this doesn't seem quite right, but for now just tag it with TestSkipped to track occurrence.
            if let Some(skipped) = &status.skipped {
                if skipped.count > 0 && self.exit_code.is_none_or(|code| code != 0) {
                    errors.push(buck2_data::ErrorReport::from(
                        &TestError::TestSkipped.into(),
                    ));
                }
            }

            if let Some(code) = self.exit_code {
                if errors.is_empty() && code != 0 {
                    errors.push(buck2_data::ErrorReport::from(&buck2_error::buck2_error!(
                        buck2_error::ErrorTag::TestExecutor,
                        "Test Executor Failed with exit code {code}"
                    )))
                }
            }
        }

        errors
    }
}
