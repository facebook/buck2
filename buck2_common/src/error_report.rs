/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::result::recursive_shared_downcast_ref;
use crate::result::MayProvideAnyhowError;

pub trait CreateErrorReport {
    fn create_error_report(&self) -> Option<buck2_data::ErrorReport>;
}

impl<T> CreateErrorReport for T
where
    T: MayProvideAnyhowError,
{
    fn create_error_report(&self) -> Option<buck2_data::ErrorReport> {
        let err = self.as_anyhow()?;

        // Infra error by default if no category tag is set
        let category = Some(
            recursive_shared_downcast_ref::<buck2_data::ErrorCategory>(err)
                .map_or(buck2_data::ErrorCategory::Infra as i32, |c| *c as i32),
        );
        let cause = recursive_shared_downcast_ref::<buck2_data::ErrorCause>(err).map(|c| *c as i32);
        let error_message = format!("{:#}", err);

        Some(buck2_data::ErrorReport {
            category,
            cause,
            error_message,
        })
    }
}
