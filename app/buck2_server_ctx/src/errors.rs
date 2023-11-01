/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub fn create_error_report(err: &buck2_error::Error) -> buck2_data::ErrorReport {
    // Infra error by default if no category tag is set
    let category = err.get_category().map(|c| match c {
        buck2_error::Category::User => buck2_data::error::ErrorCategory::UserDeprecated,
        buck2_error::Category::Infra => buck2_data::error::ErrorCategory::Infra,
    });
    let typ = err.get_error_type().map(|c| c as i32);

    let (message, telemetry_message) = if let Some(f) = err.get_late_format() && err.is_emitted() {
        (format!("{:?}", f), Some(format!("{:?}", err)))
    } else {
        (format!("{:?}", err), None)
    };

    buck2_data::ErrorReport {
        category: category.map(|c| c as i32),
        typ,
        message,
        telemetry_message,
        source_location: None,
    }
}
