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
    let category = match err.get_category().unwrap_or(buck2_error::Category::Infra) {
        buck2_error::Category::User => buck2_data::ErrorCategory::User,
        buck2_error::Category::Infra => buck2_data::ErrorCategory::Infra,
    };
    let cause = err
        .downcast_ref::<buck2_data::ErrorCause>()
        .map(|c| *c as i32);

    let (error_message, telemetry_error_message) = if let Some(f) = err.get_late_format() && err.is_emitted() {
        (format!("{:?}", f), Some(format!("{:?}", err)))
    } else {
        (format!("{:?}", err), None)
    };

    buck2_data::ErrorReport {
        category: Some(category as i32),
        cause,
        error_message,
        telemetry_error_message,
    }
}

/// Formats an error to be displayed at the end of the command invocation.
///
/// This function is aware of errors that may have been emitted earlier in the command execution.
pub fn late_format_error(e: &buck2_error::Error) -> Option<String> {
    if e.is_emitted() {
        Some(format!("{:?}", e.get_late_format()?))
    } else {
        Some(format!("{:?}", e))
    }
}
