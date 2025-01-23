/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use gazebo::prelude::SliceExt;

pub fn create_error_report(err: &buck2_error::Error) -> buck2_data::ErrorReport {
    let (message, telemetry_message) = if let Some(f) = err.is_emitted() {
        (format!("{:?}", f), Some(format!("{:?}", err)))
    } else {
        (format!("{:?}", err), None)
    };

    let source_location = Some(err.source_location().to_string());
    let category_key = err.category_key();

    let sub_error_categories = if let Some(error_diagnostics) = err
        .action_error()
        .and_then(|e| e.error_diagnostics.as_ref())
    {
        if let Some(buck2_data::action_error_diagnostics::Data::SubErrors(sub_errors)) =
            &error_diagnostics.data
        {
            sub_errors
                .sub_errors
                .iter()
                .map(|s| s.category.clone())
                .collect::<Vec<_>>()
        } else {
            vec![]
        }
    } else {
        vec![]
    };

    buck2_data::ErrorReport {
        message,
        telemetry_message,
        source_location,
        tags: err.tags().map(|t| *t as i32),
        sub_error_categories,
        category_key: Some(category_key),
    }
}
