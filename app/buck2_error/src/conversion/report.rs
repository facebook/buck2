/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_data::ErrorReport;

use crate::source_location::SourceLocation;
use crate::ErrorTag;

impl From<ErrorReport> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: ErrorReport) -> Self {
        // TODO(ctolliday) convert remaining fields
        // (note: not using tags() as a workaround for likely false positive ASAN failure)
        let tags: Vec<ErrorTag> = value
            .tags
            .into_iter()
            .filter_map(|v| ErrorTag::try_from(v).ok())
            .collect();
        crate::Error::new(
            value.message.clone(),
            *tags.first().unwrap_or(&ErrorTag::Tier0),
            SourceLocation::new(file!()),
            None,
        )
        .tag(tags)
    }
}

impl From<&crate::Error> for ErrorReport {
    fn from(err: &crate::Error) -> Self {
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
            tags: err.tags().iter().map(|t| *t as i32).collect(),
            sub_error_categories,
            category_key: Some(category_key),
        }
    }
}
