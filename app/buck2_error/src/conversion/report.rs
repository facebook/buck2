/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_data::ErrorReport;

use crate::context_value::ContextValue;
use crate::context_value::StringTag;
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

        let mut error = crate::Error::new(
            value.message.clone(),
            *tags.first().unwrap_or(&ErrorTag::InvalidErrorReport),
            value
                .source_location
                .map(|s| s.into())
                .unwrap_or(SourceLocation::new(file!())),
            None,
        )
        .tag(tags);

        for tag in value.string_tags {
            let tag = ContextValue::StringTag(StringTag { tag: tag.tag });
            error = error.context(tag);
        }
        error
    }
}

impl From<&crate::Error> for ErrorReport {
    fn from(err: &crate::Error) -> Self {
        let (message, telemetry_message) = if let Some(f) = err.is_emitted() {
            (format!("{:?}", f), Some(format!("{:?}", err)))
        } else {
            (format!("{:?}", err), None)
        };

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
        let string_tags = err
            .iter_context()
            .filter_map(|kind| match kind {
                ContextValue::StringTag(val) => Some(buck2_data::error_report::StringTag {
                    tag: val.tag.clone(),
                }),
                _ => None,
            })
            .collect();

        buck2_data::ErrorReport {
            message,
            telemetry_message,
            source_location: Some(err.source_location().clone().into()),
            tags: err.tags().iter().map(|t| *t as i32).collect(),
            string_tags,
            sub_error_categories,
            category_key: Some(category_key),
        }
    }
}
