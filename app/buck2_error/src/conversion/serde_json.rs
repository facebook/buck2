/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use serde_json::error::Category;

use crate::conversion::stds::io::io_error_kind_to_error_tag;

impl From<serde_json::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: serde_json::Error) -> Self {
        let error_tag = match value.classify() {
            Category::Data | Category::Syntax => crate::ErrorTag::Input,
            Category::Eof => crate::ErrorTag::Tier0,
            Category::Io => match value.io_error_kind() {
                Some(error_kind) => io_error_kind_to_error_tag(error_kind),
                None => crate::ErrorTag::Tier0,
            },
        };

        crate::conversion::from_any_with_tag(value, error_tag)
    }
}
