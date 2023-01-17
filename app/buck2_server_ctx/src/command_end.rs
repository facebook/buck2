/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

/// Common code executed in the end of command to produce `CommandEnd`.
pub fn command_end<R, D>(
    metadata: HashMap<String, String>,
    result: &anyhow::Result<R>,
    data: D,
) -> buck2_data::CommandEnd
where
    D: Into<buck2_data::command_end::Data>,
{
    command_end_ext(metadata, result, data.into(), |_| true)
}

pub fn command_end_ext<R, D, F>(
    metadata: HashMap<String, String>,
    result: &anyhow::Result<R>,
    data: D,
    is_success: F,
) -> buck2_data::CommandEnd
where
    F: FnOnce(&R) -> bool,
    D: Into<buck2_data::command_end::Data>,
{
    let (is_success, error_messages) = match result {
        Ok(r) => (is_success(r), Vec::new()),
        Err(e) => (false, vec![format!("{:#}", e)]),
    };
    buck2_data::CommandEnd {
        is_success,
        error_messages,
        metadata,
        data: Some(data.into()),
    }
}
