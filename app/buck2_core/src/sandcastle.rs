/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::buck2_env;

/// Are we running on sandcastle?
pub fn is_sandcastle() -> anyhow::Result<bool> {
    Ok(buck2_env!("SANDCASTLE")?.is_some())
}

pub fn sandcastle_id() -> anyhow::Result<Option<&'static str>> {
    buck2_env!("SANDCASTLE_ID")
}
