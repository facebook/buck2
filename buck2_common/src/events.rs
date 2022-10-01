/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::env_helper::EnvHelper;

pub fn scribe_category() -> anyhow::Result<String> {
    const DEFAULT_SCRIBE_CATEGORY: &str = "buck2_events";
    // Note that both daemon and client are emitting events, and that changing this variable has
    // no effect on the daemon until buckd is restarted but has effect on the client.
    static SCRIBE_CATEGORY: EnvHelper<String> = EnvHelper::new("BUCK2_SCRIBE_CATEGORY");
    Ok(SCRIBE_CATEGORY
        .get()?
        .map_or_else(|| DEFAULT_SCRIBE_CATEGORY.to_owned(), |c| c.clone()))
}
