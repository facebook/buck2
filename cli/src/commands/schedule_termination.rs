/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::thread;
use std::time::Duration;

use buck2_core::env_helper::EnvHelper;

/// Our tests sometimes don't exit Buck 2 cleanly, and they might not get an oppportunity to do so
/// if they are terminated. This allows the daemon to self-destruct.
pub(crate) fn maybe_schedule_termination() -> anyhow::Result<()> {
    static TERMINATE_AFTER: EnvHelper<u64> = EnvHelper::new("BUCK2_TERMINATE_AFTER");

    if let Some(duration) = TERMINATE_AFTER.get_copied()? {
        thread::Builder::new()
            .name("buck2-terminate-after".to_owned())
            .spawn(move || {
                thread::sleep(Duration::from_secs(duration));
                panic!("Buck is exiting after {}s elapsed", duration);
            })?;
    }

    Ok(())
}
