/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(test)]
#![cfg(target_os = "linux")]
#![feature(exit_status_error)]

use std::process::Command;

use anyhow::Context;
use buck2_miniperf_proto::MiniperfOutput;

#[test]
fn test_miniperf() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let path = temp_dir.path().join("out");

    let mut cmd = Command::new(std::env::var_os("MINIPERF").context("$MINIPERF is not set")?);
    cmd.arg(&path);
    cmd.arg(
        std::env::var_os("THREE_BILLION_INSTRUCTIONS")
            .context("$THREE_BILLION_INSTRUCTIONS is not set")?,
    );
    cmd.status()?.exit_ok()?;

    let out = std::fs::read(&path).context("Failed to read")?;
    let out = bincode::deserialize::<MiniperfOutput>(&out).context("Failed to deserialize")?;

    // Check that we're within 5%
    assert!(
        out.counters
            .as_ref()
            .unwrap()
            .user_instructions
            .adjusted_count()
            > 2850000000
    );
    assert!(
        out.counters
            .as_ref()
            .unwrap()
            .user_instructions
            .adjusted_count()
            < 3150000000
    );

    Ok(())
}
