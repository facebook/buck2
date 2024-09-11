/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(target_os = "macos")]

use std::sync::OnceLock;

use anyhow::Context;

#[allow(clippy::absurd_extreme_comparisons)]
pub fn sc_clk_tck() -> anyhow::Result<u32> {
    static TICKS: OnceLock<u32> = OnceLock::new();
    TICKS
        .get_or_try_init(|| {
            unsafe {
                let rate = libc::sysconf(libc::_SC_CLK_TCK);
                let rate: u32 = rate
                    .try_into()
                    .context("Integer overflow converting ticks per second")?;
                // Practically it is always 100. But we have to check it.
                if rate <= 0 || rate > 10_000 {
                    return Err(anyhow::anyhow!("Invalid ticks per second: {}", rate));
                }
                Ok(rate)
            }
        })
        .copied()
}

#[cfg(test)]
mod tests {
    use crate::macos::sc_clk_tck::sc_clk_tck;

    #[test]
    fn test_ticks_per_second() {
        // It is always 100.
        assert_eq!(100, sc_clk_tck().unwrap());
    }
}
