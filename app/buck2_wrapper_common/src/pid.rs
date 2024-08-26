/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::num::NonZeroU32;

use anyhow::Context;
use dupe::Dupe;

/// Process id.
#[derive(Debug, Clone, Copy, Dupe, derive_more::Display)]
#[display("{}", pid)]
pub struct Pid {
    pid: NonZeroU32,
}

impl Pid {
    pub fn from_u32(pid: u32) -> anyhow::Result<Self> {
        Ok(Pid {
            pid: NonZeroU32::new(pid).context("pid must be non-zero")?,
        })
    }

    pub fn from_i64(pid: i64) -> anyhow::Result<Self> {
        Self::from_u32(
            pid.try_into()
                .context("integer overflow converting pid to u32")?,
        )
    }

    pub fn to_u32(self) -> u32 {
        self.pid.get()
    }

    #[cfg(unix)]
    pub fn to_nix(self) -> anyhow::Result<nix::unistd::Pid> {
        Ok(nix::unistd::Pid::from_raw(
            self.pid
                .get()
                .try_into()
                .context("Integer overflow converting pid to pid_t")?,
        ))
    }
}
