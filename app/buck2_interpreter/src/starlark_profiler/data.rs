/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp;
use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use anyhow::Context;
use starlark::eval::ProfileData;
use starlark::StarlarkResultExt;

#[derive(Debug, Clone, Allocative)]
pub struct StarlarkProfileDataAndStats {
    #[allocative(skip)] // OK to skip because used only when profiling enabled.
    pub profile_data: ProfileData,
    pub(crate) initialized_at: Instant,
    pub(crate) finalized_at: Instant,
    pub(crate) total_retained_bytes: usize,
}

impl StarlarkProfileDataAndStats {
    pub fn elapsed(&self) -> Duration {
        self.finalized_at.duration_since(self.initialized_at)
    }

    pub fn total_retained_bytes(&self) -> usize {
        self.total_retained_bytes
    }

    pub fn merge<'a>(
        datas: impl IntoIterator<Item = &'a StarlarkProfileDataAndStats>,
    ) -> anyhow::Result<StarlarkProfileDataAndStats> {
        let datas = Vec::from_iter(datas);
        let mut iter = datas.iter().copied();
        let first = iter.next().context("empty collection of profile data")?;
        let mut total_retained_bytes = first.total_retained_bytes;
        let mut initialized_at = first.initialized_at;
        let mut finalized_at = first.finalized_at;

        for data in iter {
            initialized_at = cmp::min(initialized_at, data.initialized_at);
            finalized_at = cmp::max(finalized_at, data.finalized_at);
            total_retained_bytes += data.total_retained_bytes;
        }

        let profile_data =
            ProfileData::merge(datas.iter().map(|data| &data.profile_data)).into_anyhow_result()?;

        Ok(StarlarkProfileDataAndStats {
            profile_data,
            initialized_at,
            finalized_at,
            total_retained_bytes,
        })
    }
}
