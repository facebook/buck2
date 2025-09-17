/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![allow(dead_code)] // This mod will be used in the next diffs.

/// Trait for fetching revisions for a given bookmark
#[async_trait::async_trait]
pub(crate) trait BookmarkRevisionFetcher: Sync + Send {
    /// Fetches the revisions for a given bookmark in the recent window
    async fn get_recent_revisions_for_bookmark(
        &self,
        bookmark: &str,
        lookback_hours: u64,
    ) -> buck2_error::Result<Option<Vec<String>>>;
}
