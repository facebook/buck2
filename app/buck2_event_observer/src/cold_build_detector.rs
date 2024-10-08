/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::build_count::BuildCountManager;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_data::FileWatcherEnd;
use buck2_data::ParsedTargetPatterns;
use buck2_error::internal_error;

/// Detects if this is the first build since a rebase.
/// The state is relevant per command since the detector is recreated for each command.
pub struct ColdBuildDetector {
    build_count_manager: BuildCountManager,
    merge_base: Option<String>,
    first_build_since_rebase: Option<bool>,
    target_patterns: Option<ParsedTargetPatterns>,
}

impl ColdBuildDetector {
    pub fn new(build_count_dir: AbsNormPathBuf) -> Self {
        Self {
            build_count_manager: BuildCountManager::new(build_count_dir),
            merge_base: None,
            first_build_since_rebase: None,
            target_patterns: None,
        }
    }

    pub fn first_build_since_rebase(&self) -> Option<bool> {
        self.first_build_since_rebase
    }

    pub async fn update_merge_base(&mut self, file_watcher: &FileWatcherEnd) -> anyhow::Result<()> {
        // There could be multiple file watcher events with only some parts of the data filled in.
        // However, there should be only one merge base per command.
        if let Some(merge_base) = file_watcher
            .stats
            .as_ref()
            .and_then(|stats| stats.branched_from_revision.as_ref())
        {
            if let Some(mb) = &self.merge_base {
                if mb != merge_base {
                    return Err(internal_error!(
                        "Merge base changed during command execution"
                    ));
                }
            } else {
                self.merge_base = Some(merge_base.clone());
                self.try_compute_first_build_since_rebase().await?;
            }
        }
        Ok(())
    }

    pub async fn update_parsed_target_patterns(
        &mut self,
        patterns: &ParsedTargetPatterns,
    ) -> anyhow::Result<()> {
        if self.target_patterns.is_some() {
            return Err(internal_error!(
                "Parsed target patterns should be updated only once per command"
            ));
        }
        self.target_patterns = Some(patterns.clone());
        self.try_compute_first_build_since_rebase().await?;
        Ok(())
    }

    async fn try_compute_first_build_since_rebase(&mut self) -> anyhow::Result<()> {
        if self.first_build_since_rebase.is_some() {
            // This value should be valid for the lifetime of the detector.
            return Ok(());
        }

        // Compute first_build_since_rebase only if both `merge base` and `target patterns` are available.
        if let (Some(merge_base), Some(patterns)) = (&self.merge_base, &self.target_patterns) {
            let build_count = self
                .build_count_manager
                .min_count(merge_base, patterns)
                .await?;
            self.first_build_since_rebase = Some(build_count.successful_build_count < 1);
        }
        Ok(())
    }
}
