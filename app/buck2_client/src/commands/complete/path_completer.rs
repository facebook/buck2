/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::sync::Arc;

use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;

use super::path_sanitizer::SanitizedPath;
use super::results::CompletionResults;

pub(crate) struct PathCompleter<'a, 'b> {
    cell_configs: Arc<BuckConfigBasedCells>,

    cwd: AbsNormPathBuf,
    // sanitizer: &'b PathSanitizer,
    results: &'b mut CompletionResults<'a>,
}

impl<'a, 'b> PathCompleter<'a, 'b> {
    pub(crate) fn new(
        cell_configs: Arc<BuckConfigBasedCells>,

        cwd: &AbsNormPath,
        // sanitizer: &'b PathSanitizer,
        results: &'b mut CompletionResults<'a>,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            cell_configs,
            cwd: cwd.to_owned(),
            // sanitizer,
            results,
        })
    }

    pub(crate) async fn complete(&mut self, partial: &SanitizedPath) -> CommandOutcome<()> {
        if partial.is_full_dir() {
            self.complete_subdirs(partial).await
        } else {
            self.complete_dir_fragment(partial).await
        }
    }

    async fn complete_subdirs(&mut self, partial: &SanitizedPath) -> CommandOutcome<()> {
        let partial_dir = partial.abs_path();

        let given_dir = partial.given();

        for entry in partial_dir.read_dir()?.flatten() {
            if entry.path().is_dir() {
                let path = SanitizedPath::new(
                    &self.cell_configs.cell_resolver,
                    &self.cwd,
                    &(given_dir.to_owned() + &file_name_string(&entry)),
                )
                .await?;
                if path.cell_name() == partial.cell_name() {
                    self.results.insert_path(&path).await;
                }
            }
        }
        CommandOutcome::Success(())
    }

    async fn complete_dir_fragment(&mut self, partial: &SanitizedPath) -> CommandOutcome<()> {
        let partial_path = partial.abs_path();
        let partial_base = partial_path.file_name().unwrap().to_str().unwrap();

        let given_dir = &partial.given()[..partial.given().len() - partial_base.len()];

        let mut scan_dir = self.cwd.to_path_buf();
        if let Some(offset_dir) = partial_path.parent() {
            scan_dir = scan_dir.join(offset_dir);
        }

        for entry_result in scan_dir.read_dir()? {
            let entry = entry_result?;
            if entry.path().is_dir() && file_name_string(&entry).starts_with(partial_base) {
                let given_expanded = SanitizedPath::new(
                    &self.cell_configs.cell_resolver,
                    &self.cwd,
                    &(given_dir.to_owned() + &file_name_string(&entry)),
                )
                .await?;
                self.results.insert_path(&given_expanded).await;
            }
        }
        CommandOutcome::Success(())
    }

    pub(crate) fn completes_to_dir(cwd: &Path, partial: &SanitizedPath) -> anyhow::Result<bool> {
        let partial_path = partial.abs_path();
        let partial_base = partial_path.file_name().unwrap().to_str().unwrap();

        let mut scan_dir = cwd.to_path_buf();
        if let Some(offset_dir) = partial_path.parent() {
            scan_dir = scan_dir.join(offset_dir);
        }

        for entry_result in scan_dir.read_dir()? {
            let entry = entry_result?;
            if entry.path().is_dir() && file_name_string(&entry).starts_with(partial_base) {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

fn file_name_string(entry: &std::fs::DirEntry) -> String {
    entry.file_name().into_string().unwrap()
}
