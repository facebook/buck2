/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_fs::IoResultExt;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::working_dir::AbsWorkingDir;

use super::path_sanitizer::PathSanitizer;
use super::path_sanitizer::SanitizedPath;
use super::results::CompletionResults;

pub(crate) struct PathCompleter<'a, 'b> {
    cwd: AbsWorkingDir,
    sanitizer: &'b PathSanitizer,
    results: &'b mut CompletionResults<'a>,
}

impl<'a, 'b> PathCompleter<'a, 'b> {
    pub(crate) fn new(
        cwd: &AbsWorkingDir,
        sanitizer: &'b PathSanitizer,
        results: &'b mut CompletionResults<'a>,
    ) -> buck2_error::Result<Self> {
        Ok(Self {
            cwd: cwd.to_owned(),
            sanitizer,
            results,
        })
    }

    pub(crate) async fn complete(mut self, given_path: &str) -> CommandOutcome<()> {
        let path = self.sanitizer.sanitize(given_path)?;
        if path.given() != given_path && self.completes_to_dir(&path)? {
            // There are potential completions to this string, but we're
            // correcting it on the first tab to minimize surprise and help
            // the user learn to type paths the right way.
            //
            // The completes_to_dir() check guards against complete_dir_fragment()
            // completing "cell//" -> "cell1//", "root//cell/"
            self.results.insert(path.given());
        } else if path.is_ready_for_next_dir() {
            self.complete_subdirs(&path).await?;
        } else {
            self.complete_dir_fragment(&path).await?;
        }
        CommandOutcome::Success(())
    }

    async fn complete_subdirs(&mut self, partial: &SanitizedPath) -> CommandOutcome<()> {
        let partial_dir = partial.abs_path();

        let given_dir = partial.given();

        for entry in partial_dir.read_dir()?.flatten() {
            if entry.path().is_dir() {
                let path = self.sanitize(&(given_dir.to_owned() + &file_name_string(&entry)))?;
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

        let mut scan_dir = self.cwd.path().to_path_buf();
        if let Some(offset_dir) = partial_path.parent() {
            scan_dir = scan_dir.join(offset_dir);
        }
        let scan_dir = AbsNormPath::new(&scan_dir)?;
        let entries = fs_util::read_dir(scan_dir).categorize_input()?;
        for entry_result in entries {
            let entry = entry_result?;
            if entry.path().is_dir() && file_name_string(&entry).starts_with(partial_base) {
                let given_expanded =
                    self.sanitize(&(given_dir.to_owned() + &file_name_string(&entry)))?;
                self.results.insert_path(&given_expanded).await;
            }
        }
        CommandOutcome::Success(())
    }

    fn completes_to_dir(&self, partial: &SanitizedPath) -> buck2_error::Result<bool> {
        let partial_path = partial.abs_path();
        let partial_base = partial_path.file_name().unwrap().to_str().unwrap();

        let mut scan_dir = self.cwd.path().to_path_buf();
        if let Some(offset_dir) = partial_path.parent() {
            scan_dir = scan_dir.join(offset_dir);
        }

        let scan_dir = AbsNormPath::new(&scan_dir)?;
        let entries = fs_util::read_dir(scan_dir).categorize_input()?;
        for entry_result in entries {
            let entry = entry_result?;
            if entry.path().is_dir() && file_name_string(&entry).starts_with(partial_base) {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn sanitize(&self, given: &str) -> buck2_error::Result<SanitizedPath> {
        self.sanitizer.sanitize(given)
    }
}

fn file_name_string(entry: &std::fs::DirEntry) -> String {
    entry.file_name().into_string().unwrap()
}
