/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_data::Snapshot;
use buck2_error::conversion::from_any_with_tag;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;

use crate::humanized::HumanizedBytes;
use crate::humanized::HumanizedBytesPerSecond;
use crate::two_snapshots::TwoSnapshots;

pub struct ReState {
    session_id: Option<String>,
    first_snapshot: Option<buck2_data::Snapshot>,
}

impl ReState {
    pub fn new() -> Self {
        Self {
            session_id: None,
            first_snapshot: None,
        }
    }

    pub fn add_re_session(&mut self, session: &buck2_data::RemoteExecutionSessionCreated) {
        self.session_id = Some(session.session_id.clone());
    }

    pub fn update(&mut self, snapshot: &buck2_data::Snapshot) {
        if self.first_snapshot.is_none() {
            self.first_snapshot = Some(snapshot.clone());
        }
    }

    pub fn first_snapshot(&self) -> &Option<Snapshot> {
        &self.first_snapshot
    }

    pub fn render_header(
        &self,
        two_snapshots: &TwoSnapshots,
        draw_mode: DrawMode,
    ) -> Option<String> {
        let mut parts = Vec::new();

        if let (Some(first), Some((_, last))) = (&self.first_snapshot, &two_snapshots.last) {
            if last.re_upload_bytes > 0
                || last.re_download_bytes > 0
                || last.http_download_bytes > 0
            {
                let re_upload_bytes = last.re_upload_bytes.checked_sub(first.re_upload_bytes)?;
                let re_download_bytes = last
                    .re_download_bytes
                    .checked_sub(first.re_download_bytes)?;
                let http_download_bytes = last
                    .http_download_bytes
                    .checked_sub(first.http_download_bytes)?;

                let part = match draw_mode {
                    DrawMode::Normal => {
                        fn format_byte_per_second(bytes_per_second: u64) -> String {
                            if bytes_per_second == 0 {
                                " ".repeat(HumanizedBytesPerSecond::FIXED_WIDTH_WIDTH)
                            } else {
                                HumanizedBytesPerSecond::fixed_width(bytes_per_second).to_string()
                            }
                        }

                        let re_upload_bytes_per_second = two_snapshots
                            .re_upload_bytes_per_second()
                            .unwrap_or_default();
                        let re_download_bytes_per_second = two_snapshots
                            .re_download_bytes_per_second()
                            .unwrap_or_default();
                        let http_download_bytes_per_second = two_snapshots
                            .http_download_bytes_per_second()
                            .unwrap_or_default();
                        format!(
                            "Up: {} {}  Down: {} {}",
                            HumanizedBytes::fixed_width(re_upload_bytes),
                            format_byte_per_second(re_upload_bytes_per_second),
                            HumanizedBytes::fixed_width(re_download_bytes + http_download_bytes),
                            format_byte_per_second(
                                re_download_bytes_per_second + http_download_bytes_per_second
                            ),
                        )
                    }
                    DrawMode::Final => {
                        format!(
                            "Up: {}  Down: {}",
                            HumanizedBytes::new(re_upload_bytes),
                            HumanizedBytes::new(re_download_bytes + http_download_bytes),
                        )
                    }
                };
                parts.push(part);
            }
        }

        if let Some(session_id) = self.session_id.as_ref() {
            parts.push(format!("({})", session_id.to_owned()));
        }

        if parts.is_empty() {
            return None;
        }

        Some(format!("Network: {}", parts.join("  ")))
    }

    fn render_detailed_item_no_progress_stats(
        &self,
        name: &str,
        stat: u64,
    ) -> buck2_error::Result<Option<Line>> {
        let line = format!(
            "{name:<20}: \
            {stat:>5} bytes"
        );
        Ok(Some(Line::unstyled(&line).map_err(|e| {
            from_any_with_tag(e, buck2_error::ErrorTag::Tier0)
        })?))
    }

    fn render_detailed_items(
        &self,
        name: &str,
        started: u32,
        finished_successfully: u32,
        finished_with_error: u32,
    ) -> buck2_error::Result<Option<Line>> {
        let in_progress = started
            .saturating_sub(finished_successfully)
            .saturating_sub(finished_with_error);
        if in_progress == 0 && finished_successfully == 0 && finished_with_error == 0 {
            return Ok(None);
        }
        let line = format!(
            "{name:<20}: \
            {in_progress:>5} in progress, \
            {finished_successfully:>5} success, \
            {finished_with_error:>5} error"
        );
        Ok(Some(Line::unstyled(&line).map_err(|e| {
            from_any_with_tag(e, buck2_error::ErrorTag::Tier0)
        })?))
    }

    fn render_local_cache_stat(
        &self,
        name: &str,
        hits_files: i64,
        hits_bytes: i64,
        misses_files: i64,
        misses_bytes: i64,
    ) -> buck2_error::Result<Option<Line>> {
        let line = format!(
            "{:<20}: \
            {:>5} / {:>5} files hits, \
            {:>5} / {:>5} files misses ",
            name,
            HumanizedBytes::new(hits_bytes as u64),
            hits_files,
            HumanizedBytes::new(misses_bytes as u64),
            misses_files,
        );
        Ok(Some(Line::unstyled(&line).map_err(|e| {
            from_any_with_tag(e, buck2_error::ErrorTag::Tier0)
        })?))
    }

    fn render_detailed(&self, two_snapshots: &TwoSnapshots) -> buck2_error::Result<Vec<Line>> {
        let mut r = Vec::new();
        if let (Some(first), Some((_, last))) = (&self.first_snapshot, &two_snapshots.last) {
            r.extend(self.render_detailed_items(
                "re_uploads",
                last.re_uploads_started,
                last.re_uploads_finished_successfully,
                last.re_uploads_finished_with_error,
            )?);
            r.extend(self.render_detailed_items(
                "re_downloads",
                last.re_downloads_started,
                last.re_downloads_finished_successfully,
                last.re_downloads_finished_with_error,
            )?);
            r.extend(self.render_detailed_items(
                "re_action_cache",
                last.re_action_cache_started,
                last.re_action_cache_finished_successfully,
                last.re_action_cache_finished_with_error,
            )?);
            r.extend(self.render_detailed_items(
                "re_executes",
                last.re_executes_started,
                last.re_executes_finished_successfully,
                last.re_executes_finished_with_error,
            )?);
            r.extend(self.render_detailed_items(
                "re_materializes",
                last.re_materializes_started,
                last.re_materializes_finished_successfully,
                last.re_materializes_finished_with_error,
            )?);
            r.extend(self.render_detailed_items(
                "re_write_action_results",
                last.re_write_action_results_started,
                last.re_write_action_results_finished_successfully,
                last.re_write_action_results_finished_with_error,
            )?);
            r.extend(self.render_detailed_items(
                "re_get_digest_expirations",
                last.re_get_digest_expirations_started,
                last.re_get_digest_expirations_finished_successfully,
                last.re_get_digest_expirations_finished_with_error,
            )?);
            // TODO(raulgarcia4): Add some in-progress-stats for http metrics as well.
            r.extend(self.render_detailed_item_no_progress_stats(
                "http_download_bytes",
                last.http_download_bytes - first.http_download_bytes,
            )?);

            r.extend(self.render_local_cache_stat(
                "local_artifact_cache",
                last.local_cache_hits_files - first.local_cache_hits_files,
                last.local_cache_hits_bytes - first.local_cache_hits_bytes,
                last.local_cache_misses_files - first.local_cache_misses_files,
                last.local_cache_misses_bytes - first.local_cache_misses_bytes,
            )?);
        }
        Ok(r)
    }

    pub fn render(
        &self,
        two_snapshots: &TwoSnapshots,
        detailed: bool,
        draw_mode: DrawMode,
    ) -> buck2_error::Result<Lines> {
        let header = match self.render_header(two_snapshots, draw_mode) {
            Some(header) => header,
            None => return Ok(Lines::new()),
        };
        let mut lines = vec![
            Line::unstyled(&header)
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?,
        ];
        if detailed {
            lines.extend(self.render_detailed(two_snapshots)?);
        }
        Ok(Lines(lines))
    }
}
