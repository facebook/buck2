/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::env;
use std::path::Path;
use std::process::Stdio;

use allocative::Allocative;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_util::process::async_background_command;
use tokio::io::AsyncBufReadExt;
use tokio::io::BufReader;

#[derive(Debug, PartialEq)]
pub(crate) enum SaplingStatus {
    Modified,
    Added,
    Removed,
    Clean,
    Missing,
    NotTracked,
    Ignored,
    Copied,
}

pub(crate) enum SaplingGetStatusResult {
    Normal(Vec<(SaplingStatus, String)>),
    TooManyChanges,
}

#[allow(dead_code)]
#[derive(Allocative, Clone)]
pub(crate) struct MergebaseDetails {
    pub mergebase: String,
    pub timestamp: Option<u64>,
    pub global_rev: Option<u64>,
}

impl PartialEq for MergebaseDetails {
    fn eq(&self, other: &Self) -> bool {
        self.mergebase == other.mergebase
    }
}

fn get_sapling_exe_path() -> String {
    env::var("EDEN_HG_BINARY").unwrap_or("hg".to_owned())
}

pub(crate) async fn get_mergebase<D: AsRef<Path>, C: AsRef<str>, M: AsRef<str>>(
    current_dir: D,
    commit: C,
    mergegase_with: M,
) -> buck2_error::Result<Option<MergebaseDetails>> {
    let output = async_background_command(get_sapling_exe_path())
        .current_dir(current_dir)
        .env("HGPLAIN", "1")
        .args([
            "log",
            "--traceback",
            "-T",
            "{node}\n{date}\n{get(extras, \"global_rev\")}",
            "-r",
            format!("ancestor({}, {})", commit.as_ref(), mergegase_with.as_ref()).as_str(),
        ])
        .output()
        .await
        .buck_error_context("Failed to obtain mergebase")?;

    if !output.status.success() || !output.stderr.is_empty() {
        buck2_error!(
            buck2_error::ErrorTag::Sapling,
            "Failed to obtain mergebase:\n{}",
            String::from_utf8(output.stderr)
                .buck_error_context("Failed to stderr reported by get_mergebase.")?
        );
    }

    parse_log_output(output.stdout)
}

fn parse_log_output(output: Vec<u8>) -> buck2_error::Result<Option<MergebaseDetails>> {
    let output = String::from_utf8(output).buck_error_context("Failed to parse hg log output")?;
    if output.is_empty() {
        return Ok(None);
    }
    let v: Vec<&str> = output.trim().splitn(3, '\n').collect();
    let mergebase = v
        .first()
        .ok_or_else(|| internal_error!("Failed to parse mergebase"))?
        .to_string();
    let timestamp = v
        .get(1)
        .and_then(|t| t.parse::<f64>().ok())
        .map(|t| t as u64); // hg returns the fractional seconds
    let global_rev = if let Some(global_rev) = v.get(2) {
        Some(
            global_rev
                .parse::<u64>()
                .buck_error_context("Failed to parse global_rev")?,
        )
    } else {
        None
    };

    Ok(Some(MergebaseDetails {
        mergebase,
        timestamp,
        global_rev,
    }))
}

// Get status between two revisions. If second is None, then it is the working copy.
// Limit the number of results to limit_results. If the number of results is greater than
// limit_results, then return true (and empty vec) to indicate that there are more results.
pub(crate) async fn get_status<D: AsRef<Path>, F: AsRef<str>, S: AsRef<str>>(
    current_dir: D,
    first: F,
    second: Option<S>,
    limit_results: usize,
) -> buck2_error::Result<SaplingGetStatusResult> {
    let mut args = vec!["status", "--traceback", "-mardu", "--rev", first.as_ref()];
    if let Some(ref second) = second {
        args.push("--rev");
        args.push(second.as_ref());
    }

    let mut output = async_background_command(get_sapling_exe_path())
        .current_dir(current_dir)
        .env("HGPLAIN", "1")
        .args(args.as_slice())
        .stdout(Stdio::piped())
        .spawn()
        .buck_error_context("Failed to obtain Sapling status")?;

    let stdout = output.stdout.take().ok_or_else(|| {
        buck2_error!(
            buck2_error::ErrorTag::Sapling,
            "Failed to read stdout when invoking 'hg status'."
        )
    })?;
    let reader = BufReader::new(stdout);

    let mut status = vec![];
    let mut lines = reader.lines();
    while let Some(line) = lines.next_line().await? {
        if let Some(status_line) = process_one_status_line(&line)? {
            if status.len() >= limit_results {
                return Ok(SaplingGetStatusResult::TooManyChanges);
            }
            status.push(status_line);
        }
    }

    Ok(SaplingGetStatusResult::Normal(status))
}

// Get directory differences between two revisions. If second is None, then it is the working copy.
// Limit the number of results to limit_results. If the number of results is greater than
// limit_results return TooManyResults.
pub(crate) async fn get_dir_diff<D: AsRef<Path>, F: AsRef<str>, S: AsRef<str>>(
    current_dir: D,
    first: F,
    second: Option<S>,
    limit_results: usize,
) -> buck2_error::Result<SaplingGetStatusResult> {
    let mut args = vec!["debugdiffdirs", "--rev", first.as_ref()];
    if let Some(ref second) = second {
        args.push("--rev");
        args.push(second.as_ref());
    }

    let mut output = async_background_command(get_sapling_exe_path())
        .current_dir(current_dir)
        .env("HGPLAIN", "1")
        .args(args.as_slice())
        .stdout(Stdio::piped())
        .spawn()
        .buck_error_context("Failed to obtain Sapling debugdiffdirs")?;

    let stdout = output.stdout.take().ok_or_else(|| {
        buck2_error!(
            buck2_error::ErrorTag::Sapling,
            "Failed to read stdout when invoking 'hg debugdiffdirs'."
        )
    })?;
    let reader = BufReader::new(stdout);

    let mut status = vec![];
    let mut lines = reader.lines();
    while let Some(line) = lines.next_line().await? {
        if let Some(status_line) = process_one_status_line(&line)? {
            if status.len() >= limit_results {
                return Ok(SaplingGetStatusResult::TooManyChanges);
            }
            status.push(status_line);
        }
    }

    Ok(SaplingGetStatusResult::Normal(status))
}

//
// Single line looks like:
//    <status> <path>
//
// Where status is one of:
//   M = modified
//   A = added
//   R = removed
//   C = clean
//   ! = missing (deleted by a non-sl command, but still tracked)
//   ? = not tracked
//   I = ignored
//     = origin of the previous file (with --copies)
// Note:
//   Paths can have spaces, but are not quoted.
fn process_one_status_line(line: &str) -> buck2_error::Result<Option<(SaplingStatus, String)>> {
    let mut chars = line.chars();
    // Must include a status and at least one char path.
    if let (Some(status), Some(' '), path) = (chars.next(), chars.next(), chars.collect::<String>())
    {
        let path = path.to_owned();
        Ok(match status {
            'M' => Some((SaplingStatus::Modified, path)),
            'A' => Some((SaplingStatus::Added, path)),
            'R' => Some((SaplingStatus::Removed, path)),
            'C' => Some((SaplingStatus::Clean, path)),
            '!' => Some((SaplingStatus::Missing, path)),
            '?' => Some((SaplingStatus::NotTracked, path)),
            'I' => Some((SaplingStatus::Ignored, path)),
            ' ' => Some((SaplingStatus::Copied, path)),
            _ => None, // Skip all others
        })
    } else {
        Err(buck2_error!(
            buck2_error::ErrorTag::Sapling,
            "Invalid status line: {line}"
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hg_status_line() -> buck2_error::Result<()> {
        assert_eq!(
            process_one_status_line("M buck2/app/buck2_file_watcher/src/edenfs/sapling.rs")?,
            Some((
                SaplingStatus::Modified,
                "buck2/app/buck2_file_watcher/src/edenfs/sapling.rs".to_owned()
            ))
        );

        assert_eq!(
            process_one_status_line("A buck2/app/buck2_file_watcher/src/edenfs/interface.rs")?,
            Some((
                SaplingStatus::Added,
                "buck2/app/buck2_file_watcher/src/edenfs/interface.rs".to_owned()
            ))
        );

        assert_eq!(
            process_one_status_line("R buck2/app/buck2_file_watcher/src/edenfs/utils.rs")?,
            Some((
                SaplingStatus::Removed,
                "buck2/app/buck2_file_watcher/src/edenfs/utils.rs".to_owned()
            ))
        );

        assert_eq!(
            process_one_status_line("! buck2/app/buck2_file_watcher/src/edenfs/sapling.rs")?,
            Some((
                SaplingStatus::Missing,
                "buck2/app/buck2_file_watcher/src/edenfs/sapling.rs".to_owned()
            ))
        );

        assert_eq!(
            process_one_status_line("? buck2/app/buck2_file_watcher/src/edenfs/sapling.rs")?,
            Some((
                SaplingStatus::NotTracked,
                "buck2/app/buck2_file_watcher/src/edenfs/sapling.rs".to_owned()
            ))
        );

        // Space in path
        assert_eq!(
            process_one_status_line("M ovrsource-legacy/unity/socialvr/modules/wb_unity_asset_bundles/Assets/MetaHorizonUnityAssetBundle/Editor/Unity Dependencies/ABDataSource.cs")?,
            Some((
                SaplingStatus::Modified,
                "ovrsource-legacy/unity/socialvr/modules/wb_unity_asset_bundles/Assets/MetaHorizonUnityAssetBundle/Editor/Unity Dependencies/ABDataSource.cs".to_owned()
            ))
        );

        assert_eq!(
            process_one_status_line("C buck2/app/buck2_file_watcher/src/edenfs/sapling.rs")?,
            Some((
                SaplingStatus::Clean,
                "buck2/app/buck2_file_watcher/src/edenfs/sapling.rs".to_owned()
            ))
        );

        assert_eq!(
            process_one_status_line("I buck2/app/buck2_file_watcher/src/edenfs/sapling.rs")?,
            Some((
                SaplingStatus::Ignored,
                "buck2/app/buck2_file_watcher/src/edenfs/sapling.rs".to_owned()
            ))
        );

        assert_eq!(
            process_one_status_line("  buck2/app/buck2_file_watcher/src/edenfs/sapling.rs")?,
            Some((
                SaplingStatus::Copied,
                "buck2/app/buck2_file_watcher/src/edenfs/sapling.rs".to_owned()
            ))
        );

        assert!(process_one_status_line("NO").is_err());

        // Invalid status (missing status), but valid path with space in it
        assert!(
            process_one_status_line(" ovrsource-legacy/unity/socialvr/modules/wb_unity_asset_bundles/Assets/MetaHorizonUnityAssetBundle/Editor/Unity Dependencies/ABDataSource.cs")
                .is_err());

        // Malformed status (no space)
        assert!(
            process_one_status_line("Mbuck2/app/buck2_file_watcher/src/edenfs/sapling.rs").is_err()
        );

        // Malformed status (colon instead of space)
        assert!(
            process_one_status_line("M:buck2/app/buck2_file_watcher/src/edenfs/sapling.rs")
                .is_err()
        );

        Ok(())
    }

    #[test]
    fn test_parse_log_output() -> buck2_error::Result<()> {
        // the format is {node}\n{date}\n{global_rev}
        let output =
            "71de423b796418e8ff5300dbe9bd9ad3aef63a9c\n1739790802.028800\n1020164040".to_owned();
        let details = parse_log_output(output.as_bytes().to_vec())?.unwrap();
        assert_eq!(
            details.mergebase,
            "71de423b796418e8ff5300dbe9bd9ad3aef63a9c"
        );
        assert_eq!(details.timestamp, Some(1739790802));
        assert_eq!(details.global_rev, Some(1020164040));
        Ok(())
    }

    #[test]
    fn test_parse_log_output_no_global_rev() -> buck2_error::Result<()> {
        // Not all repos have global revision
        let output = "71de423b796418e8ff5300dbe9bd9ad3aef63a9c\n1739790802.028800\n".to_owned();
        let details = parse_log_output(output.as_bytes().to_vec())?.unwrap();
        assert_eq!(
            details.mergebase,
            "71de423b796418e8ff5300dbe9bd9ad3aef63a9c"
        );
        assert_eq!(details.global_rev, None);
        assert_eq!(details.timestamp, Some(1739790802));
        Ok(())
    }
}
