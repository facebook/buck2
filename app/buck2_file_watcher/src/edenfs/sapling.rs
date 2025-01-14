/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::process::Stdio;

use buck2_error::buck2_error;
use buck2_error::BuckErrorContext;
use buck2_util::process::async_background_command;
use tokio::io::AsyncBufReadExt;
use tokio::io::BufReader;

#[derive(Debug, PartialEq)]
pub(crate) enum SaplingStatus {
    Modified,
    Added,
    Removed,
    Missing,
    NotTracked,
}

pub(crate) enum SaplingGetStatusResult {
    Normal(Vec<(SaplingStatus, String)>),
    TooManyChanges,
}

pub(crate) async fn get_mergebase<D: AsRef<Path>, C: AsRef<str>, M: AsRef<str>>(
    current_dir: D,
    commit: C,
    mergegase_with: M,
) -> buck2_error::Result<Option<String>> {
    let output = async_background_command("sl")
        .current_dir(current_dir)
        .env("HGPLAIN", "1")
        .args([
            "log",
            "--traceback",
            "-T",
            "{node}",
            "-r",
            format!("ancestor({}, {})", commit.as_ref(), mergegase_with.as_ref()).as_str(),
        ])
        .output()
        .await
        .buck_error_context("Failed to obtain mergebase")?;

    if !output.status.success() || !output.stderr.is_empty() {
        buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "Failed to obtain mergebase:\n{}",
            String::from_utf8(output.stderr)
                .buck_error_context("Failed to stderr reported by get_mergebase.")?
        );
    }

    let mergebase =
        String::from_utf8(output.stdout).buck_error_context("Failed to parse mergebase")?;
    if mergebase.is_empty() {
        Ok(None)
    } else {
        Ok(Some(mergebase))
    }
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
    let mut args = vec!["status", "--traceback", "-mardui", "--rev", first.as_ref()];
    if let Some(ref second) = second {
        args.push("--rev");
        args.push(second.as_ref());
    }

    let mut output = async_background_command("sl")
        .current_dir(current_dir)
        .args(args.as_slice())
        .stdout(Stdio::piped())
        .spawn()
        .buck_error_context("Failed to obtain Sapling status")?;

    let stdout = output.stdout.take().ok_or_else(|| {
        buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "Failed to read stdout when invoking 'sl status'."
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
//    M fbcode/buck2/app/buck2_file_watcher/src/edenfs/sapling.rs
//    A fbcode/buck2/app/buck2_file_watcher/src/edenfs/sapling.rs
//    R fbcode/buck2/app/buck2_file_watcher/src/edenfs/sapling.rs
//    ! fbcode/buck2/app/buck2_file_watcher/src/edenfs/sapling.rs
//    ? fbcode/buck2/app/buck2_file_watcher/src/edenfs/sapling.rs
//
// Where:
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
    // Must include a status and at least one char path.
    if line.len() >= 3 {
        let mut chars = line.chars();
        let change = chars.next().unwrap();
        let path = chars.skip(1).collect::<String>();
        Ok(match change {
            'M' => Some((SaplingStatus::Modified, path)),
            'A' => Some((SaplingStatus::Added, path)),
            'R' => Some((SaplingStatus::Removed, path)),
            '!' => Some((SaplingStatus::Missing, path)),
            '?' => Some((SaplingStatus::NotTracked, path)),
            _ => None, // Skip all others
        })
    } else {
        Err(buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "Invalid status line: {line}"
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sl_status_line() -> buck2_error::Result<()> {
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

        assert_eq!(
            process_one_status_line("C buck2/app/buck2_file_watcher/src/edenfs/sapling.rs")?,
            None
        );

        assert!(process_one_status_line("NO").is_err());

        Ok(())
    }
}
