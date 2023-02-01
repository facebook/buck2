/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use thiserror::Error;
use tokio::process::Command;

#[derive(Debug, Error)]
enum SourceControlError {
    #[error("HG command failed with code '{0}' and error '{1}' ")]
    HgCommand(i32, String),
    #[error("Git command failed with code '{0}' and error '{1}' ")]
    GitCommand(i32, String),
    #[error("`{0}` was not UTF-8")]
    Utf8(String),
}

enum CommandResult {
    Ok(String),
    RepoNotFound,
}

pub async fn get_info() -> anyhow::Result<String> {
    let hg_info = get_hg_info().await;
    if let Ok(CommandResult::Ok(output)) = hg_info {
        return Ok(output);
    }
    let git_info = get_git_info().await;
    if let Ok(CommandResult::Ok(output)) = git_info {
        return Ok(output);
    }
    hg_info?;
    git_info?;
    Ok("Current directory is not inside a repository (tried hg and git)".to_owned())
}

async fn get_hg_info() -> anyhow::Result<CommandResult> {
    let result = Command::new("hg")
        .args(["snapshot", "create"])
        .env("HGPLAIN", "1")
        .output()
        .await?;
    if result.status.success() {
        let output = from_utf8(result.stdout, "hg snapshot stdout")?;
        return Ok(CommandResult::Ok(format!("HG snapshot ID: {}", output)));
    };
    let error = from_utf8(result.stderr, "hg snapshot stderr")?;
    if error.contains("is not inside a repository") {
        return Ok(CommandResult::RepoNotFound);
    };
    let code = result.status.code().unwrap_or(1);
    Err(SourceControlError::HgCommand(code, error).into())
}

async fn get_git_info() -> anyhow::Result<CommandResult> {
    let commit_hash = Command::new("git")
        .args(["log", "-1", "--format=%H"])
        .output()
        .await?;
    if !commit_hash.status.success() {
        let error = from_utf8(commit_hash.stderr, "git log stderr")?;
        if error.contains("not a git repository") {
            return Ok(CommandResult::RepoNotFound);
        };
        let code = commit_hash.status.code().unwrap_or(1);
        return Err(SourceControlError::GitCommand(code, error).into());
    };

    let status = Command::new("git").args(["status", "-sb"]).output().await?;
    if !status.status.success() {
        let error = from_utf8(status.stderr, "git status stderr")?;
        let code = status.status.code().unwrap_or(1);
        return Err(SourceControlError::GitCommand(code, error).into());
    };
    Ok(CommandResult::Ok(format!(
        "Git base commit hash:\n{}\nGit status:\n{}",
        from_utf8(commit_hash.stdout, "git log stdout")?,
        from_utf8(status.stdout, "git status stdout")?
    )))
}

fn from_utf8(result: Vec<u8>, subject: &str) -> anyhow::Result<String> {
    String::from_utf8(result).context(SourceControlError::Utf8(subject.to_owned()))
}
