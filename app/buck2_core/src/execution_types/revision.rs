/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::process::Stdio;
use std::sync::RwLock;

use buck2_util::process::background_command;

static CACHED_REVISION: RwLock<Option<String>> = RwLock::new(None);

enum RepoVcs {
    Hg,
    Git,
}

fn get_vcs_revision(vcs: RepoVcs) -> Option<String> {
    let mut cmd = match vcs {
        RepoVcs::Hg => {
            let mut cmd = background_command("hg");
            cmd.args(["id", "-i", "--debug"]);
            cmd.env("HGPLAIN", "1");
            cmd
        }
        RepoVcs::Git => {
            let mut cmd = background_command("git");
            cmd.args(["describe", "--always", "--dirty=+", "--abbrev=40"]);
            cmd
        }
    };
    let output = cmd
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    Some(std::str::from_utf8(&output.stdout).ok()?.trim().to_owned())
}

fn compute_revision() -> Option<String> {
    if let Some(id) = get_vcs_revision(RepoVcs::Hg) {
        return Some(id);
    }
    get_vcs_revision(RepoVcs::Git)
}

pub fn clear_revision() {
    *CACHED_REVISION.write().unwrap_or_else(|e| e.into_inner()) = None;
}

fn refresh_revision() -> Option<String> {
    let mut guard = CACHED_REVISION.write().unwrap_or_else(|e| e.into_inner());
    // Recheck inside the lock to avoid queueing up multiple refreshes.
    if let Some(revision) = guard.as_ref() {
        return Some(revision.clone());
    }
    *guard = compute_revision();
    guard.clone()
}

pub(super) struct Revision;

impl Revision {
    pub fn get(&self) -> Option<String> {
        let cached = CACHED_REVISION
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .clone();
        match cached {
            Some(revision) => Some(revision),
            None => refresh_revision(),
        }
    }
}

pub(super) static REVISION: Revision = Revision;
