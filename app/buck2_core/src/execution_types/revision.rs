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

use buck2_util::process::background_command;
use once_cell::sync::Lazy;

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

pub(super) static REVISION: Lazy<Option<String>> = Lazy::new(|| {
    if let Some(id) = get_vcs_revision(RepoVcs::Hg) {
        return Some(id);
    }
    get_vcs_revision(RepoVcs::Git)
});
