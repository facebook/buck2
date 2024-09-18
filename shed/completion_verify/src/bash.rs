/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;

use crate::extract_from_outputs;
use crate::runtime::BashRuntime;
use crate::runtime::Term;

pub(crate) fn run_bash(script: &str, input: &str) -> io::Result<Vec<String>> {
    let home = tempfile::tempdir()?;
    let home = home.path();

    std::fs::write(
        home.join(".bashrc"),
        "\
PS1='% '
",
    )?;
    std::fs::write(
        home.join(".inputrc"),
        "# expected empty file to disable loading ~/.inputrc\n",
    )?;
    let mut r = BashRuntime::with_home(home.to_owned())?;
    r.register("buck2", script)?;

    let one_tab = r.complete(&format!("{}\t", input), &Term::new())?;
    // complete_pty turns on echoing in this case to work around a bash bug, so strip that
    let one_tab = one_tab.strip_prefix(input).unwrap_or(&one_tab).trim_start();

    // Depending on situation, bash might need as many as three tabs to produce all completions
    extract_from_outputs(
        input,
        [
            Ok(one_tab.to_owned()),
            r.complete(&format!("{}\t\t", input), &Term::new()),
            r.complete(&format!("{}\t\t\t", input), &Term::new()),
        ],
    )
}
