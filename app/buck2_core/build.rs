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
use std::fs;

/// Returns the version of the rustc compiler.
fn rustc_version() -> String {
    let rustc = env::var("RUSTC").unwrap();
    // @patternlint-disable-next-line buck2-no-command-new
    let version = std::process::Command::new(rustc)
        .arg("--version")
        .output()
        .unwrap();

    assert!(version.status.success());

    let stdout = String::from_utf8(version.stdout).unwrap();
    stdout.trim().to_owned()
}

/// Get the version we expect to be using.
fn expected_version() -> String {
    let path = "../../rust-toolchain";
    for line in fs::read_to_string(path).unwrap().lines() {
        let prefix = "# @rustc_version: ";
        if let Some(version) = line.strip_prefix(prefix) {
            return version.to_owned();
        }
    }
    panic!("could not find rust-toolchain version in `{path}`");
}

fn check_rustc_version() {
    let expected = expected_version();
    let actual = rustc_version();
    if actual != expected {
        panic!(
            "buck2 only works with version `{expected}` of rustc, but you are \
            using `{actual}`. Correct version is installed automatically when \
            `rustup` is used",
        );
    }
}

fn main() {
    if !cfg!(fbcode_build) {
        check_rustc_version();
    }
}
