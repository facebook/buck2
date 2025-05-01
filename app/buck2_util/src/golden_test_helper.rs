/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Used by golden tests written in rust, particularly useful for starlark tests
//!
//! This is mostly a copy from starlark_syntax/src/golden_test_template.rs.
//! Copied so we wouldn't need to depend on starlark_syntax for tests.

use std::env;
use std::fmt::Write;
use std::fs;

use buck2_error::BuckErrorContext;

const REGENERATE_VAR_NAME: &str = "BUCK2_RUST_REGENERATE_GOLDEN_TESTS";

#[allow(clippy::write_literal)] // We mark generated files as generated, but not this file.
fn make_golden(output: &str) -> String {
    let mut golden = String::new();
    writeln!(golden, "# {at}generated", at = "@").unwrap();
    writeln!(
        golden,
        "# To regenerate, append -- --env {REGENERATE_VAR_NAME}=1 and re-run the test"
    )
    .unwrap();
    writeln!(golden).unwrap();
    writeln!(golden, "{}", output.trim_end()).unwrap();
    golden
}

/// Common code for golden tests.
pub fn golden_test_template(golden_rel_path: &str, output: &str) {
    assert!(golden_rel_path.contains(".golden"));

    let manifest_dir =
        env::var("CARGO_MANIFEST_DIR").expect("`CARGO_MANIFEST_DIR` variable must be set");
    let golden_file_path = format!("{manifest_dir}/{golden_rel_path}");
    let output_with_prefix = make_golden(output);

    if env::var(REGENERATE_VAR_NAME).is_ok() {
        fs::write(&golden_file_path, &output_with_prefix)
            .with_buck_error_context(|| format!("Writing `{golden_file_path}`"))
            .unwrap();
    } else {
        let expected = fs::read_to_string(&golden_file_path)
            .with_buck_error_context(|| format!("Reading `{golden_file_path}`"))
            .unwrap();

        let expected = if cfg!(windows) {
            // Git may check out files on Windows with \r\n as line separator.
            // We could configure git, but it's more reliable to handle it in the test.
            expected.replace("\r\n", "\n")
        } else {
            expected
        };
        assert_eq!(expected, output_with_prefix);
    }
}

/// Duplicate of `starlark::tests::util::trim_rust_backtrace` to avoid exposing test internals.
/// There's no anyhow API to print error without rust backtrace
/// ([issue](https://github.com/dtolnay/anyhow/issues/300)).
pub fn trim_rust_backtrace(error: &str) -> &str {
    match error.find("\nStack backtrace:") {
        Some(pos) => error[..pos].trim_end(),
        None => error.trim_end(),
    }
}
