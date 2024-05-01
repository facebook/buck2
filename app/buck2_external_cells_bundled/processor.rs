/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(unix)]
fn is_executable(path: &str) -> bool {
    use std::os::unix::fs::PermissionsExt;

    let permissions = std::fs::metadata(path)
        .expect("Failed to get metadata")
        .permissions();
    permissions.mode() & 0o111 != 0
}

#[cfg(not(unix))]
fn is_executable(_path: &str) -> bool {
    false
}

fn main() {
    let inp_path = std::env::args()
        .nth(1)
        .expect("Expected path to a source file");
    let exec_bit_path = std::env::args()
        .nth(2)
        .expect("Expected path to write the executable bit");
    let is_executable = is_executable(&inp_path);
    std::fs::write(&exec_bit_path, is_executable.to_string()).expect("Failed to write");
}
