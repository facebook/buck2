/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::path::PathBuf;

/// Set up $PROTOC to point to the in repo binary if available
///
/// Note: repo root is expected to be a relative or absolute path to the root of the repository.
pub fn maybe_setup_protoc(repo_root: &str) {
    // Bail if the env var is explicitly set
    if std::env::var_os("PROTOC").is_some() {
        return;
    }

    let mut protoc = PathBuf::from(repo_root);
    protoc.push("third-party/protobuf/dotslash/protoc");
    if cfg!(windows) {
        protoc.push(".exe");
    }

    // Bail if we can't find the path to the in repo protoc, e.g. for OSS builds.
    if !protoc.exists() {
        return;
    }

    let protoc = fs::canonicalize(protoc).expect("Failed to canonicalize protoc path");
    std::env::set_var("PROTOC", protoc);
}
