/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::path::Path;
use std::path::PathBuf;

pub(crate) fn canonicalize<P: AsRef<Path>>(path: P) -> io::Result<PathBuf> {
    let canonical_path = dunce::canonicalize(&path)?;

    if cfg!(windows) && path.as_ref().starts_with("\\\\?\\") {
        tracing::warn!(
            path = path.as_ref().display().to_string(),
            "Couldn't strip UNC prefix from path. Using it as-is."
        );
    }

    Ok(canonical_path)
}
