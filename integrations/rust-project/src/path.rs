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

/// Try to canonicalize `path`, but return `path` as-is if we couldn't
/// (e.g. the path didn't exist, broken symlinks, etc).
///
/// This is useful when being invoked by the IDE on unfinished code: we
/// might see all sorts of broken paths whilst the user is working.
pub(crate) fn safe_canonicalize(path: &Path) -> PathBuf {
    match canonicalize(path) {
        Ok(path) => path,
        Err(_) => path.to_owned(),
    }
}

/// If the canonical version of `path` is in a source code checkout, return
/// the canonical path, otherwise return the path as-is.
pub(crate) fn canonicalize_to_vcs_path(path: &Path, project_root: &Path) -> PathBuf {
    let canonical_path = safe_canonicalize(path);

    // Buck builds Rust code by creating symlinks in buck-out/ to the
    // files in VCS (e.g. hg, git). This is what rustc sees, but we
    // don't want rust-analyzer to see the files in buck-out. We want
    // rust-analyzer to see the files in their original location, so
    // go-to-def takes the user to `my-project/src/foo.rs` not
    // `buck-out/abc123/my-project/foo.rs`.
    //
    // For generated files, there is no file in VCS. The symlink in
    // buck-out just points to the build output that generated the
    // file. The build output directory may not be a valid crate
    // directory.
    //
    // ```text
    // $ ls -l ~/fbsource/buck-out/.rust-analyzer/gen/abc123/my_project/__foo_type_defs-rust__/__srcs/
    // consts.rs -> ../../__foo_type_defs-rust-foo_type_defs.thrift__/out/gen-rust/consts.rs
    // docs.md -> ../../__foo_type_defs-rust-crate__/out/types.md
    // lib.rs -> ../../__foo_type_defs-rust-foo_type_defs.thrift__/out/gen-rust/types.rs
    // services.rs -> ../../__foo_type_defs-rust-foo_type_defs.thrift__/out/gen-rust/services.rs
    // ```
    //
    // In this example, expanding the symlink to the root module
    // (lib.rs) would mean that we'd use
    // ~/fbsource/buck-out/.rust-analyzer/gen/fbcode/abc123/my_project/__foo_type_defs-rust-foo_type_defs.thrift__/out/gen-rust/
    // as the crate root, which does not contain a lib.rs file.
    if let Ok(rel) = canonical_path.strip_prefix(project_root) {
        // The file was generated, use the original path, don't expand
        // symlinks.
        if rel.starts_with("buck-out") {
            return path.to_owned();
        }
    }

    canonical_path
}

fn canonicalize<P: AsRef<Path>>(path: P) -> io::Result<PathBuf> {
    let canonical_path = dunce::canonicalize(&path)?;

    if cfg!(windows) && path.as_ref().starts_with("\\\\?\\") {
        tracing::warn!(
            path = path.as_ref().display().to_string(),
            "Couldn't strip UNC prefix from path. Using it as-is."
        );
    }

    Ok(canonical_path)
}
