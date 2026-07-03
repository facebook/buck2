/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//!
//! The paths module for buck2.
//!
//! Introduces 'ForwardRelativePath', 'ForwardRelativePathBuf', 'AbsPath', and
//! 'AbsPathBuf', which are equivalents of 'Path' and 'PathBuf'.
//!
//! ForwardRelativePaths are fully normalized relative platform agnostic paths
//! that only points forward. This means  that there is no `.` or `..` in this
//! path, and does not begin with `/`. These are resolved to the 'PathBuf' by
//! resolving them against an 'AbsPath'.
//!
//! 'AbsPath' are absolute paths, meaning they must start with a directory root
//! of either `/` or some  windows root directory like `c:`. These behave
//! roughly like 'Path'.

use std::sync::OnceLock;

pub mod abs_norm_path;
pub mod abs_path;
pub mod cmp_impls;
pub mod file_name;
pub mod fmt;
pub mod forward_rel_path;
pub mod into_filename_buf_iterator;
pub mod path_util;
pub mod relative_path;

pub use into_filename_buf_iterator::IntoFileNameBufIterator;

pub use self::relative_path::RelativePath;
pub use self::relative_path::RelativePathBuf;

static ALLOW_BACKSLASHES_IN_PATHS: OnceLock<bool> = OnceLock::new();

/// Initializes the process-wide path policy from the daemon startup configuration.
///
/// Client and daemon initialization share a process under `--no-buckd`, so setting the same value
/// more than once is allowed. A conflicting value would let one process mix path invariants and is
/// therefore rejected.
pub fn init_allow_backslashes_in_paths(value: bool) -> buck2_error::Result<()> {
    init_allow_backslashes_in_paths_impl(&ALLOW_BACKSLASHES_IN_PATHS, value)
}

fn init_allow_backslashes_in_paths_impl(
    cell: &OnceLock<bool>,
    value: bool,
) -> buck2_error::Result<()> {
    let initialized = *cell.get_or_init(|| value);
    if initialized == value {
        Ok(())
    } else {
        Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "Backslash path policy is already initialized to `{initialized}`, cannot set it to \
             `{value}`"
        ))
    }
}

/// A backslash is a path separator on Windows, so it is always rejected there. On other
/// platforms it is a valid path character, but is rejected by default to keep paths portable.
pub(crate) fn backslash_allowed() -> bool {
    effective_backslash_policy(ALLOW_BACKSLASHES_IN_PATHS.get().copied().unwrap_or(false))
}

fn effective_backslash_policy(configured: bool) -> bool {
    !cfg!(windows) && configured
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn backslash_policy_initialization_is_idempotent_but_not_mutable() {
        let policy = OnceLock::new();
        assert!(init_allow_backslashes_in_paths_impl(&policy, false).is_ok());
        assert!(init_allow_backslashes_in_paths_impl(&policy, false).is_ok());
        assert!(init_allow_backslashes_in_paths_impl(&policy, true).is_err());
    }

    #[test]
    fn backslashes_are_never_literal_path_characters_on_windows() {
        assert!(!effective_backslash_policy(false));
        assert_eq!(effective_backslash_policy(true), !cfg!(windows));
    }
}
