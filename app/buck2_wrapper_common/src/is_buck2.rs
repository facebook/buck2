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
use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;
use std::sync::OnceLock;

use dupe::Dupe;

#[derive(Copy, Clone, Dupe)]
pub enum WhoIsAsking {
    Buck2,
    BuckWrapper,
}

pub(crate) fn is_buck2_exe(path: &Path, who_is_asking: WhoIsAsking) -> bool {
    let Some(file_stem) = path.file_stem() else {
        return false;
    };
    // On linux when the running executable is deleted or unlinked the string ' (deleted)' is appended to symlinked file in /proc/<pid>/exe
    if [
        OsStr::new("buck2"),
        OsStr::new("buck2 (deleted)"),
        OsStr::new("buck2-daemon"),
        OsStr::new("buck2-daemon (deleted)"),
    ].contains(&file_stem) {
        true
    } else {
        match who_is_asking {
            WhoIsAsking::BuckWrapper => {
                // We don't know another name of the buck2 executable in the wrapper.
                false
            }
            WhoIsAsking::Buck2 => {
                static CURRENT_EXE: OnceLock<PathBuf> = OnceLock::new();
                if let Ok(current_exe) = CURRENT_EXE.get_or_try_init(env::current_exe) {
                    if let Some(current_exe_file_stem) = current_exe.file_stem() {
                        if current_exe_file_stem == file_stem {
                            return true;
                        }
                    }
                }
                false
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::path::Path;

    use crate::is_buck2::WhoIsAsking;
    use crate::is_buck2::is_buck2_exe;

    #[test]
    fn test_is_buck2_exe() {
        let (fake_buck, other_path) = if cfg!(windows) {
            ("C:\\dir\\buck2.exe", "C:\\dir\\other.exe")
        } else {
            ("/dir/buck2", "/dir/other")
        };

        assert!(is_buck2_exe(Path::new(fake_buck), WhoIsAsking::Buck2));
        assert!(is_buck2_exe(Path::new(fake_buck), WhoIsAsking::BuckWrapper));

        let current_exe = env::current_exe().unwrap();

        assert!(is_buck2_exe(&current_exe, WhoIsAsking::Buck2));
        assert!(!is_buck2_exe(&current_exe, WhoIsAsking::BuckWrapper));

        assert!(!is_buck2_exe(Path::new(other_path), WhoIsAsking::Buck2));
        assert!(!is_buck2_exe(
            Path::new(other_path),
            WhoIsAsking::BuckWrapper
        ));
    }
}
