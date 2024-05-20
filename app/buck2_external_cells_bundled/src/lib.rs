/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[derive(Copy, Clone)]
pub struct BundledFile {
    pub path: &'static str,
    /// FIXME(JakobDegen): Consider compressing the data
    pub contents: &'static [u8],
    pub is_executable: bool,
}

#[derive(Copy, Clone)]
pub struct BundledCell {
    pub name: &'static str,
    pub files: &'static [BundledFile],
    pub is_testing: bool,
}

#[cfg(buck_build)]
mod nano_prelude {
    include!("nano_prelude/contents.rs");
}

#[cfg(buck_build)]
const NANO_PRELUDE: BundledCell = BundledCell {
    name: "nano_prelude",
    files: nano_prelude::DATA,
    is_testing: true,
};

#[cfg(buck_build)]
mod prelude {
    include!("prelude/contents.rs");
}

#[cfg(not(buck_build))]
mod prelude {
    include!(concat!(env!("OUT_DIR"), "/include.rs"));
}

const PRELUDE: BundledCell = BundledCell {
    name: "prelude",
    files: prelude::DATA,
    is_testing: false,
};

const TEST_CELL: BundledCell = BundledCell {
    name: "test_bundled_cell",
    files: &[
        BundledFile {
            path: ".buckconfig",
            contents: include_bytes!("../test_data/.buckconfig"),
            is_executable: false,
        },
        BundledFile {
            path: "BUCK_TREE",
            contents: include_bytes!("../test_data/BUCK_TREE"),
            is_executable: false,
        },
        BundledFile {
            path: "dir/src.txt",
            contents: include_bytes!("../test_data/dir/src.txt"),
            is_executable: false,
        },
        BundledFile {
            path: "dir/src2.txt",
            contents: include_bytes!("../test_data/dir/src2.txt"),
            is_executable: true,
        },
        BundledFile {
            path: "dir/src3.txt",
            contents: include_bytes!("../test_data/dir/src3.txt"),
            is_executable: true,
        },
        BundledFile {
            path: "dir/BUCK.fixture",
            contents: include_bytes!("../test_data/dir/BUCK.fixture"),
            is_executable: false,
        },
        BundledFile {
            path: "dir/defs.bzl",
            contents: include_bytes!("../test_data/dir/defs.bzl"),
            is_executable: false,
        },
    ],
    is_testing: true,
};

pub const fn get_bundled_data() -> &'static [BundledCell] {
    #[cfg(buck_build)]
    {
        &[TEST_CELL, NANO_PRELUDE, PRELUDE]
    }
    #[cfg(not(buck_build))]
    {
        &[TEST_CELL, PRELUDE]
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_sanity_check() {
        let c = super::TEST_CELL;
        assert!(c.files.iter().any(|file| {
            file.path == "dir/src.txt"
            // Git may check out files on Windows with \r\n as line separator.
            && std::str::from_utf8(file.contents).unwrap().replace("\r\n", "\n") == "foobar\n"
        }))
    }

    #[test]
    fn test_bundled_prelude_data() {
        let c = super::PRELUDE;
        // Make sure there's a buckconfig
        assert!(c.files.iter().any(|file| {
            file.path == ".buckconfig"
                && std::str::from_utf8(file.contents)
                    .unwrap()
                    .contains("prelude = .")
        }));

        // And that there's at least 50 files with a reasonable amount of data
        assert!(
            c.files
                .iter()
                .filter(|file| file.contents.len() > 100)
                .count()
                > 50
        );
    }
}
