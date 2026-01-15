/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_util::arc_str::ArcS;
use dupe::Dupe;
use serde::Deserialize;
use serde::Serialize;
use starlark_map::sorted_set::SortedSet;

use crate::package_listing::binary_search::binary_search_by;

#[derive(Eq, PartialEq, Debug, Allocative, Serialize, Deserialize)]
pub struct PackageFileListing {
    /// This is kept sorted for efficient prefix matching.
    pub(crate) files: SortedSet<ArcS<PackageRelativePath>>,
}

impl PackageFileListing {
    pub fn files(&self) -> impl ExactSizeIterator<Item = &PackageRelativePath> {
        self.files.iter().map(|x| &**x)
    }

    pub(crate) fn files_within(
        &self,
        prefix: &PackageRelativePath,
    ) -> impl Iterator<Item = &ArcS<PackageRelativePath>> + use<'_> {
        let len = prefix.as_str().len();
        self.files_with_prefix(prefix.as_str()).filter(move |x| {
            // Same logic as PackageRelativePath.starts_with,
            // but avoid rechecking that `x` starts with `prefix`, since we already know that.
            len == 0 || len == x.as_str().len() || x.as_str().as_bytes()[len] == b'/'
        })
    }

    pub fn files_with_prefix(
        &self,
        prefix: &str,
    ) -> impl Iterator<Item = &ArcS<PackageRelativePath>> + use<'_> {
        use std::cmp::Ordering;
        let files = &self.files;
        let len = files.len();
        let (Ok(lower) | Err(lower)) = binary_search_by(len, |idx: usize| -> Ordering {
            let x = files.get_index(idx).unwrap().as_str();
            if x.starts_with(prefix) {
                Ordering::Greater
            } else {
                x.cmp(prefix)
            }
        });
        let (Ok(upper) | Err(upper)) = binary_search_by(len, |idx: usize| -> Ordering {
            let x = files.get_index(idx).unwrap().as_str();
            if x.starts_with(prefix) {
                Ordering::Less
            } else {
                x.cmp(prefix)
            }
        });

        (lower..upper).map(|idx: usize| files.get_index(idx).unwrap())
    }

    pub fn get_file(&self, file: &PackageRelativePath) -> Option<ArcS<PackageRelativePath>> {
        if let Some(file) = self.files.get(file) {
            return Some(file.dupe());
        }

        // We don't have the file directly, but we might have a symlink file that covers this file
        // so check for that. Would be much nicer if we didn't have random symlinks.
        // These random symlinks are NOT checked for changes etc, so are pretty dangerous.
        // The config `project.read_only_paths` is the allow-list of where such symlinks can exist in v1.
        let mut current = file;
        while let Some(x) = current.parent() {
            current = x;
            if self.files.get(current).is_some() {
                return Some(file.to_arc());
            }
        }
        None
    }
}

pub mod testing {
    use buck2_core::package::package_relative_path::PackageRelativePath;
    use starlark_map::sorted_set::SortedSet;

    use crate::package_listing::file_listing::PackageFileListing;

    impl PackageFileListing {
        pub fn testing_new(files: &[&str]) -> PackageFileListing {
            let files = files
                .iter()
                .map(|f| PackageRelativePath::new(*f).unwrap().to_arc());
            PackageFileListing {
                files: SortedSet::from_iter(files),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_listing_with_prefix() {
        let listing = PackageFileListing::testing_new(&["a/1", "a/2", "b/1", "b/2", "c/1", "c/2"]);
        assert_eq!(
            listing
                .files_with_prefix("a")
                .map(|p| p.as_str())
                .collect::<Vec<_>>(),
            vec!["a/1", "a/2"]
        );
        assert_eq!(
            listing
                .files_with_prefix("b")
                .map(|p| p.as_str())
                .collect::<Vec<_>>(),
            vec!["b/1", "b/2"]
        );
        assert_eq!(
            listing
                .files_with_prefix("c")
                .map(|p| p.as_str())
                .collect::<Vec<_>>(),
            vec!["c/1", "c/2"]
        );

        let listing = PackageFileListing::testing_new(&["a/1", "a/2", "a/3"]);
        assert_eq!(
            listing
                .files_with_prefix("a")
                .map(|p| p.as_str())
                .collect::<Vec<_>>(),
            vec!["a/1", "a/2", "a/3"]
        );

        assert_eq!(
            listing
                .files_with_prefix("a/1")
                .map(|p| p.as_str())
                .collect::<Vec<_>>(),
            vec!["a/1"],
        );

        assert_eq!(0, listing.files_with_prefix("d").count());
    }

    #[test]
    fn test_listing_within() {
        let listing = PackageFileListing::testing_new(&["a/1", "a/1/2", "aa/2", "b/1"]);
        assert_eq!(
            listing
                .files_within(PackageRelativePath::new("aa").unwrap())
                .map(|p| p.as_str())
                .collect::<Vec<_>>(),
            vec!["aa/2"]
        );
        assert_eq!(
            listing
                .files_within(PackageRelativePath::new("a").unwrap())
                .map(|p| p.as_str())
                .collect::<Vec<_>>(),
            vec!["a/1", "a/1/2"]
        );
        assert_eq!(
            listing
                .files_within(PackageRelativePath::new("a/1").unwrap())
                .map(|p| p.as_str())
                .collect::<Vec<_>>(),
            vec!["a/1", "a/1/2"]
        );
        assert_eq!(
            listing
                .files_within(PackageRelativePath::new("b/1").unwrap())
                .map(|p| p.as_str())
                .collect::<Vec<_>>(),
            vec!["b/1"]
        );
        assert_eq!(
            listing
                .files_within(PackageRelativePath::new("").unwrap())
                .map(|p| p.as_str())
                .collect::<Vec<_>>(),
            vec!["a/1", "a/1/2", "aa/2", "b/1"]
        );
    }
}
