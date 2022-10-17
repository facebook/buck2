/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::collections::sorted_index_set::SortedIndexSet;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::package::package_relative_path::PackageRelativePathBuf;

use crate::package_listing::binary_search::binary_search_by;

#[derive(Eq, PartialEq, Debug)]
pub struct PackageFileListing {
    /// This is kept sorted for efficient prefix matching.
    pub(crate) files: SortedIndexSet<PackageRelativePathBuf>,
}

impl PackageFileListing {
    pub fn files(&self) -> impl ExactSizeIterator<Item = &PackageRelativePath> {
        self.files.iter().map(PackageRelativePathBuf::as_ref)
    }

    pub(crate) fn files_within(
        &self,
        prefix: &PackageRelativePath,
    ) -> impl Iterator<Item = &PackageRelativePath> {
        let len = prefix.as_str().len();
        self.files_with_prefix(prefix.as_str()).filter(move |x| {
            // Same logic as PackageRelativePath.starts_with,
            // but avoid rechecking that `x` starts with `prefix`, since we already know that.
            len == 0 || len == x.as_str().len() || x.as_str().as_bytes()[len] == b'/'
        })
    }

    pub fn files_with_prefix(&self, prefix: &str) -> impl Iterator<Item = &PackageRelativePath> {
        use std::cmp::Ordering;
        let files = &self.files;
        let len = files.len();
        let lower = binary_search_by(len, |idx: usize| -> Ordering {
            let x = files.get_index(idx).unwrap().as_str();
            if x.starts_with(prefix) {
                Ordering::Greater
            } else {
                x.cmp(prefix)
            }
        });
        let upper = binary_search_by(len, |idx: usize| -> Ordering {
            let x = files.get_index(idx).unwrap().as_str();
            if x.starts_with(prefix) {
                Ordering::Less
            } else {
                x.cmp(prefix)
            }
        });
        (lower.into_ok_or_err()..upper.into_ok_or_err())
            .map(|idx: usize| files.get_index(idx).unwrap().as_ref())
    }

    pub fn contains_file(&self, mut file: &PackageRelativePath) -> bool {
        if self.files.get(file).is_some() {
            return true;
        }
        // We don't have the file directly, but we might have a symlink file that covers this file
        // so check for that. Would be much nicer if we didn't have random symlinks.
        // These random symlinks are NOT checked for changes etc, so are pretty dangerous.
        // The config `project.read_only_paths` is the allow-list of where such symlinks can exist in v1.
        while let Some(x) = file.parent() {
            file = x;
            if self.files.get(file).is_some() {
                return true;
            }
        }
        false
    }
}

pub mod testing {
    use buck2_core::collections::sorted_index_set::SortedIndexSet;
    use buck2_core::package::package_relative_path::PackageRelativePath;
    use indexmap::IndexSet;

    use crate::package_listing::file_listing::PackageFileListing;

    impl PackageFileListing {
        pub fn testing_new(files: &[&str]) -> PackageFileListing {
            let files: IndexSet<_> = files
                .iter()
                .map(|f| PackageRelativePath::new(*f).unwrap().to_owned())
                .collect();
            PackageFileListing {
                files: SortedIndexSet::from(files),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_listing_with_prefix() {
        let listing = PackageFileListing::testing_new(&["a/1", "a/2", "b/1", "b/2", "c/1", "c/2"]);
        assert_eq!(
            listing.files_with_prefix("a").collect::<Vec<_>>(),
            vec!["a/1", "a/2"]
        );
        assert_eq!(
            listing.files_with_prefix("b").collect::<Vec<_>>(),
            vec!["b/1", "b/2"]
        );
        assert_eq!(
            listing.files_with_prefix("c").collect::<Vec<_>>(),
            vec!["c/1", "c/2"]
        );

        let listing = PackageFileListing::testing_new(&["a/1", "a/2", "a/3"]);
        assert_eq!(
            listing.files_with_prefix("a").collect::<Vec<_>>(),
            vec!["a/1", "a/2", "a/3"]
        );

        assert_eq!(
            listing.files_with_prefix("a/1").collect::<Vec<_>>(),
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
                .collect::<Vec<_>>(),
            vec!["aa/2"]
        );
        assert_eq!(
            listing
                .files_within(PackageRelativePath::new("a").unwrap())
                .collect::<Vec<_>>(),
            vec!["a/1", "a/1/2"]
        );
        assert_eq!(
            listing
                .files_within(PackageRelativePath::new("a/1").unwrap())
                .collect::<Vec<_>>(),
            vec!["a/1", "a/1/2"]
        );
        assert_eq!(
            listing
                .files_within(PackageRelativePath::new("b/1").unwrap())
                .collect::<Vec<_>>(),
            vec!["b/1"]
        );
        assert_eq!(
            listing
                .files_within(PackageRelativePath::new("").unwrap())
                .collect::<Vec<_>>(),
            vec!["a/1", "a/1/2", "aa/2", "b/1"]
        );
    }
}
