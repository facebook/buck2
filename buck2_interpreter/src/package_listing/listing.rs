/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::{
    fs::paths::{FileName, FileNameBuf},
    package::{PackageRelativePath, PackageRelativePathBuf},
};
use gazebo::dupe::Dupe;
use indexmap::IndexSet;

use crate::{
    extra::binary_search::binary_search_by, package_listing::sorted_index_set::SortedIndexSet,
};

#[derive(Clone, Dupe, Eq, PartialEq, Debug)]
pub struct PackageListing {
    listing: Arc<PackageListingData>,
}

#[derive(Eq, PartialEq, Debug)]
struct PackageListingData {
    /// This is kept sorted for efficient prefix matching.
    files: SortedIndexSet<PackageRelativePathBuf>,
    directories: IndexSet<PackageRelativePathBuf>,
    buildfile: FileNameBuf,
}

impl PackageListing {
    pub(crate) fn new(
        files: SortedIndexSet<PackageRelativePathBuf>,
        directories: IndexSet<PackageRelativePathBuf>,
        buildfile: FileNameBuf,
    ) -> Self {
        Self {
            listing: Arc::new(PackageListingData {
                files,
                directories,
                buildfile,
            }),
        }
    }

    pub fn empty(buildfile: FileNameBuf) -> Self {
        Self::new(SortedIndexSet::empty(), IndexSet::new(), buildfile)
    }

    pub fn files(&self) -> impl ExactSizeIterator<Item = &PackageRelativePathBuf> {
        self.listing.files.as_ref().iter()
    }

    pub fn files_with_prefix(&self, prefix: &str) -> impl Iterator<Item = &PackageRelativePathBuf> {
        use std::cmp::Ordering;
        let files = self.listing.files.as_ref();
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
            .map(|idx: usize| files.get_index(idx).unwrap())
    }

    pub fn files_within<'a>(
        &'a self,
        dir: &'a PackageRelativePath,
    ) -> impl Iterator<Item = &'a PackageRelativePathBuf> + 'a {
        self.files().filter(move |x| x.starts_with(dir))
    }

    pub fn contains_file(&self, mut file: &PackageRelativePath) -> bool {
        if self.listing.files.as_ref().get(file).is_some() {
            return true;
        }
        // We don't have the file directly, but we might have a symlink file that covers this file
        // so check for that. Would be much nicer if we didn't have random symlinks.
        // These random symlinks are NOT checked for changes etc, so are pretty dangerous.
        // The config `project.read_only_paths` is the allow-list of where such symlinks can exist in v1.
        while let Some(x) = file.parent() {
            file = x;
            if self.listing.files.as_ref().get(file).is_some() {
                return true;
            }
        }
        false
    }

    pub fn contains_dir(&self, dir: &PackageRelativePath) -> bool {
        // Empty paths must refer to a directory, since the whole thing is rooted
        // at a directory. But empty paths are not explicitly added to the `directories` variable,
        // so handle them specially.
        dir.as_str().is_empty() || self.listing.directories.contains(dir)
    }

    pub fn buildfile(&self) -> &FileName {
        &self.listing.buildfile
    }
}

pub mod testing {
    use buck2_core::{fs::paths::FileNameBuf, package::PackageRelativePathBuf};
    use indexmap::IndexSet;

    use crate::package_listing::{listing::PackageListing, sorted_index_set::SortedIndexSet};

    pub trait PackageListingExt {
        fn testing_empty() -> Self;
        fn testing_files(files: &[&str]) -> Self;
        fn testing_new(files: &[&str], buildfile: &str) -> Self;
    }

    impl PackageListingExt for PackageListing {
        fn testing_empty() -> Self {
            Self::testing_files(&[])
        }

        fn testing_files(files: &[&str]) -> Self {
            Self::testing_new(files, "BUCK")
        }

        fn testing_new(files: &[&str], buildfile: &str) -> Self {
            let files: IndexSet<_> = files
                .iter()
                .map(|f| PackageRelativePathBuf::unchecked_new((*f).to_owned()))
                .collect();
            PackageListing::new(
                SortedIndexSet::new(files),
                IndexSet::new(),
                FileNameBuf::unchecked_new(buildfile.to_owned()),
            )
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::package_listing::listing::testing::PackageListingExt;

    #[test]
    fn test_listing_with_prefix() {
        let listing = PackageListing::testing_files(&["a/1", "a/2", "b/1", "b/2", "c/1", "c/2"]);
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

        let listing = PackageListing::testing_files(&["a/1", "a/2", "a/3"]);
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
}
