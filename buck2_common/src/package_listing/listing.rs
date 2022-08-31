/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::fs::paths::FileName;
use buck2_core::fs::paths::FileNameBuf;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::package::package_relative_path::PackageRelativePathBuf;
use gazebo::dupe::Dupe;
use indexmap::IndexSet;

use crate::package_listing::file_listing::PackageFileListing;
use crate::sorted_index_set::SortedIndexSet;

#[derive(Clone, Dupe, Eq, PartialEq, Debug)]
pub struct PackageListing {
    listing: Arc<PackageListingData>,
}

#[derive(Eq, PartialEq, Debug)]
struct PackageListingData {
    files: PackageFileListing,
    directories: IndexSet<PackageRelativePathBuf>,
    subpackages: Vec<PackageRelativePathBuf>,
    buildfile: FileNameBuf,
}

impl PackageListing {
    pub(crate) fn new(
        files: SortedIndexSet<PackageRelativePathBuf>,
        directories: IndexSet<PackageRelativePathBuf>,
        subpackages: Vec<PackageRelativePathBuf>,
        buildfile: FileNameBuf,
    ) -> Self {
        Self {
            listing: Arc::new(PackageListingData {
                files: PackageFileListing { files },
                directories,
                subpackages,
                buildfile,
            }),
        }
    }

    pub fn empty(buildfile: FileNameBuf) -> Self {
        Self::new(
            SortedIndexSet::empty(),
            IndexSet::new(),
            Vec::new(),
            buildfile,
        )
    }

    pub fn files(&self) -> &PackageFileListing {
        &self.listing.files
    }

    pub fn contains_file(&self, file: &PackageRelativePath) -> bool {
        self.listing.files.contains_file(file)
    }

    pub fn contains_dir(&self, dir: &PackageRelativePath) -> bool {
        // Empty paths must refer to a directory, since the whole thing is rooted
        // at a directory. But empty paths are not explicitly added to the `directories` variable,
        // so handle them specially.
        dir.as_str().is_empty() || self.listing.directories.contains(dir)
    }

    pub fn files_within<'a>(
        &'a self,
        dir: &PackageRelativePath,
    ) -> impl Iterator<Item = &'a PackageRelativePath> {
        self.listing.files.files_within(dir)
    }

    pub fn subpackages_within<'a>(
        &'a self,
        dir: &'a PackageRelativePath,
    ) -> impl Iterator<Item = &'a PackageRelativePath> + 'a {
        self.listing
            .subpackages
            .iter()
            .map(|x| x.as_ref())
            .filter(move |x: &&PackageRelativePath| x.starts_with(dir))
    }

    pub fn buildfile(&self) -> &FileName {
        &self.listing.buildfile
    }
}

pub mod testing {
    use buck2_core::fs::paths::FileNameBuf;
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    use indexmap::IndexSet;

    use crate::package_listing::listing::PackageListing;
    use crate::sorted_index_set::SortedIndexSet;

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
                Vec::new(),
                FileNameBuf::unchecked_new(buildfile.to_owned()),
            )
        }
    }
}
