/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context;
use buck2_common::file_ops::{FileOps, SimpleDirEntry};
use buck2_core::{
    cells::{paths::CellPath, CellResolver},
    fs::paths::{FileNameBuf, ForwardRelativePath},
    package::{Package, PackageRelativePathBuf},
    result::SharedResult,
};
use futures::{stream::FuturesUnordered, StreamExt};
use thiserror::Error;

use crate::package_listing::{
    find_buildfile::find_buildfile, listing::PackageListing, resolver::PackageListingResolver,
    sorted_index_set::SortedIndexSet,
};

#[derive(Debug, Error)]
enum PackageListingError {
    #[error("Expected `{0}` to be a package directory, but there was no buildfile there, expected one of `{}`", .1.join("`, `"))]
    NoBuildFile(CellPath, Vec<FileNameBuf>),
    #[error("Expected `{0}` to be within a package directory, but there was no buildfile in any parent directories. Expected one of `{}`", .1.join("`, `"))]
    NoContainingPackage(CellPath, Vec<FileNameBuf>),
}

#[async_trait]
impl<'c> PackageListingResolver for InterpreterPackageListingResolver<'c> {
    async fn resolve(&self, package: &Package) -> SharedResult<PackageListing> {
        Ok(self
            .gather_package_listing(package)
            .await
            .with_context(|| format!("when gathering package listing for `{}`", package))?)
    }

    async fn get_enclosing_package(&self, path: &CellPath) -> anyhow::Result<Package> {
        let path = path.clone();
        let cell_instance = self.cell_resolver.get(path.cell())?;
        let buildfile_candidates = cell_instance.buildfiles();
        if let Some(path) = path.parent() {
            for path in path.ancestors() {
                let listing = self.fs.read_dir(&path).await?;
                if find_buildfile(buildfile_candidates, &listing).is_some() {
                    return Ok(Package::from_cell_path(&path));
                }
            }
        }
        Err(PackageListingError::NoContainingPackage(
            path.to_owned(),
            buildfile_candidates.to_vec(),
        )
        .into())
    }

    async fn get_enclosing_packages(&self, path: &CellPath) -> anyhow::Result<Vec<Package>> {
        let path = path.clone();
        let cell_instance = self.cell_resolver.get(path.cell())?;
        let buildfile_candidates = cell_instance.buildfiles();
        if let Some(path) = path.parent() {
            let mut packages = Vec::new();
            for path in path.ancestors() {
                let listing = self.fs.read_dir(&path).await?;
                if find_buildfile(buildfile_candidates, &listing).is_some() {
                    packages.push(Package::from_cell_path(&path));
                }
            }
            Ok(packages)
        } else {
            Err(PackageListingError::NoContainingPackage(
                path.to_owned(),
                buildfile_candidates.to_vec(),
            )
            .into())
        }
    }
}

pub struct InterpreterPackageListingResolver<'c> {
    cell_resolver: CellResolver,
    fs: Arc<dyn FileOps + 'c>,
}

impl<'c> InterpreterPackageListingResolver<'c> {
    pub fn new(cell_resolver: CellResolver, fs: Arc<dyn FileOps + 'c>) -> Self {
        Self { cell_resolver, fs }
    }

    pub async fn gather_package_listing<'a>(
        &'a self,
        root: &'a Package,
    ) -> anyhow::Result<PackageListing> {
        let cell_instance = self.cell_resolver.get(root.cell_name())?;
        let buildfile_candidates = cell_instance.buildfiles();

        let mut files = Vec::new();
        let mut dirs = Vec::new();

        let root_entries = self.fs.read_dir(root.as_cell_path()).await?;
        let buildfile = find_buildfile(buildfile_candidates, &root_entries).ok_or_else(|| {
            PackageListingError::NoBuildFile(
                root.as_cell_path().clone(),
                buildfile_candidates.to_vec(),
            )
        })?;

        let mut work = FuturesUnordered::new();

        let process_entries = |work: &mut FuturesUnordered<_>,
                               files: &mut Vec<_>,
                               dirs: &mut Vec<_>,
                               path: &CellPath,
                               entries: &[SimpleDirEntry]|
         -> anyhow::Result<()> {
            for d in entries {
                let child_path = path.join_unnormalized(ForwardRelativePath::new(&d.file_name)?);
                if d.file_type.is_dir() {
                    dirs.push(child_path.clone());
                    work.push(async move {
                        let entries = self.fs.read_dir(&child_path).await;
                        (child_path, entries)
                    });
                } else {
                    files.push(child_path);
                }
            }
            Ok(())
        };

        process_entries(
            &mut work,
            &mut files,
            &mut dirs,
            root.as_cell_path(),
            &*root_entries,
        )?;

        while let Some((path, entries_result)) = work.next().await {
            let entries = entries_result?;
            if find_buildfile(buildfile_candidates, &*entries).is_none() {
                process_entries(&mut work, &mut files, &mut dirs, &path, &*entries)?;
            }
        }

        // The files are discovered in a non-deterministic order so we need to fix
        // that here. Sorting files here is easier than after converting them to package relative.
        files.sort();
        dirs.sort();

        let prepare = |xs: &[CellPath]| {
            xs.iter()
                .map(|cell_path| {
                    Ok(PackageRelativePathBuf::from(
                        cell_path.strip_prefix(root.as_cell_path())?.to_buf(),
                    ))
                })
                .collect::<anyhow::Result<_>>()
        };

        Ok(PackageListing::new(
            SortedIndexSet::new_unchecked(prepare(&files)?),
            prepare(&dirs)?,
            buildfile.to_owned(),
        ))
    }
}
