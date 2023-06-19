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
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::package::PackageLabel;
use buck2_util::arc_str::ArcS;
use buck2_util::collections::sorted_set::SortedSet;
use buck2_util::collections::sorted_vec::SortedVec;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::StreamExt;
use thiserror::Error;

use crate::file_ops::FileOps;
use crate::file_ops::SimpleDirEntry;
use crate::find_buildfile::find_buildfile;
use crate::package_listing::listing::PackageListing;
use crate::package_listing::resolver::PackageListingResolver;
use crate::result::SharedResult;

#[derive(Debug, Error)]
enum PackageListingError {
    #[error("Expected `{0}` to be a package directory, but there was no buildfile there, expected one of `{}`", .1.join("`, `"))]
    NoBuildFile(CellPath, Vec<FileNameBuf>),
    #[error("Expected `{0}` to be within a package directory, but there was no buildfile in any parent directories. Expected one of `{}`", .1.join("`, `"))]
    NoContainingPackage(CellPath, Vec<FileNameBuf>),
}

#[async_trait]
impl<'c> PackageListingResolver for InterpreterPackageListingResolver<'c> {
    async fn resolve(&self, package: PackageLabel) -> SharedResult<PackageListing> {
        Ok(self
            .gather_package_listing(package.dupe())
            .await
            .context(buck2_data::ErrorCause::InvalidPackage)
            .with_context(|| format!("Error gathering package listing for `{}`", package))?)
    }

    async fn get_enclosing_package(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<PackageLabel> {
        let cell_instance = self.cell_resolver.get(path.cell())?;
        let buildfile_candidates = cell_instance.buildfiles();
        if let Some(path) = path.parent() {
            for path in path.ancestors() {
                let listing = self.fs.read_dir(path.dupe()).await?.included;
                if find_buildfile(buildfile_candidates, &listing).is_some() {
                    return Ok(PackageLabel::from_cell_path(path));
                }
            }
        }
        Err(PackageListingError::NoContainingPackage(
            path.to_owned(),
            buildfile_candidates.to_vec(),
        )
        .into())
    }

    async fn get_enclosing_packages(
        &self,
        path: CellPathRef<'async_trait>,
        enclosing_path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<Vec<PackageLabel>> {
        let cell_instance = self.cell_resolver.get(path.cell())?;
        let buildfile_candidates = cell_instance.buildfiles();
        if let Some(path) = path.parent() {
            let mut packages = Vec::new();
            for path in path.ancestors() {
                if !path.starts_with(enclosing_path.dupe()) {
                    // stop when we are no longer within the enclosing path
                    break;
                }
                let listing = self.fs.read_dir(path.dupe()).await?.included;
                if find_buildfile(buildfile_candidates, &listing).is_some() {
                    packages.push(PackageLabel::from_cell_path(path));
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
        root: PackageLabel,
    ) -> anyhow::Result<PackageListing> {
        let cell_instance = self.cell_resolver.get(root.cell_name())?;
        let buildfile_candidates = cell_instance.buildfiles();

        let mut files: Vec<ArcS<PackageRelativePath>> = Vec::new();
        let mut dirs: Vec<ArcS<PackageRelativePath>> = Vec::new();
        let mut subpackages: Vec<ArcS<PackageRelativePath>> = Vec::new();

        let root_entries = self
            .fs
            .read_dir(root.as_cell_path())
            .await
            .context(buck2_data::ErrorCategory::User)?
            .included;
        let buildfile = find_buildfile(buildfile_candidates, &root_entries)
            .ok_or_else(|| {
                PackageListingError::NoBuildFile(
                    root.as_cell_path().to_owned(),
                    buildfile_candidates.to_vec(),
                )
            })
            .context(buck2_data::ErrorCategory::User)?;

        let mut work = FuturesUnordered::new();

        let root = &root;
        let process_entries = |work: &mut FuturesUnordered<_>,
                               files: &mut Vec<ArcS<PackageRelativePath>>,
                               path: &PackageRelativePath,
                               entries: &[SimpleDirEntry]|
         -> anyhow::Result<()> {
            for d in entries {
                let child_path = path.join(&d.file_name).to_arc();
                if d.file_type.is_dir() {
                    work.push(async move {
                        let entries = self
                            .fs
                            .read_dir(
                                root.as_cell_path()
                                    .join(child_path.as_forward_rel_path())
                                    .as_ref(),
                            )
                            .await;
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
            PackageRelativePath::empty(),
            &root_entries,
        )?;

        while let Some((path, entries_result)) = work.next().await {
            let entries = entries_result?.included;
            if find_buildfile(buildfile_candidates, &entries).is_none() {
                process_entries(&mut work, &mut files, path.as_ref(), &entries)?;
                dirs.push(path);
            } else {
                subpackages.push(path);
            }
        }

        // The files are discovered in a non-deterministic order so we need to fix
        // that here.
        let files = SortedVec::from(files);
        let dirs = SortedVec::from(dirs);
        let subpackages = SortedVec::from(subpackages);

        Ok(PackageListing::new(
            SortedSet::from(files),
            SortedSet::from(dirs),
            subpackages,
            buildfile.to_owned(),
        ))
    }
}
