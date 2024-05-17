/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::package::package_relative_path::PackageRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_error::BuckErrorContext;
use buck2_util::arc_str::ArcS;
use dice::DiceComputations;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::FutureExt;
use starlark_map::sorted_set::SortedSet;
use starlark_map::sorted_vec::SortedVec;

use crate::dice::file_ops::DiceFileComputations;
use crate::find_buildfile::find_buildfile;
use crate::package_listing::listing::PackageListing;
use crate::package_listing::resolver::PackageListingResolver;

#[derive(Debug, buck2_error::Error)]
enum PackageListingError {
    #[error("Expected `{0}` to be a package directory, but there was no buildfile there, expected one of `{}`", .1.join("`, `"))]
    NoBuildFile(CellPath, Vec<FileNameBuf>),
    #[error("Expected `{0}` to be within a package directory, but there was no buildfile in any parent directories. Expected one of `{}`", .1.join("`, `"))]
    NoContainingPackage(CellPath, Vec<FileNameBuf>),
}

#[async_trait]
impl PackageListingResolver for InterpreterPackageListingResolver<'_, '_> {
    async fn resolve(&mut self, package: PackageLabel) -> buck2_error::Result<PackageListing> {
        Ok(self
            .gather_package_listing(package.dupe())
            .await
            .with_context(|| format!("Error gathering package listing for `{}`", package))?)
    }

    async fn get_enclosing_package(
        &mut self,
        path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<PackageLabel> {
        let buildfile_candidates =
            DiceFileComputations::buildfiles(&mut self.ctx, path.cell()).await?;
        if let Some(path) = path.parent() {
            for path in path.ancestors() {
                let listing = DiceFileComputations::read_dir(self.ctx, path)
                    .await?
                    .included;
                if find_buildfile(&buildfile_candidates, &listing).is_some() {
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
        &mut self,
        path: CellPathRef<'async_trait>,
        enclosing_path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<Vec<PackageLabel>> {
        let buildfile_candidates =
            DiceFileComputations::buildfiles(&mut self.ctx, path.cell()).await?;
        if let Some(path) = path.parent() {
            let mut packages = Vec::new();
            for path in path.ancestors() {
                if !path.starts_with(enclosing_path.dupe()) {
                    // stop when we are no longer within the enclosing path
                    break;
                }
                let listing = DiceFileComputations::read_dir(self.ctx, path.dupe())
                    .await?
                    .included;
                if find_buildfile(&buildfile_candidates, &listing).is_some() {
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

pub struct InterpreterPackageListingResolver<'c, 'd> {
    ctx: &'c mut DiceComputations<'d>,
}

impl<'c, 'd> InterpreterPackageListingResolver<'c, 'd> {
    pub fn new(ctx: &'c mut DiceComputations<'d>) -> Self {
        Self { ctx }
    }

    pub async fn gather_package_listing<'a>(
        &mut self,
        root: PackageLabel,
    ) -> anyhow::Result<PackageListing> {
        gather_package_listing_impl(self.ctx, root).await
    }
}

struct Directory {
    path: ArcS<PackageRelativePath>,
    files: Vec<ArcS<PackageRelativePath>>,
    subdirs: Vec<Directory>,
    subpackages: Vec<ArcS<PackageRelativePath>>,
    buildfile: Option<FileNameBuf>,

    recursive_files_count: usize,
    recursive_dirs_count: usize,
    recursive_subpackages_count: usize,
}

impl Directory {
    // Ok(None) indicates that the path is a subpackage
    async fn gather(
        ctx: &mut DiceComputations<'_>,
        buildfile_candidates: &[FileNameBuf],
        root: CellPathRef<'_>,
        path: &PackageRelativePath,
        is_root: bool,
    ) -> anyhow::Result<Option<Directory>> {
        let cell_path = root.join(path.as_forward_rel_path());
        let entries = DiceFileComputations::read_dir(ctx, cell_path.as_ref())
            .await
            .input()?
            .included;

        let buildfile = find_buildfile(buildfile_candidates, &entries);

        match (is_root, buildfile) {
            (true, None) => {
                return Err(PackageListingError::NoBuildFile(
                    cell_path.to_owned(),
                    buildfile_candidates.to_vec(),
                ))
                .input();
            }
            (false, Some(_)) => {
                return Ok(None);
            }
            _ => {}
        }

        let mut subdirs = Vec::new();
        let mut files = Vec::new();

        for d in &*entries {
            let child_path = path.join(&d.file_name);
            if d.file_type.is_dir() {
                subdirs.push(child_path);
            } else {
                files.push(child_path.to_arc());
            }
        }

        let (subdirs, subpackages) =
            Self::gather_subdirs(ctx, buildfile_candidates, root, subdirs).await?;

        let mut recursive_files_count = files.len();
        let mut recursive_dirs_count = subdirs.len();
        let mut recursive_subpackages_count = subpackages.len();
        for d in &subdirs {
            recursive_files_count += d.recursive_files_count;
            recursive_dirs_count += d.recursive_dirs_count;
            recursive_subpackages_count += d.recursive_subpackages_count;
        }

        Ok(Some(Directory {
            path: path.to_arc(),
            files,
            subdirs,
            subpackages,
            buildfile: buildfile.map(|v| v.to_owned()),
            recursive_files_count,
            recursive_dirs_count,
            recursive_subpackages_count,
        }))
    }

    fn gather_subdirs<'a, 'd>(
        ctx: &'a mut DiceComputations<'d>,
        buildfile_candidates: &'a [FileNameBuf],
        root: CellPathRef<'a>,
        subdirs: Vec<PackageRelativePathBuf>,
    ) -> BoxFuture<'a, anyhow::Result<(Vec<Directory>, Vec<ArcS<PackageRelativePath>>)>> {
        async move {
            let mut new_subdirs = Vec::new();
            let mut subpackages = Vec::new();

            for res in ctx
                .compute_join(subdirs, |ctx, path| {
                    async move {
                        let res = Directory::gather(ctx, buildfile_candidates, root, &path, false)
                            .await?;
                        anyhow::Ok((path, res))
                    }
                    .boxed()
                })
                .await
            {
                let (path, res) = res?;
                match res {
                    Some(v) => new_subdirs.push(v),
                    None => subpackages.push(path.to_arc()),
                }
            }
            Ok((new_subdirs, subpackages))
        }
        .boxed()
    }

    fn collect_into(
        self,
        files: &mut Vec<ArcS<PackageRelativePath>>,
        dirs: &mut Vec<ArcS<PackageRelativePath>>,
        pkgs: &mut Vec<ArcS<PackageRelativePath>>,
    ) {
        files.extend(self.files);
        pkgs.extend(self.subpackages);
        if !self.path.is_empty() {
            dirs.push(self.path);
        }
        for d in self.subdirs {
            d.collect_into(files, dirs, pkgs)
        }
    }

    fn flatten(mut self) -> PackageListing {
        let buildfile = self.buildfile.take().unwrap();
        let mut files = Vec::with_capacity(self.recursive_files_count);
        let mut dirs = Vec::with_capacity(self.recursive_dirs_count);
        let mut subpackages = Vec::with_capacity(self.recursive_subpackages_count);

        self.collect_into(&mut files, &mut dirs, &mut subpackages);

        // The files are discovered in a deterministic order but not necessarily sorted.
        // TODO(cjhopman): Do we require that they be sorted for anything?
        let files = SortedVec::from(files);
        let dirs = SortedVec::from(dirs);
        let subpackages = SortedVec::from(subpackages);

        PackageListing::new(
            SortedSet::from(files),
            SortedSet::from(dirs),
            subpackages,
            buildfile,
        )
    }
}

async fn gather_package_listing_impl(
    ctx: &mut DiceComputations<'_>,
    root: PackageLabel,
) -> anyhow::Result<PackageListing> {
    let buildfile_candidates = DiceFileComputations::buildfiles(ctx, root.cell_name()).await?;
    Ok(Directory::gather(
        ctx,
        &buildfile_candidates,
        root.as_cell_path(),
        PackageRelativePath::empty(),
        true,
    )
    .await?
    .unwrap()
    .flatten())
}
