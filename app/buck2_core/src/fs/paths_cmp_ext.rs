/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_fs::paths::cmp_impls::impl_cmp;
use buck2_fs::paths::cmp_impls::impl_cmp_str;
use buck2_fs::paths::file_name::FileName;

use crate::cells::paths::CellRelativePath;
use crate::fs::project_rel_path::ProjectRelativePath;
use crate::fs::project_rel_path::ProjectRelativePathBuf;
use crate::package::package_relative_path::PackageRelativePath;
use crate::package::package_relative_path::PackageRelativePathBuf;

impl AsRef<PackageRelativePath> for FileName {
    #[inline]
    fn as_ref(&self) -> &PackageRelativePath {
        PackageRelativePath::unchecked_new(self.as_str())
    }
}

impl AsRef<CellRelativePath> for FileName {
    #[inline]
    fn as_ref(&self) -> &CellRelativePath {
        CellRelativePath::unchecked_new(self.as_str())
    }
}

impl_cmp!(
    PackageRelativePathBuf,
    PackageRelativePath,
    PackageRelativePath
);
impl_cmp!(
    PackageRelativePathBuf,
    &'_ PackageRelativePath,
    PackageRelativePath
);

impl_cmp_str!(PackageRelativePathBuf, str, PackageRelativePath);
impl_cmp_str!(PackageRelativePathBuf, &'_ str, PackageRelativePath);
impl_cmp_str!(PackageRelativePathBuf, String, PackageRelativePath);
impl_cmp_str!(PackageRelativePath, str, PackageRelativePath);
impl_cmp_str!(PackageRelativePath, &'_ str, PackageRelativePath);
impl_cmp_str!(PackageRelativePath, String, PackageRelativePath);
impl_cmp_str!(&'_ PackageRelativePath, str, PackageRelativePath);
impl_cmp_str!(&'_ PackageRelativePath, String, PackageRelativePath);

// ===== ProjectRelativePath comparisons =====

impl_cmp!(
    ProjectRelativePathBuf,
    ProjectRelativePath,
    ProjectRelativePath
);
impl_cmp!(
    ProjectRelativePathBuf,
    &'_ ProjectRelativePath,
    ProjectRelativePath
);

impl_cmp_str!(ProjectRelativePathBuf, str, ProjectRelativePath);
impl_cmp_str!(ProjectRelativePathBuf, &'_ str, ProjectRelativePath);
impl_cmp_str!(ProjectRelativePathBuf, String, ProjectRelativePath);
impl_cmp_str!(ProjectRelativePath, str, ProjectRelativePath);
impl_cmp_str!(ProjectRelativePath, &'_ str, ProjectRelativePath);
impl_cmp_str!(ProjectRelativePath, String, ProjectRelativePath);
impl_cmp_str!(&'_ ProjectRelativePath, str, ProjectRelativePath);
impl_cmp_str!(&'_ ProjectRelativePath, String, ProjectRelativePath);
