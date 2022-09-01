/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::package::Package;

use crate::package_listing::listing::PackageListing;
use crate::result::SharedResult;

#[async_trait]
pub trait PackageListingResolver: Send + Sync {
    async fn resolve(&self, package: &Package) -> SharedResult<PackageListing>;

    async fn get_enclosing_package(&self, path: &CellPath) -> anyhow::Result<Package>;

    async fn get_enclosing_packages(
        &self,
        path: &CellPath,
        enclosing_path: &CellPath,
    ) -> anyhow::Result<Vec<Package>>;
}
