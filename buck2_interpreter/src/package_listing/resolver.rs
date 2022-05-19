/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::{cells::paths::CellPath, package::Package, result::SharedResult};

use crate::package_listing::listing::PackageListing;

#[async_trait]
pub trait PackageListingResolver: Send + Sync {
    async fn resolve(&self, package: &Package) -> SharedResult<PackageListing>;

    async fn get_enclosing_package(&self, path: &CellPath) -> anyhow::Result<Package>;

    async fn get_enclosing_packages(&self, path: &CellPath) -> anyhow::Result<Vec<Package>>;
}
