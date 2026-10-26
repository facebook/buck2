/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::package::PackageLabel;

use crate::package_listing::listing::PackageListing;

#[async_trait]
pub trait PackageListingResolver: Send + Sync {
    async fn resolve(&mut self, package: PackageLabel) -> buck2_error::Result<PackageListing>;

    async fn get_enclosing_package(&mut self, path: CellPath) -> buck2_error::Result<PackageLabel>;

    async fn get_enclosing_packages(
        &mut self,
        path: CellPath,
        enclosing_path: CellPath,
    ) -> buck2_error::Result<Vec<PackageLabel>>;
}
