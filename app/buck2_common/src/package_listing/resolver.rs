/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::package::PackageLabel;

use crate::package_listing::listing::PackageListing;

#[async_trait]
pub trait PackageListingResolver: Send + Sync {
    async fn resolve(&mut self, package: PackageLabel) -> buck2_error::Result<PackageListing>;

    async fn get_enclosing_package(
        &mut self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<PackageLabel>;

    async fn get_enclosing_packages(
        &mut self,
        path: CellPathRef<'async_trait>,
        enclosing_path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<Vec<PackageLabel>>;
}
