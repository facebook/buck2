/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::dice::{cells::HasCellResolver, file_ops::HasFileOps};
use buck2_core::{package::Package, result::SharedResult};
use dice::{DiceComputations, Key};
use gazebo::dupe::Dupe;

use crate::package_listing::{
    interpreter::InterpreterPackageListingResolver, listing::PackageListing,
    resolver::PackageListingResolver,
};

pub trait HasPackageListingResolver<'c> {
    type PL: PackageListingResolver + 'c;
    fn get_package_listing_resolver(&'c self) -> Self::PL;
}

impl<'c> HasPackageListingResolver<'c> for DiceComputations {
    type PL = DicePackageListingResolver<'c>;
    fn get_package_listing_resolver(&'c self) -> Self::PL {
        DicePackageListingResolver(self)
    }
}

#[derive(Clone, Dupe)]
pub struct DicePackageListingResolver<'compute>(&'compute DiceComputations);

#[async_trait]
impl<'c> PackageListingResolver for DicePackageListingResolver<'c> {
    async fn resolve(&self, package: &Package) -> SharedResult<PackageListing> {
        #[derive(Clone, Dupe, derive_more::Display, Debug, Eq, Hash, PartialEq)]
        #[display(fmt = "PackageListing({})", _0)]
        struct PackageListingKey(Package);

        #[async_trait]
        impl Key for PackageListingKey {
            type Value = SharedResult<PackageListing>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let cell_resolver = ctx.get_cell_resolver().await;
                let file_ops = ctx.file_ops();
                InterpreterPackageListingResolver::new(cell_resolver, Arc::new(file_ops))
                    .resolve(&self.0)
                    .await
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }
        }

        self.0.compute(&PackageListingKey(package.dupe())).await
    }

    async fn get_enclosing_package(
        &self,
        path: &buck2_core::cells::paths::CellPath,
    ) -> anyhow::Result<Package> {
        let cell_resolver = self.0.get_cell_resolver().await;
        let file_ops = self.0.file_ops();
        InterpreterPackageListingResolver::new(cell_resolver, Arc::new(file_ops))
            .get_enclosing_package(path)
            .await
    }

    async fn get_enclosing_packages(
        &self,
        path: &buck2_core::cells::paths::CellPath,
    ) -> anyhow::Result<Vec<Package>> {
        let cell_resolver = self.0.get_cell_resolver().await;
        let file_ops = self.0.file_ops();
        InterpreterPackageListingResolver::new(cell_resolver, Arc::new(file_ops))
            .get_enclosing_packages(path)
            .await
    }
}
