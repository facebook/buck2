/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::package::PackageLabel;
use buck2_error::shared_result::SharedResult;
use buck2_error::shared_result::ToUnsharedResultExt;
use buck2_events::dispatch::async_record_root_spans;
use buck2_events::span::SpanId;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use more_futures::cancellation::CancellationContext;
use smallvec::SmallVec;

use crate::dice::cells::HasCellResolver;
use crate::dice::file_ops::HasFileOps;
use crate::package_listing::interpreter::InterpreterPackageListingResolver;
use crate::package_listing::listing::PackageListing;
use crate::package_listing::resolver::PackageListingResolver;

#[async_trait]
pub trait HasPackageListingResolver<'c> {
    type PL: PackageListingResolver + 'c;
    fn get_package_listing_resolver(&'c self) -> Self::PL;
    async fn resolve_package_listing(
        &self,
        package: PackageLabel,
    ) -> anyhow::Result<PackageListing>;
}

#[async_trait]
impl<'c> HasPackageListingResolver<'c> for DiceComputations {
    type PL = DicePackageListingResolver<'c>;
    fn get_package_listing_resolver(&'c self) -> Self::PL {
        DicePackageListingResolver(self)
    }

    async fn resolve_package_listing(
        &self,
        package: PackageLabel,
    ) -> anyhow::Result<PackageListing> {
        self.get_package_listing_resolver()
            .resolve(package)
            .await
            .unshared_error()
    }
}

#[derive(
    Clone,
    Dupe,
    derive_more::Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Allocative
)]
pub struct PackageListingKey(pub PackageLabel);

pub struct PackageListingKeyActivationData {
    pub duration: Duration,
    pub spans: SmallVec<[SpanId; 1]>,
}

#[async_trait]
impl Key for PackageListingKey {
    type Value = SharedResult<PackageListing>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let now = Instant::now();

        let cell_resolver = ctx.get_cell_resolver().await?;
        let file_ops = ctx.file_ops();
        let (result, spans) = async_record_root_spans(
            InterpreterPackageListingResolver::new(cell_resolver, Arc::new(file_ops))
                .resolve(self.0.dupe()),
        )
        .await;

        ctx.store_evaluation_data(PackageListingKeyActivationData {
            duration: now.elapsed(),
            spans,
        })?;

        result
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

#[derive(Clone, Dupe)]
pub struct DicePackageListingResolver<'compute>(&'compute DiceComputations);

#[async_trait]
impl<'c> PackageListingResolver for DicePackageListingResolver<'c> {
    async fn resolve(&self, package: PackageLabel) -> SharedResult<PackageListing> {
        self.0.compute(&PackageListingKey(package.dupe())).await?
    }

    async fn get_enclosing_package(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<PackageLabel> {
        let cell_resolver = self.0.get_cell_resolver().await?;
        let file_ops = self.0.file_ops();
        InterpreterPackageListingResolver::new(cell_resolver, Arc::new(file_ops))
            .get_enclosing_package(path)
            .await
    }

    async fn get_enclosing_packages(
        &self,
        path: CellPathRef<'async_trait>,
        enclosing_violation_path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<Vec<PackageLabel>> {
        let cell_resolver = self.0.get_cell_resolver().await?;
        let file_ops = self.0.file_ops();
        InterpreterPackageListingResolver::new(cell_resolver, Arc::new(file_ops))
            .get_enclosing_packages(path, enclosing_violation_path)
            .await
    }
}
