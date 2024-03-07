/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::package::PackageLabel;
use buck2_events::dispatch::async_record_root_spans;
use buck2_events::span::SpanId;
use buck2_futures::cancellation::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use smallvec::SmallVec;

use crate::package_listing::interpreter::InterpreterPackageListingResolver;
use crate::package_listing::listing::PackageListing;
use crate::package_listing::resolver::PackageListingResolver;

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
    type Value = buck2_error::Result<PackageListing>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let now = Instant::now();

        let (result, spans) = async_record_root_spans(
            InterpreterPackageListingResolver::new(ctx).resolve(self.0.dupe()),
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

pub struct DicePackageListingResolver<'compute, 'dice>(pub &'compute mut DiceComputations<'dice>);

#[async_trait]
impl<'c, 'd> PackageListingResolver for DicePackageListingResolver<'c, 'd> {
    async fn resolve(&mut self, package: PackageLabel) -> buck2_error::Result<PackageListing> {
        self.0.compute(&PackageListingKey(package.dupe())).await?
    }

    async fn get_enclosing_package(
        &mut self,
        path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<PackageLabel> {
        InterpreterPackageListingResolver::new(self.0)
            .get_enclosing_package(path)
            .await
    }

    async fn get_enclosing_packages(
        &mut self,
        path: CellPathRef<'async_trait>,
        enclosing_violation_path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<Vec<PackageLabel>> {
        InterpreterPackageListingResolver::new(self.0)
            .get_enclosing_packages(path, enclosing_violation_path)
            .await
    }
}

impl DicePackageListingResolver<'_, '_> {
    pub async fn resolve_package_listing(
        &mut self,
        package: PackageLabel,
    ) -> anyhow::Result<PackageListing> {
        self.resolve(package).await.map_err(anyhow::Error::from)
    }
}
