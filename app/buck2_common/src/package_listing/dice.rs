/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::package::PackageLabel;
use buck2_events::dispatch::async_record_root_spans;
use buck2_events::span::SpanId;
use buck2_util::time_span::TimeSpan;
use dice::DiceComputations;
use dice::Key;
use dice_futures::cancellation::CancellationContext;
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
    pub time_span: TimeSpan,
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
        let now = TimeSpan::start_now();

        let (result, spans) = async_record_root_spans(
            InterpreterPackageListingResolver::new(ctx).resolve(self.0.dupe()),
        )
        .await;

        ctx.store_evaluation_data(PackageListingKeyActivationData {
            time_span: now.end_now(),
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
impl PackageListingResolver for DicePackageListingResolver<'_, '_> {
    async fn resolve(&mut self, package: PackageLabel) -> buck2_error::Result<PackageListing> {
        self.0.compute(&PackageListingKey(package)).await?
    }

    async fn get_enclosing_package(
        &mut self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<PackageLabel> {
        InterpreterPackageListingResolver::new(self.0)
            .get_enclosing_package(path)
            .await
    }

    async fn get_enclosing_packages(
        &mut self,
        path: CellPathRef<'async_trait>,
        enclosing_violation_path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<Vec<PackageLabel>> {
        InterpreterPackageListingResolver::new(self.0)
            .get_enclosing_packages(path, enclosing_violation_path)
            .await
    }
}

impl DicePackageListingResolver<'_, '_> {
    pub async fn resolve_package_listing(
        &mut self,
        package: PackageLabel,
    ) -> buck2_error::Result<PackageListing> {
        self.resolve(package).await
    }
}
