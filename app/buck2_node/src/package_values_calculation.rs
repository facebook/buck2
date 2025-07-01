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
use buck2_core::package::PackageLabel;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use starlark_map::small_map::SmallMap;

use crate::metadata::key::MetadataKey;

#[async_trait]
pub trait PackageValuesCalculation: Send + Sync + 'static {
    async fn package_values(
        &self,
        ctx: &mut DiceComputations<'_>,
        package: PackageLabel,
    ) -> buck2_error::Result<SmallMap<MetadataKey, serde_json::Value>>;
}

pub static PACKAGE_VALUES_CALCULATION: LateBinding<&'static dyn PackageValuesCalculation> =
    LateBinding::new("PACKAGE_VALUES_CALCULATION");
