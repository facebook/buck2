/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::cells::name::CellName;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::target::label::label::TargetLabel;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

use crate::configuration::resolved::ConfigurationSettingKey;
use crate::configuration::resolved::ResolvedConfiguration;

#[async_trait]
pub trait ConfigurationCalculationDyn: Send + Sync + 'static {
    async fn get_platform_configuration(
        &self,
        dice: &mut DiceComputations<'_>,
        target: &TargetLabel,
    ) -> buck2_error::Result<ConfigurationData>;

    async fn get_resolved_configuration(
        &self,
        dice: &mut DiceComputations<'_>,
        target_cfg: &ConfigurationData,
        target_node_cell: CellName,
        configuration_deps: &[ConfigurationSettingKey],
    ) -> buck2_error::Result<ResolvedConfiguration>;
}

pub static CONFIGURATION_CALCULATION: LateBinding<&'static dyn ConfigurationCalculationDyn> =
    LateBinding::new("CONFIGURATION_CALCULATION");
