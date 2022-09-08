/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::SetIoProvider;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::LegacyBuckConfig;
use dice::cycles::DetectCycles;
use dice::Dice;

use crate::bxl::calculation::BxlCalculationDyn;

/// Utility to configure the dice globals.
/// One place to not forget to initialize something in all places.
pub fn configure_dice_for_buck(
    io: Arc<dyn IoProvider>,
    bxl: &'static dyn BxlCalculationDyn,
    root_config: Option<&LegacyBuckConfig>,
    detect_cycles: Option<DetectCycles>,
) -> anyhow::Result<Arc<Dice>> {
    let mut dice = Dice::builder();
    dice.set_io_provider(io);
    dice.set(bxl);

    let detect_cycles = detect_cycles.map_or_else(
        || {
            root_config
                .and_then(|c| {
                    c.parse::<DetectCycles>("buck2", "detect_cycles")
                        .transpose()
                })
                .unwrap_or(Ok(DetectCycles::Disabled))
        },
        Ok,
    )?;

    let dice = dice.build(detect_cycles);
    let dice_ctx = dice.ctx();
    dice_ctx.set_none_cell_resolver()?;
    dice_ctx.set_none_legacy_configs()?;
    dice_ctx.commit();

    Ok(dice)
}
