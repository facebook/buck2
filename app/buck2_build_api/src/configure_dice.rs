/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::dice::cells::SetCellResolver;
use buck2_common::dice::data::SetIoProvider;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::dice::SetLegacyConfigs;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::SetDigestConfig;
use dice::DetectCycles;
use dice::Dice;
use dice::WhichDice;
use dice::WhichSpawner;

/// Utility to configure the dice globals.
/// One place to not forget to initialize something in all places.
pub async fn configure_dice_for_buck(
    io: Arc<dyn IoProvider>,
    digest_config: DigestConfig,
    root_config: Option<&LegacyBuckConfig>,
    detect_cycles: Option<DetectCycles>,
    which_dice: Option<WhichDice>,
) -> anyhow::Result<Arc<Dice>> {
    let detect_cycles = detect_cycles.map_or_else(
        || {
            root_config
                .and_then(|c| {
                    c.parse::<DetectCycles>("buck2", "detect_cycles")
                        .transpose()
                })
                .unwrap_or(Ok(DetectCycles::Enabled))
        },
        Ok,
    )?;

    let which_dice = which_dice.map_or_else(
        || {
            root_config
                .and_then(|c| c.parse::<WhichDice>("buck2", "dice").transpose())
                .unwrap_or(Ok(WhichDice::Legacy))
        },
        Ok,
    )?;

    let mut dice = match which_dice {
        WhichDice::Legacy => Dice::builder(),
        WhichDice::Modern => Dice::modern(),
    };
    dice.set_io_provider(io);
    dice.set_digest_config(digest_config);

    let dice = dice.build_with_which_spawner(detect_cycles, WhichSpawner::DropCancel);
    let mut dice_ctx = dice.updater();
    dice_ctx.set_none_cell_resolver()?;
    dice_ctx.set_none_legacy_configs()?;
    dice_ctx.commit().await;

    Ok(dice)
}
