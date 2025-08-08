/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_common::dice::cells::SetCellResolver;
use buck2_common::dice::data::SetIoProvider;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::dice::SetLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::rollout_percentage::RolloutPercentage;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::SetDigestConfig;
use dice::DetectCycles;
use dice::Dice;

use crate::actions::execute::dice_data::SetInvalidationTrackingConfig;
use crate::build::detailed_aggregated_metrics::dice::SetDetailedAggregatedMetricsEventHandler;
use crate::build::detailed_aggregated_metrics::events::start_detailed_aggregated_metrics_state_tracker;

/// Utility to configure the dice globals.
/// One place to not forget to initialize something in all places.
pub async fn configure_dice_for_buck(
    io: Arc<dyn IoProvider>,
    digest_config: DigestConfig,
    root_config: Option<&LegacyBuckConfig>,
    detect_cycles: Option<DetectCycles>,
) -> buck2_error::Result<Arc<Dice>> {
    let detect_cycles = detect_cycles.map_or_else(
        || {
            root_config
                .and_then(|c| {
                    c.parse::<DetectCycles>(BuckconfigKeyRef {
                        section: "buck2",
                        property: "detect_cycles",
                    })
                    .transpose()
                })
                .unwrap_or(Ok(DetectCycles::Enabled))
        },
        Ok,
    )?;

    let mut dice = Dice::builder();
    dice.set_io_provider(io);
    dice.set_digest_config(digest_config);
    let invalidation_tracking_enabled = match root_config {
        Some(c) => c
            .parse::<RolloutPercentage>(BuckconfigKeyRef {
                section: "buck2",
                property: "invalidation_tracking_enabled",
            })?
            .is_some_and(|v| v.roll()),
        None => false,
    };
    dice.set_invalidation_tracking_config(invalidation_tracking_enabled);

    dice.set_detailed_aggregated_metrics_event_handler(Some(
        start_detailed_aggregated_metrics_state_tracker(),
    ));

    let dice = dice.build(detect_cycles);
    let mut dice_ctx = dice.updater();
    dice_ctx.set_none_cell_resolver()?;
    dice_ctx.set_none_legacy_config_external_data()?;
    dice_ctx.commit().await;

    Ok(dice)
}
