/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use buck2_common::dice::cells::SetCellResolver;
use buck2_common::dice::data::SetIoProvider;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::dice::SetLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_common::tenting::SetTentingAclProvider;
use buck2_common::tenting::TentingAclProvider;
use buck2_core::rollout_percentage::RolloutPercentage;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::SetDigestConfig;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceStorage;
use dice::PagableStorageBackend;

use crate::actions::execute::dice_data::SetInvalidationTrackingConfig;
use crate::build::detailed_aggregated_metrics::dice::SetDetailedAggregatedMetricsHandle;
use crate::build::detailed_aggregated_metrics::events::DetailedAggregatedMetricsHandle;

/// Utility to configure the dice globals.
/// One place to not forget to initialize something in all places.
pub async fn configure_dice_for_buck(
    io: Arc<dyn IoProvider>,
    digest_config: DigestConfig,
    root_config: Option<&LegacyBuckConfig>,
    detect_cycles: Option<DetectCycles>,
    tenting_acl_provider: Option<Arc<dyn TentingAclProvider>>,
    // Path to open pagable DICE storage at, or `None` to leave paging disabled.
    dice_state_path: Option<&Path>,
    // On-disk backend for pagable storage (`buck2_hydration.pagable_storage_backend`).
    pagable_storage_backend: PagableStorageBackend,
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
    dice.set_tenting_acl_provider(tenting_acl_provider);
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

    // Empty handle; a command enables the tracker lazily if it needs one.
    dice.set_detailed_aggregated_metrics_handle(DetailedAggregatedMetricsHandle::new());

    // Opt-in pagable storage, enabling `Dice::page_out()` to serialize node
    // values to disk (backend chosen by `pagable_storage_backend`) via `buck2
    // debug hydration`. `dice_state_path` is `Some` when
    // `buck2_hydration.enable_paging` is set (its value is the default path,
    // under buck-out). The `BUCK2_DICE_DB_PATH` override (used by benchmarks)
    // takes precedence and picks the path.
    let db_path: Option<PathBuf> = match std::env::var_os("BUCK2_DICE_DB_PATH") {
        Some(path) => Some(PathBuf::from(path)),
        None => dice_state_path.map(Path::to_path_buf),
    };
    if let Some(path) = db_path {
        let backend = pagable_storage_backend.with_env_override().map_err(|e| {
            buck2_error::conversion::from_any_with_tag(e, buck2_error::ErrorTag::Environment)
        })?;
        let storage = DiceStorage::open(&path, backend).map_err(|e| {
            buck2_error::conversion::from_any_with_tag(e, buck2_error::ErrorTag::Environment)
        })?;
        dice.set_pagable_storage(storage);
    }

    let dice = dice.build(detect_cycles);
    let mut dice_ctx = dice.updater();
    dice_ctx.set_none_cell_resolver()?;
    dice_ctx.set_none_legacy_config_external_data()?;
    dice_ctx.commit().await;

    Ok(dice)
}
