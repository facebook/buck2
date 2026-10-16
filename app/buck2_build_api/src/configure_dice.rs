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
use buck2_common::settings::BuckSettings;
use buck2_common::settings::dice::SetBuckSettings;
use buck2_common::tenting::SetTentingAclProvider;
use buck2_common::tenting::TentingAclProvider;
use buck2_core::rollout_percentage::RolloutPercentage;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::SetDigestConfig;
use dice::BranchId;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceData;
use dice::DiceDataBuilder;
use dice::DiceStorage;
use dice::EvictionPolicy;
use dice::PagableStorageBackend;
use dice::PageOutPolicy;
use starlark::pagable::DeferredFieldReadsEnabled;

use crate::actions::execute::dice_data::SetInvalidationTrackingConfig;
use crate::build::detailed_aggregated_metrics::dice::SetDetailedAggregatedMetricsHandle;
use crate::build::detailed_aggregated_metrics::events::DetailedAggregatedMetricsHandle;

/// Repository inputs attached to a DICE root.
pub struct DiceRootConfiguration<'a> {
    pub io: Arc<dyn IoProvider>,
    pub digest_config: DigestConfig,
    pub root_config: Option<&'a LegacyBuckConfig>,
    pub tenting_acl_provider: Option<Arc<dyn TentingAclProvider>>,
    pub buck_settings: BuckSettings,
}

/// Construct the immutable data associated with one repository root tree.
pub fn configure_dice_data_for_buck(
    configuration: DiceRootConfiguration<'_>,
) -> buck2_error::Result<DiceData> {
    let DiceRootConfiguration {
        io,
        digest_config,
        root_config,
        tenting_acl_provider,
        buck_settings,
    } = configuration;
    let mut data = DiceData::new();
    data.set_io_provider(io);
    data.set_digest_config(digest_config);
    data.set_buck_settings(buck_settings);
    data.set_tenting_acl_provider(tenting_acl_provider);
    let invalidation_tracking_enabled = match root_config {
        Some(c) => c
            .parse::<RolloutPercentage>(BuckconfigKeyRef {
                section: "buck2",
                property: "invalidation_tracking_enabled",
            })?
            .is_some_and(|v| v.roll()),
        None => false,
    };
    data.set_invalidation_tracking_config(invalidation_tracking_enabled);

    // Empty handle; a command enables the tracker lazily if it needs one.
    data.set_detailed_aggregated_metrics_handle(DetailedAggregatedMetricsHandle::new());
    Ok(data)
}

/// Configure engine-global DICE facilities around one initial root's data.
pub async fn configure_dice_for_buck(
    io: Arc<dyn IoProvider>,
    digest_config: DigestConfig,
    root_config: Option<&LegacyBuckConfig>,
    tenting_acl_provider: Option<Arc<dyn TentingAclProvider>>,
    buck_settings: BuckSettings,
    // Path to open pagable DICE storage at, or `None` to leave paging disabled.
    dice_state_path: Option<&Path>,
    // On-disk backend for pagable storage (`buck2_hydration.pagable_storage_backend`).
    pagable_storage_backend: PagableStorageBackend,
    // Which values a page-out writes (`hydration.page_out_policy`).
    page_out_policy: PageOutPolicy,
    // When the values a page-out writes leave memory (`hydration.eviction_policy`).
    eviction_policy: EvictionPolicy,
    defer_field_reads: bool,
) -> buck2_error::Result<(Arc<Dice>, BranchId)> {
    let data = configure_dice_data_for_buck(DiceRootConfiguration {
        io,
        digest_config,
        root_config,
        tenting_acl_provider,
        buck_settings,
    })?;
    let mut dice = DiceDataBuilder::from(data);

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
        let mut storage = DiceStorage::open(&path, backend).map_err(|e| {
            buck2_error::conversion::from_any_with_tag(e, buck2_error::ErrorTag::Environment)
        })?;
        storage.set_page_out_policy(page_out_policy);
        storage.set_eviction_policy(eviction_policy);
        if defer_field_reads {
            storage
                .storage_context()
                .get_or_init(|| DeferredFieldReadsEnabled);
        }
        dice.set_pagable_storage(storage);
    }

    let dice = dice.build(DetectCycles::Enabled);
    let root = dice.new_root().await;
    let mut dice_ctx = dice.updater(root);
    dice_ctx.set_none_cell_resolver()?;
    dice_ctx.set_none_legacy_config_external_data()?;
    dice_ctx.commit().await;

    Ok((dice, root))
}
