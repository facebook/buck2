/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::common::target_cfg::TargetCfgWithUniverseOptions;
use buck2_core::configuration::bound_id::BoundConfigurationId;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::pattern::pattern_type::ConfigurationPredicate;
use buck2_core::pattern::pattern_type::ConfiguredTargetPatternExtra;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::pattern::parse_and_resolve_patterns_to_targets_from_cli_args;
use buck2_server_ctx::target_resolution_config::TargetResolutionConfig;
use dice::DiceComputations;
use gazebo::prelude::SliceExt;

use crate::common::target_resolution_config::audit_command_target_resolution_config;

#[derive(Debug, buck2_error::Error)]
enum PatternNotSupportedError {
    #[error("Builtin configurations are not supported: `{0}`")]
    BuiltinConfigurationsNotSupported(String),
    #[error(
        "Patterns with configuration label without configuration hash are not supported: `{0}`"
    )]
    ConfigurationLabelWithoutHashNotSupported(String),
}

pub(crate) async fn audit_command_configured_target_labels(
    ctx: &mut DiceComputations<'_>,
    patterns: &[String],
    target_cfg: &TargetCfgWithUniverseOptions,
    server_ctx: &dyn ServerCommandContextTrait,
) -> anyhow::Result<Vec<ConfiguredTargetLabel>> {
    let targets =
        parse_and_resolve_patterns_to_targets_from_cli_args::<ConfiguredTargetPatternExtra>(
            ctx,
            &patterns.map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
            server_ctx.working_dir(),
        )
        .await?;

    let target_resolution_config =
        audit_command_target_resolution_config(ctx, target_cfg, server_ctx).await?;

    let mut configured_target_labels: Vec<ConfiguredTargetLabel> = Vec::new();
    for (target_label, extra) in targets {
        match extra.cfg {
            ConfigurationPredicate::Any => {
                configured_target_labels.extend(
                    target_resolution_config
                        .get_configured_target(ctx, &target_label)
                        .await?,
                );
            }
            ConfigurationPredicate::Builtin(p) => {
                return Err(PatternNotSupportedError::BuiltinConfigurationsNotSupported(
                    p.to_string(),
                )
                .into());
            }
            ConfigurationPredicate::Bound(label, None) => {
                return Err(
                    PatternNotSupportedError::ConfigurationLabelWithoutHashNotSupported(
                        label.to_string(),
                    )
                    .into(),
                );
            }
            ConfigurationPredicate::Bound(label, Some(hash)) => {
                let cfg = ConfigurationData::lookup_bound(BoundConfigurationId { label, hash })?;
                let configured = target_label.configure(cfg);
                match &target_resolution_config {
                    TargetResolutionConfig::Default(_) => {
                        configured_target_labels.push(configured);
                    }
                    TargetResolutionConfig::Universe(universe) => {
                        if universe.contains(&configured) {
                            configured_target_labels.push(configured);
                        }
                    }
                }
            }
        }
    }

    Ok(configured_target_labels)
}
