/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_common::dice::cycles::CycleAdapterDescriptor;
use buck2_common::result::SharedResult;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_core::target::label::TargetLabel;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::RuleKind;
use buck2_util::cycle_detector::CycleDescriptor;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use gazebo::prelude::*;
use thiserror::Error;

use crate::configuration::calculation::ConfigurationCalculation;
use crate::nodes::calculation::get_execution_platform_toolchain_dep;
use crate::nodes::calculation::ConfiguredTargetNodeKey;

/// Provides the Dice calculations used for implementing builds and related operations.
///
/// Most of this is implemented within the buck2_build_api, with some thin wrappers over some
/// interpreter things.
#[async_trait]
pub trait Calculation<'c> {
    /// Returns the Configuration for an unconfigured TargetLabel or ProvidersLabel.
    ///
    /// This performs "target platform resolution" on the provided target and returns the configured
    /// result.
    ///
    /// Normally, a TargetLabel and ProvidersLabel would
    /// get its Configuration based on the context it's being requested in (i.e configuration is
    /// passed down from higher nodes). For top-level requested things, though, we will have an
    /// unconfigured (or "lightly"-configured) thing and the Configuration will be determined as
    /// a mix of the global Configuration, the target's `default_target_platform` and
    /// (potentially) self-transitions on that node.
    async fn get_configured_target(
        &self,
        target: &TargetLabel,
        global_target_platform: Option<&TargetLabel>,
    ) -> anyhow::Result<ConfiguredTargetLabel>;

    async fn get_configured_provider_label(
        &self,
        target: &ProvidersLabel,
        global_target_platform: Option<&TargetLabel>,
    ) -> anyhow::Result<ConfiguredProvidersLabel>;

    async fn get_default_configured_target(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<ConfiguredTargetLabel>;
}

#[async_trait]
impl<'c> Calculation<'c> for DiceComputations {
    async fn get_configured_target(
        &self,
        target: &TargetLabel,
        global_target_platform: Option<&TargetLabel>,
    ) -> anyhow::Result<ConfiguredTargetLabel> {
        let node = self.get_target_node(target).await?;

        let get_platform_configuration = async || -> SharedResult<ConfigurationData> {
            Ok(match global_target_platform {
                Some(global_target_platform) => {
                    self.get_platform_configuration(global_target_platform)
                        .await?
                }
                None => match node.get_default_target_platform() {
                    Some(target) => self.get_platform_configuration(target).await?,
                    None => self.get_default_platform(target).await?,
                },
            })
        };

        match node.rule_kind() {
            RuleKind::Configuration => Ok(target.configure(ConfigurationData::unbound())),
            RuleKind::Normal => Ok(target.configure(get_platform_configuration().await?)),
            RuleKind::Toolchain => {
                let cfg = get_platform_configuration().await?;
                let exec_cfg = get_execution_platform_toolchain_dep(
                    self,
                    &target.configure(cfg.dupe()),
                    &node,
                )
                .await?
                .cfg();
                Ok(target.configure_with_exec(cfg, exec_cfg.cfg().dupe()))
            }
        }
    }

    async fn get_configured_provider_label(
        &self,
        target: &ProvidersLabel,
        global_target_platform: Option<&TargetLabel>,
    ) -> anyhow::Result<ConfiguredProvidersLabel> {
        let configured_target_label = self
            .get_configured_target(target.target(), global_target_platform)
            .await?;
        Ok(ConfiguredProvidersLabel::new(
            configured_target_label,
            target.name().clone(),
        ))
    }

    async fn get_default_configured_target(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<ConfiguredTargetLabel> {
        self.get_configured_target(target, None).await
    }
}

#[derive(Error, Debug, Clone, Dupe)]
pub struct ConfiguredGraphCycleError {
    cycle: Arc<Vec<ConfiguredGraphCycleKeys>>,
}

impl std::fmt::Display for ConfiguredGraphCycleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Configured target cycle detected (`->` means \"depends on\"):"
        )?;
        for p in self.cycle.iter() {
            writeln!(f, "  {} ->", p)?;
        }
        // point back at the first item in the cycle.
        writeln!(f, "  {}", self.cycle.first().unwrap())?;
        Ok(())
    }
}

// TODO(cjhopman): There's other keys that could be involved in a cycle in the configured graph and they should probably also be tracked
// here. Would be good to check on things like transitions, toolchains, configuration nodes. Still, this will currently catch most
// configured graph cycles.
#[derive(Debug, Display, Clone, Eq, PartialEq, Hash)]
pub enum ConfiguredGraphCycleKeys {
    #[display(fmt = "{}", _0)]
    ConfiguredTargetNode(ConfiguredTargetNodeKey),
}

#[derive(Debug)]
pub struct ConfiguredGraphCycleDescriptor;

impl CycleDescriptor for ConfiguredGraphCycleDescriptor {
    type Key = ConfiguredGraphCycleKeys;

    type Error = ConfiguredGraphCycleError;

    fn cycle_error(cycle: Vec<&Self::Key>) -> Self::Error {
        ConfiguredGraphCycleError {
            cycle: Arc::new(cycle.cloned()),
        }
    }
}

impl CycleAdapterDescriptor for ConfiguredGraphCycleDescriptor {
    fn to_key(key: &dyn std::any::Any) -> Option<Self::Key> {
        if let Some(v) = key.downcast_ref::<ConfiguredTargetNodeKey>() {
            return Some(ConfiguredGraphCycleKeys::ConfiguredTargetNode(v.dupe()));
        }
        None
    }
}
