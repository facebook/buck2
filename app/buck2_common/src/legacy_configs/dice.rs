/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Dice operations for legacy configuration

use std::future::Future;
use std::str::FromStr;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::name::CellName;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_events::dispatch::get_dispatcher;
use derive_more::Display;
use dice::DiceComputations;
use dice::DiceProjectionComputations;
use dice::DiceTransactionUpdater;
use dice::InjectedKey;
use dice::Key;
use dice::OpaqueValue;
use dice::ProjectionKey;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use pagable::Pagable;

use crate::dice::cells::HasCellResolver;
use crate::legacy_configs::cells::BuckConfigBasedCells;
use crate::legacy_configs::cells::ExternalBuckconfigData;
use crate::legacy_configs::configs::LegacyBuckConfig;
use crate::legacy_configs::key::BuckconfigKeyRef;
use crate::legacy_configs::view::LegacyBuckConfigView;

/// Buckconfig view which queries buckconfig entry from DICE.
#[derive(Clone, Dupe)]
pub struct OpaqueLegacyBuckConfigOnDice {
    config: Arc<OpaqueValue<LegacyBuckConfigForCellKey>>,
}

impl std::fmt::Debug for OpaqueLegacyBuckConfigOnDice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LegacyBuckConfigOnDice")
            .field("config", &self.config)
            .finish()
    }
}

impl OpaqueLegacyBuckConfigOnDice {
    pub fn lookup(
        &self,
        ctx: &mut DiceComputations,
        key: BuckconfigKeyRef,
    ) -> buck2_error::Result<Option<Arc<str>>> {
        let BuckconfigKeyRef { section, property } = key;
        Ok(ctx.projection(
            &*self.config,
            &LegacyBuckConfigPropertyProjectionKey {
                section: section.to_owned(),
                property: property.to_owned(),
            },
        )?)
    }

    pub fn view<'a, 'd>(
        &'a self,
        ctx: &'a mut DiceComputations<'d>,
    ) -> LegacyBuckConfigOnDice<'a, 'd> {
        LegacyBuckConfigOnDice { ctx, config: self }
    }
}

pub struct LegacyBuckConfigOnDice<'a, 'd> {
    ctx: &'a mut DiceComputations<'d>,
    config: &'a OpaqueLegacyBuckConfigOnDice,
}

impl LegacyBuckConfigOnDice<'_, '_> {
    pub fn parse<T: FromStr>(&mut self, key: BuckconfigKeyRef) -> buck2_error::Result<Option<T>>
    where
        buck2_error::Error: From<<T as FromStr>::Err>,
    {
        LegacyBuckConfig::parse_value(key, self.get(key)?.as_deref())
    }
}

impl std::fmt::Debug for LegacyBuckConfigOnDice<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LegacyBuckConfigOnDice")
            .field("config", &self.config)
            .finish()
    }
}

impl LegacyBuckConfigView for LegacyBuckConfigOnDice<'_, '_> {
    fn get(&mut self, key: BuckconfigKeyRef) -> buck2_error::Result<Option<Arc<str>>> {
        self.config.lookup(self.ctx, key)
    }
}

pub trait HasInjectedLegacyConfigs {
    fn get_injected_external_buckconfig_data(
        &mut self,
    ) -> impl Future<Output = buck2_error::Result<Arc<ExternalBuckconfigData>>>;

    fn is_injected_external_buckconfig_data_key_set(
        &mut self,
    ) -> impl Future<Output = buck2_error::Result<bool>>;
}

#[async_trait]
pub trait HasLegacyConfigs {
    /// Get buckconfigs.
    ///
    /// This operation does not record buckconfig as a dependency of current computation.
    /// Accessing specific buckconfig property, records that key as dependency.
    async fn get_legacy_config_on_dice(
        &mut self,
        cell_name: CellName,
    ) -> buck2_error::Result<OpaqueLegacyBuckConfigOnDice>;

    async fn get_legacy_root_config_on_dice(
        &mut self,
    ) -> buck2_error::Result<OpaqueLegacyBuckConfigOnDice>;

    /// Use this function carefully: a computation which fetches this key will be recomputed
    /// if any buckconfig property changes.
    ///
    /// Consider using `get_legacy_config_property` instead.
    async fn get_legacy_config_for_cell(
        &mut self,
        cell_name: CellName,
    ) -> buck2_error::Result<LegacyBuckConfig>;

    async fn get_legacy_config_property(
        &mut self,
        cell_name: CellName,
        key: BuckconfigKeyRef<'_>,
    ) -> buck2_error::Result<Option<Arc<str>>>;

    async fn parse_legacy_config_property<T: FromStr>(
        &mut self,
        cell_name: CellName,
        key: BuckconfigKeyRef<'_>,
    ) -> buck2_error::Result<Option<T>>
    where
        buck2_error::Error: From<<T as FromStr>::Err>,
        T: Send + Sync + 'static;
}

pub trait SetLegacyConfigs {
    fn set_legacy_config_external_data(
        &mut self,
        overrides: ExternalBuckconfigData,
    ) -> buck2_error::Result<()>;

    fn set_none_legacy_config_external_data(&mut self) -> buck2_error::Result<()>;
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display("{:?}", self)]
struct LegacyExternalBuckConfigDataKey;

impl InjectedKey for LegacyExternalBuckConfigDataKey {
    type Value = Option<Arc<ExternalBuckconfigData>>;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[derive(Clone, Display, Debug, Hash, Eq, PartialEq, Allocative, Pagable)]
#[display("LegacyBuckConfigForCellKey({})", self.cell_name)]
struct LegacyBuckConfigForCellKey {
    cell_name: CellName,
}

#[async_trait]
impl Key for LegacyBuckConfigForCellKey {
    type Value = buck2_error::Result<LegacyBuckConfig>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> buck2_error::Result<LegacyBuckConfig> {
        let cells = ctx.get_cell_resolver().await?;
        let this_cell = cells.get(self.cell_name)?;
        let config = BuckConfigBasedCells::parse_single_cell_with_dice(ctx, this_cell.path())
            .await
            .with_buck_error_context(|| {
                format!("Computing legacy buckconfigs for cell `{}`", self.cell_name)
            })?;
        let config = config.filter_values(is_config_invisible_to_dice);

        let event = buck2_data::CellHasNewConfigs {
            cell: self.cell_name.as_str().to_owned(),
        };
        get_dispatcher().instant_event(event);

        Ok(config)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x.compare(y),
            _ => false,
        }
    }
}

/// The computation `LegacyBuckConfigForCellKey` computation might encounter an error.
///
/// We can't return that error immediately, because we only compute the opaque value. We could
/// return the error when doing the projection to the buckconfig values, but that would result in us
/// increasing the size of the value returned from that computation. Instead, we'll use a different
/// projection key to extract just the error from the cell computation, and compute that when
/// constructing the `OpaqueLegacyBuckConfigOnDice`.
#[derive(Debug, Display, Hash, Eq, PartialEq, Clone, Allocative)]
struct LegacyBuckConfigErrorKey();

impl ProjectionKey for LegacyBuckConfigErrorKey {
    type DeriveFromKey = LegacyBuckConfigForCellKey;
    type Value = Option<buck2_error::Error>;

    fn compute(
        &self,
        config: &buck2_error::Result<LegacyBuckConfig>,
        _ctx: &DiceProjectionComputations,
    ) -> Option<buck2_error::Error> {
        config.as_ref().err().cloned()
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x.is_none() && y.is_none()
    }
}

#[derive(Debug, Display, Hash, Eq, PartialEq, Clone, Allocative)]
#[display("{}.{}", section, property)]
struct LegacyBuckConfigPropertyProjectionKey {
    section: String,
    property: String,
}

impl ProjectionKey for LegacyBuckConfigPropertyProjectionKey {
    type DeriveFromKey = LegacyBuckConfigForCellKey;
    type Value = Option<Arc<str>>;

    fn compute(
        &self,
        config: &buck2_error::Result<LegacyBuckConfig>,
        _ctx: &DiceProjectionComputations,
    ) -> Option<Arc<str>> {
        // See the comment in `LegacyBuckConfigErrorKey` for why this is safe
        let config = config.as_ref().unwrap();
        config
            .get(BuckconfigKeyRef {
                section: &self.section,
                property: &self.property,
            })
            .map(|s| s.to_owned().into())
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

impl HasInjectedLegacyConfigs for DiceComputations<'_> {
    async fn get_injected_external_buckconfig_data(
        &mut self,
    ) -> buck2_error::Result<Arc<ExternalBuckconfigData>> {
        self.compute(&LegacyExternalBuckConfigDataKey).await?.ok_or_else(|| internal_error!(
            "Tried to retrieve LegacyExternalBuckConfigDataKey from the graph, but key has None value"
        ))
    }

    async fn is_injected_external_buckconfig_data_key_set(&mut self) -> buck2_error::Result<bool> {
        Ok(self
            .compute(&LegacyExternalBuckConfigDataKey)
            .await?
            .is_some())
    }
}

pub fn inject_legacy_config_for_test(
    dice: &mut DiceTransactionUpdater,
    cell_name: CellName,
    configs: LegacyBuckConfig,
) -> buck2_error::Result<()> {
    dice.changed_to([(LegacyBuckConfigForCellKey { cell_name }, Ok(configs))])?;
    dice.changed_to([(LegacyExternalBuckConfigDataKey, None)])?;
    Ok(())
}

#[async_trait]
impl HasLegacyConfigs for DiceComputations<'_> {
    async fn get_legacy_config_on_dice(
        &mut self,
        cell_name: CellName,
    ) -> buck2_error::Result<OpaqueLegacyBuckConfigOnDice> {
        let config = self
            .compute_opaque(&LegacyBuckConfigForCellKey { cell_name })
            .await?;
        if let Some(error) = self.projection(&config, &LegacyBuckConfigErrorKey())? {
            return Err(error);
        }
        Ok(OpaqueLegacyBuckConfigOnDice {
            config: Arc::new(config),
        })
    }

    async fn get_legacy_root_config_on_dice(
        &mut self,
    ) -> buck2_error::Result<OpaqueLegacyBuckConfigOnDice> {
        let cell_resolver = self.get_cell_resolver().await?;
        self.get_legacy_config_on_dice(cell_resolver.root_cell())
            .await
    }

    async fn get_legacy_config_for_cell(
        &mut self,
        cell_name: CellName,
    ) -> buck2_error::Result<LegacyBuckConfig> {
        self.compute(&LegacyBuckConfigForCellKey { cell_name })
            .await?
    }

    async fn get_legacy_config_property(
        &mut self,
        cell_name: CellName,
        key: BuckconfigKeyRef<'_>,
    ) -> buck2_error::Result<Option<Arc<str>>> {
        self.get_legacy_config_on_dice(cell_name)
            .await?
            .lookup(self, key)
    }

    async fn parse_legacy_config_property<T: FromStr>(
        &mut self,
        cell_name: CellName,
        key: BuckconfigKeyRef<'_>,
    ) -> buck2_error::Result<Option<T>>
    where
        buck2_error::Error: From<<T as FromStr>::Err>,
        T: Send + Sync + 'static,
    {
        LegacyBuckConfig::parse_value(
            key,
            self.get_legacy_config_property(cell_name, key)
                .await?
                .as_deref(),
        )
    }
}

impl SetLegacyConfigs for DiceTransactionUpdater {
    fn set_legacy_config_external_data(
        &mut self,
        data: ExternalBuckconfigData,
    ) -> buck2_error::Result<()> {
        let data = data.filter_values(is_config_invisible_to_dice);
        Ok(self.changed_to(vec![(
            LegacyExternalBuckConfigDataKey,
            Some(Arc::new(data)),
        )])?)
    }

    fn set_none_legacy_config_external_data(&mut self) -> buck2_error::Result<()> {
        Ok(self.changed_to(vec![(LegacyExternalBuckConfigDataKey, None)])?)
    }
}

fn is_config_invisible_to_dice(key: &BuckconfigKeyRef) -> bool {
    !CONFIGS_INVISIBLE_TO_DICE.contains(key)
}

/// A set of buckconfigs that are visibile outside of dice, but not within it. Importantly, changes
/// to these configs do not cause state invalidations.
// FIXME(JakobDegen): Error if someone tries to read any of these from in dice
const CONFIGS_INVISIBLE_TO_DICE: &[BuckconfigKeyRef<'static>] = &[
    BuckconfigKeyRef {
        section: "buck2_re_client",
        property: "override_use_case",
    },
    BuckconfigKeyRef {
        section: "scuba",
        property: "defaults",
    },
];

#[cfg(test)]
mod tests {
    use buck2_cli_proto::ConfigOverride;

    use crate::legacy_configs::configs::testing::parse_with_config_args;

    #[test]
    fn config_equals() -> buck2_error::Result<()> {
        let path = "test";
        let config1 = parse_with_config_args(
            &[("test", "[sec1]\na=b\n[sec2]\nx=y")],
            path,
            &[ConfigOverride::flag_no_cell("sec1.a=c")],
        )?;

        let config2 = parse_with_config_args(&[("test", "[sec1]\na=c\n[sec2]\nx=y")], path, &[])?;

        let config3 = parse_with_config_args(
            &[("test", "[sec1]\na=b\n[sec2]\nx=y")],
            path,
            &[ConfigOverride::flag_no_cell("sec1.d=e")],
        )?;

        assert!(config1.compare(&config1));
        assert!(config2.compare(&config2));
        assert!(config3.compare(&config3));
        assert!(config1.compare(&config2));
        assert!(!config1.compare(&config3));
        assert!(!config2.compare(&config3));

        Ok(())
    }
}
