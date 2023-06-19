/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Dice operations for legacy configuration

use std::str::FromStr;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::name::CellName;
use buck2_util::collections::sorted_map::SortedMap;
use derive_more::Display;
use dice::DiceComputations;
use dice::DiceProjectionComputations;
use dice::DiceTransactionUpdater;
use dice::InjectedKey;
use dice::Key;
use dice::OpaqueValue;
use dice::ProjectionKey;
use dupe::Dupe;
use dupe::OptionDupedExt;
use more_futures::cancellation::CancellationContext;

use crate::dice::cells::HasCellResolver;
use crate::legacy_configs::view::LegacyBuckConfigView;
use crate::legacy_configs::view::LegacyBuckConfigsView;
use crate::legacy_configs::ConfigError;
use crate::legacy_configs::LegacyBuckConfig;
use crate::legacy_configs::LegacyBuckConfigs;
use crate::result::SharedResult;
use crate::result::ToSharedResultExt;

/// Buckconfig view which queries buckconfig entry from DICE.
#[derive(Clone, Dupe, Debug)]
pub struct LegacyBuckConfigOnDice<'a> {
    config: Arc<OpaqueValue<'a, LegacyBuckConfigForCellKey>>,
}

impl<'a> LegacyBuckConfigView for LegacyBuckConfigOnDice<'a> {
    fn get(&self, section: &str, key: &str) -> anyhow::Result<Option<Arc<str>>> {
        self.get(section, key)
    }
}

impl<'a> LegacyBuckConfigOnDice<'a> {
    pub fn get(&self, section: &str, property: &str) -> anyhow::Result<Option<Arc<str>>> {
        Ok(self
            .config
            .projection(&LegacyBuckConfigPropertyProjectionKey {
                section: section.to_owned(),
                property: property.to_owned(),
            })?)
    }
}

#[derive(Debug)]
pub struct LegacyBuckConfigsOnDice<'a> {
    configs: SortedMap<CellName, LegacyBuckConfigOnDice<'a>>,
}

impl<'a> LegacyBuckConfigsOnDice<'a> {
    pub fn get(&self, cell_name: CellName) -> anyhow::Result<LegacyBuckConfigOnDice<'a>> {
        self.configs
            .get(&cell_name)
            .duped()
            .ok_or_else(|| ConfigError::UnknownCell(cell_name.to_owned()).into())
    }
}

impl<'a> LegacyBuckConfigsView for LegacyBuckConfigsOnDice<'a> {
    fn get<'x>(&'x self, cell_name: CellName) -> anyhow::Result<&'x dyn LegacyBuckConfigView> {
        let config = self
            .configs
            .get(&cell_name)
            .ok_or_else(|| anyhow::Error::new(ConfigError::UnknownCell(cell_name.to_owned())))?;
        Ok(config)
    }

    fn iter<'x>(
        &'x self,
    ) -> Box<dyn Iterator<Item = (CellName, &'x dyn LegacyBuckConfigView)> + 'x> {
        Box::new(
            self.configs
                .iter()
                .map(|(cell_name, config)| (*cell_name, config as &dyn LegacyBuckConfigView)),
        )
    }
}

#[async_trait]
pub trait HasLegacyConfigs {
    /// Get buckconfigs.
    ///
    /// This operation does not record buckconfig as a dependency of current computation.
    /// Accessing specific buckconfig property, records that key as dependency.
    async fn get_legacy_configs_on_dice(&self) -> anyhow::Result<LegacyBuckConfigsOnDice>;

    async fn get_legacy_config_on_dice(
        &self,
        cell_name: CellName,
    ) -> anyhow::Result<LegacyBuckConfigOnDice>;

    async fn get_legacy_root_config_on_dice(&self) -> anyhow::Result<LegacyBuckConfigOnDice>;

    /// Use this function carefully: a computation which fetches this key will be recomputed
    /// if any buckconfig property changes.
    ///
    /// Consider using `get_legacy_config_property` instead.
    async fn get_legacy_configs(&self) -> anyhow::Result<LegacyBuckConfigs>;

    /// Checks if LegacyBuckConfigsKey has been set in the DICE graph.
    async fn is_legacy_configs_key_set(&self) -> anyhow::Result<bool>;

    /// Use this function carefully: a computation which fetches this key will be recomputed
    /// if any buckconfig property changes.
    ///
    /// Consider using `get_legacy_config_property` instead.
    async fn get_legacy_config_for_cell(
        &self,
        cell_name: CellName,
    ) -> SharedResult<LegacyBuckConfig>;

    async fn get_legacy_config_property(
        &self,
        cell_name: CellName,
        section: &str,
        property: &str,
    ) -> anyhow::Result<Option<Arc<str>>>;

    async fn parse_legacy_config_property<T: FromStr>(
        &self,
        cell_name: CellName,
        section: &str,
        key: &str,
    ) -> anyhow::Result<Option<T>>
    where
        anyhow::Error: From<<T as FromStr>::Err>,
        T: Send + Sync + 'static;
}

pub trait SetLegacyConfigs {
    fn set_legacy_configs(&mut self, legacy_configs: LegacyBuckConfigs) -> anyhow::Result<()>;

    fn set_none_legacy_configs(&mut self) -> anyhow::Result<()>;
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "{:?}", self)]
struct LegacyBuckConfigKey;

impl InjectedKey for LegacyBuckConfigKey {
    type Value = Option<LegacyBuckConfigs>;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Some(x), Some(y)) => x.compare(y),
            (None, None) => true,
            (_, _) => false,
        }
    }
}

#[derive(Clone, Display, Debug, Hash, Eq, PartialEq, Allocative)]
#[display(fmt = "LegacyBuckConfigForCellKey({})", "self.cell_name")]
struct LegacyBuckConfigForCellKey {
    cell_name: CellName,
}

#[async_trait]
impl Key for LegacyBuckConfigForCellKey {
    type Value = SharedResult<LegacyBuckConfig>;

    async fn compute(
        &self,
        ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> SharedResult<LegacyBuckConfig> {
        let legacy_configs = ctx.get_legacy_configs().await?;
        legacy_configs
            .get(self.cell_name)
            .map(|x| x.dupe())
            .shared_error()
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x.compare(y),
            _ => false,
        }
    }
}

#[derive(Debug, Display, Clone, Eq, PartialEq, Hash, Allocative)]
#[display(fmt = "{}//{}.{}", cell_name, section, property)]
struct LegacyBuckConfigPropertyKey {
    cell_name: CellName,
    section: String,
    property: String,
}

#[async_trait]
impl Key for LegacyBuckConfigPropertyKey {
    type Value = SharedResult<Option<Arc<str>>>;

    async fn compute(
        &self,
        ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> SharedResult<Option<Arc<str>>> {
        let legacy_config = ctx.get_legacy_config_for_cell(self.cell_name).await?;
        Ok(legacy_config
            .get(&self.section, &self.property)
            .map(|s| s.to_owned().into()))
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

#[derive(Debug, Display, Hash, Eq, PartialEq, Clone, Allocative)]
#[display(fmt = "{}.{}", section, property)]
struct LegacyBuckConfigPropertyProjectionKey {
    section: String,
    property: String,
}

impl ProjectionKey for LegacyBuckConfigPropertyProjectionKey {
    type DeriveFromKey = LegacyBuckConfigForCellKey;
    type Value = Option<Arc<str>>;

    fn compute(
        &self,
        config: &SharedResult<LegacyBuckConfig>,
        _ctx: &DiceProjectionComputations,
    ) -> Option<Arc<str>> {
        // This is safe, because this code is only called from `DiceLegacyBuckConfig`
        // which is known to be constructed from a valid cell.
        let config = config.as_ref().unwrap();
        config
            .get(&self.section, &self.property)
            .map(|s| s.to_owned().into())
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[derive(Debug, Display, Hash, PartialEq, Eq, Clone, Dupe, Allocative)]
#[display(fmt = "{:?}", self)]
struct LegacyBuckConfigCellNamesKey;

impl ProjectionKey for LegacyBuckConfigCellNamesKey {
    type DeriveFromKey = LegacyBuckConfigKey;
    type Value = Arc<Vec<CellName>>;

    fn compute(
        &self,
        configs: &Option<LegacyBuckConfigs>,
        _ctx: &DiceProjectionComputations,
    ) -> Arc<Vec<CellName>> {
        let cell_names: Vec<_> = configs
            .as_ref()
            .unwrap_or_else(|| {
                panic!(
                    "Tried to retrieve LegacyBuckConfigKey from the graph, but key has None value"
                )
            })
            .iter()
            .map(|(k, _)| k)
            .collect();
        assert!(
            cell_names.is_sorted(),
            "configs.iter() must return a sorted iterator"
        );
        Arc::new(cell_names)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[async_trait]
impl HasLegacyConfigs for DiceComputations {
    async fn get_legacy_configs_on_dice(&self) -> anyhow::Result<LegacyBuckConfigsOnDice> {
        let configs = self.compute_opaque(&LegacyBuckConfigKey).await?;
        let cell_names = configs.projection(&LegacyBuckConfigCellNamesKey)?;
        let mut configs_on_dice = Vec::with_capacity(cell_names.len());
        for cell_name in &*cell_names {
            let config = self
                .compute_opaque(&LegacyBuckConfigForCellKey {
                    cell_name: *cell_name,
                })
                .await?;
            configs_on_dice.push((
                *cell_name,
                LegacyBuckConfigOnDice {
                    config: Arc::new(config),
                },
            ));
        }
        Ok(LegacyBuckConfigsOnDice {
            configs: SortedMap::from_iter(configs_on_dice),
        })
    }

    async fn get_legacy_config_on_dice(
        &self,
        cell_name: CellName,
    ) -> anyhow::Result<LegacyBuckConfigOnDice> {
        self.get_legacy_configs_on_dice().await?.get(cell_name)
    }

    async fn get_legacy_root_config_on_dice(&self) -> anyhow::Result<LegacyBuckConfigOnDice> {
        let cell_resolver = self.get_cell_resolver().await?;
        self.get_legacy_config_on_dice(cell_resolver.root_cell())
            .await
    }

    async fn get_legacy_configs(&self) -> anyhow::Result<LegacyBuckConfigs> {
        self.compute(&LegacyBuckConfigKey).await?.ok_or_else(|| {
            panic!("Tried to retrieve LegacyBuckConfigKey from the graph, but key has None value")
        })
    }

    async fn is_legacy_configs_key_set(&self) -> anyhow::Result<bool> {
        Ok(self.compute(&LegacyBuckConfigKey).await?.is_some())
    }

    async fn get_legacy_config_for_cell(
        &self,
        cell_name: CellName,
    ) -> SharedResult<LegacyBuckConfig> {
        self.compute(&LegacyBuckConfigForCellKey { cell_name })
            .await?
    }

    async fn get_legacy_config_property(
        &self,
        cell_name: CellName,
        section: &str,
        property: &str,
    ) -> anyhow::Result<Option<Arc<str>>> {
        Ok(self
            .compute(&LegacyBuckConfigPropertyKey {
                cell_name,
                section: section.to_owned(),
                property: property.to_owned(),
            })
            .await??)
    }

    async fn parse_legacy_config_property<T: FromStr>(
        &self,
        cell_name: CellName,
        section: &str,
        key: &str,
    ) -> anyhow::Result<Option<T>>
    where
        anyhow::Error: From<<T as FromStr>::Err>,
        T: Send + Sync + 'static,
    {
        let v = self
            .get_legacy_config_property(cell_name, section, key)
            .await?;
        match v {
            None => Ok(None),
            Some(v) => Ok(Some(LegacyBuckConfig::parse_impl(section, key, &v)?)),
        }
    }
}

impl SetLegacyConfigs for DiceTransactionUpdater {
    fn set_legacy_configs(&mut self, legacy_configs: LegacyBuckConfigs) -> anyhow::Result<()> {
        Ok(self.changed_to(vec![(LegacyBuckConfigKey, Some(legacy_configs))])?)
    }

    fn set_none_legacy_configs(&mut self) -> anyhow::Result<()> {
        Ok(self.changed_to(vec![(LegacyBuckConfigKey, None)])?)
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::cells::name::CellName;
    use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
    use dice::InjectedKey;

    use crate::legacy_configs::dice::LegacyBuckConfigKey;
    use crate::legacy_configs::testing::TestConfigParserFileOps;
    use crate::legacy_configs::LegacyBuckConfig;
    use crate::legacy_configs::LegacyBuckConfigs;
    use crate::legacy_configs::LegacyConfigCmdArg;

    #[test]
    fn config_equals() -> anyhow::Result<()> {
        #[cfg(not(windows))]
        let path = &AbsNormPathBuf::from("/test".to_owned())?;
        #[cfg(windows)]
        let path = &AbsNormPathBuf::from("C:/test".to_owned())?;
        let config1 = Some(LegacyBuckConfigs::new(hashmap![
            CellName::testing_new("cell1")
            => {
                let mut file_ops = TestConfigParserFileOps::new(&[("/test", "[sec1]\na=b\n[sec2]\nx=y")])?;
                LegacyBuckConfig::parse_with_file_ops(
                    path,
                    &mut file_ops,
                    &[LegacyConfigCmdArg::Flag("sec1.a=c".to_owned())],
                )?
            },
            CellName::testing_new("cell2")
            => {
                let mut file_ops = TestConfigParserFileOps::new(&[("/test", "[sec1]\nx=y\n[sec2]\na=b")])?;
                LegacyBuckConfig::parse_with_file_ops(
                    path,
                    &mut file_ops,
                    &[],
                )?
            }
        ]));

        let config2 = Some(LegacyBuckConfigs::new(hashmap![
            CellName::testing_new("cell1")
            => {
                let mut file_ops = TestConfigParserFileOps::new(&[("/test", "[sec1]\na=b\n[sec2]\nx=y")])?;
                LegacyBuckConfig::parse_with_file_ops(
                    path,
                    &mut file_ops,
                    &[LegacyConfigCmdArg::Flag("sec1.a=c".to_owned())],
                )?
            },
        ]));

        let config3 = Some(LegacyBuckConfigs::new(hashmap![
            CellName::testing_new("cell1")
            => {
                let mut file_ops = TestConfigParserFileOps::new(&[("/test", "[sec1]\na=c\n[sec2]\nx=y")])?;
                LegacyBuckConfig::parse_with_file_ops(
                    path,
                    &mut file_ops,
                    &[],
                )?
            },
        ]));

        let config4 = Some(LegacyBuckConfigs::new(hashmap![
            CellName::testing_new("cell1")
            => {
                let mut file_ops = TestConfigParserFileOps::new(&[("/test", "[sec1]\na=b\n[sec2]\nx=y")])?;
                LegacyBuckConfig::parse_with_file_ops(
                    path,
                    &mut file_ops,
                    &[LegacyConfigCmdArg::Flag("sec1.d=e".to_owned())],
                )?
            },
        ]));

        let config5: Option<LegacyBuckConfigs> = None;
        let config6: Option<LegacyBuckConfigs> = None;

        assert_eq!(LegacyBuckConfigKey::equality(&config1, &config1), true);
        assert_eq!(LegacyBuckConfigKey::equality(&config2, &config2), true);
        assert_eq!(LegacyBuckConfigKey::equality(&config3, &config3), true);
        assert_eq!(LegacyBuckConfigKey::equality(&config4, &config4), true);
        assert_eq!(LegacyBuckConfigKey::equality(&config1, &config2), false);
        assert_eq!(LegacyBuckConfigKey::equality(&config1, &config3), false);
        assert_eq!(LegacyBuckConfigKey::equality(&config1, &config4), false);
        assert_eq!(LegacyBuckConfigKey::equality(&config2, &config3), true);
        assert_eq!(LegacyBuckConfigKey::equality(&config2, &config4), false);
        assert_eq!(LegacyBuckConfigKey::equality(&config3, &config4), false);
        assert_eq!(LegacyBuckConfigKey::equality(&config5, &config1), false);
        assert_eq!(LegacyBuckConfigKey::equality(&config5, &config6), true);

        Ok(())
    }
}
