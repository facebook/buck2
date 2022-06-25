/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::cells::CellName;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::package::Package;
use buck2_core::result::SharedResult;
use buck2_core::target_aliases::TargetAliasResolver;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use gazebo::prelude::*;
use indexmap::IndexSet;
use itertools::Itertools;

use crate::dice::cells::HasCellResolver;
use crate::legacy_configs::dice::HasLegacyConfigs;
use crate::legacy_configs::LegacyBuckConfig;

#[derive(thiserror::Error, Debug)]
enum AliasResolutionError {
    #[error("No [alias] section in buckconfig")]
    MissingAliasSection,
    #[error("[alias] section does not contain the requested alias")]
    NotAnAlias,
    #[error("[alias] section produced a dangling chain: `{}`", .0.iter().join(" -> "))]
    AliasChainBroken(Vec<String>),
    #[error("cycle detected in alias resolution [{} -> {1}]", .0.iter().join(" -> "))]
    AliasCycle(Vec<String>, String),
}

#[derive(Dupe, Clone)]
pub struct BuckConfigTargetAliasResolver {
    config: LegacyBuckConfig,
}

impl PartialEq for BuckConfigTargetAliasResolver {
    fn eq(&self, other: &BuckConfigTargetAliasResolver) -> bool {
        // `TargetAliasResolver` only uses `alias` section of buckconfig,
        // comparing only this section is enough.
        // Please update this code if `TargetAliasResolver` uses other buckconfigs.
        let self_aliases = self.config.get_section("alias");
        let other_aliases = other.config.get_section("alias");
        match (self_aliases, other_aliases) {
            (Some(self_aliases), Some(other_aliases)) => self_aliases.compare(other_aliases),
            (None, None) => true,
            (None, Some(_)) | (Some(_), None) => false,
        }
    }
}

impl TargetAliasResolver for BuckConfigTargetAliasResolver {
    fn get<'r, 'a: 'r, 'b: 'r>(&'a self, name: &'b str) -> anyhow::Result<Option<&'r str>> {
        match self.resolve_alias(name) {
            Ok(a) => Ok(Some(a)),
            Err(AliasResolutionError::MissingAliasSection | AliasResolutionError::NotAnAlias) => {
                Ok(None)
            }
            Err(
                e @ AliasResolutionError::AliasChainBroken(..)
                | e @ AliasResolutionError::AliasCycle(..),
            ) => Err(anyhow::Error::from(e).context(format!("Error resolving alias `{}`", name))),
        }
    }
}

impl BuckConfigTargetAliasResolver {
    pub fn new(config: LegacyBuckConfig) -> Self {
        Self { config }
    }

    /// Resolves an alias in the `[alias]` section. Aliases can refer to other aliases. Any
    /// string containing ":" is considered to be the end of the alias resolution.
    fn resolve_alias<'r, 'a: 'r, 'b: 'r>(
        &'a self,
        alias_in: &'b str,
    ) -> Result<&'r str, AliasResolutionError> {
        // The rebinding is necessary to give `alias` a different lifetime.
        let mut alias = alias_in;

        if alias.contains(':') {
            return Err(AliasResolutionError::NotAnAlias);
        }

        let section = self.config.get_section("alias");
        let mut stack = IndexSet::<&str>::new();
        while !alias.contains(':') {
            if stack.contains(alias) {
                return Err(AliasResolutionError::AliasCycle(
                    stack.into_iter().map(|s| s.to_owned()).collect(),
                    alias.to_owned(),
                ));
            }

            alias = match &section {
                Some(section) => match section.get(alias) {
                    Some(v) => {
                        stack.insert(alias);
                        v.as_str()
                    }
                    None => {
                        if stack.is_empty() {
                            return Err(AliasResolutionError::NotAnAlias);
                        } else {
                            let chain = stack
                                .into_iter()
                                .chain(std::iter::once(alias))
                                .map(|e| e.to_owned())
                                .collect();
                            return Err(AliasResolutionError::AliasChainBroken(chain));
                        }
                    }
                },
                None => return Err(AliasResolutionError::MissingAliasSection),
            };
        }

        Ok(alias)
    }
}

#[async_trait]
pub trait HasTargetAliasResolver {
    async fn target_alias_resolver_for_cell(
        &self,
        cell_name: &CellName,
    ) -> anyhow::Result<BuckConfigTargetAliasResolver>;

    async fn target_alias_resolver_for_working_dir(
        &self,
        working_dir: &ProjectRelativePath,
    ) -> anyhow::Result<BuckConfigTargetAliasResolver>;
}

#[derive(Debug, Display, Hash, PartialEq, Eq, Clone)]
#[display(fmt = "{}", .0)]
struct TargetAliasResolverKey {
    cell_name: CellName,
}

#[async_trait]
impl Key for TargetAliasResolverKey {
    type Value = SharedResult<BuckConfigTargetAliasResolver>;

    async fn compute(&self, ctx: &DiceComputations) -> SharedResult<BuckConfigTargetAliasResolver> {
        let legacy_configs = ctx.get_legacy_config_for_cell(&self.cell_name).await?;
        Ok(legacy_configs.target_alias_resolver())
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

#[async_trait]
impl HasTargetAliasResolver for DiceComputations {
    async fn target_alias_resolver_for_cell(
        &self,
        cell_name: &CellName,
    ) -> anyhow::Result<BuckConfigTargetAliasResolver> {
        Ok(self
            .compute(&TargetAliasResolverKey {
                cell_name: cell_name.clone(),
            })
            .await?)
    }

    async fn target_alias_resolver_for_working_dir(
        &self,
        working_dir: &ProjectRelativePath,
    ) -> anyhow::Result<BuckConfigTargetAliasResolver> {
        let cell_resolver = self.get_cell_resolver().await?;
        let working_dir = Package::from_cell_path(&cell_resolver.get_cell_path(&working_dir)?);
        let cell_name = working_dir.as_cell_path().cell();
        self.target_alias_resolver_for_cell(cell_name).await
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use indoc::indoc;

    use crate::legacy_configs;
    use crate::target_aliases::AliasResolutionError;
    use crate::target_aliases::BuckConfigTargetAliasResolver;

    #[test]
    fn test_aliases() -> anyhow::Result<()> {
        let config = legacy_configs::testing::parse(
            &[(
                "/config",
                indoc!(
                    r#"
            [alias]
              baz = foo
              foo = //:foo
              bar = foo
              bar2 = bar
              cycle1 = cycle2
              cycle2 = cycle3
              cycle3 = cycle1
              chain1 = chain2
              chain2 = chain3

        "#
                ),
            )],
            "/config",
        )?;

        let target_alias_resolver = BuckConfigTargetAliasResolver::new(config);

        assert_eq!("//:foo", target_alias_resolver.resolve_alias("foo")?);
        assert_eq!("//:foo", target_alias_resolver.resolve_alias("bar")?);
        assert_eq!("//:foo", target_alias_resolver.resolve_alias("bar2")?);
        assert_eq!("//:foo", target_alias_resolver.resolve_alias("baz")?);

        assert_matches!(
            target_alias_resolver.resolve_alias("missing"),
            Err(AliasResolutionError::NotAnAlias)
        );

        assert_matches!(
            target_alias_resolver.resolve_alias("chain1"),
            Err(e) => {
                let err = format!("{:#}", e);
                let expected = "chain1 -> chain2 -> chain3";
                assert!(
                    err.contains(expected),
                    "expected error to contain `{}`, got `{}`",
                    expected,
                    err
                );
            }
        );

        assert_matches!(
            target_alias_resolver.resolve_alias("cycle1"),
            Err(e) => {
                let err = format!("{:#}", e);
                let expected = "cycle1 -> cycle2 -> cycle3 -> cycle1";
                assert!(
                    err.contains(expected),
                    "expected error to contain `{}`, got `{}`",
                    expected,
                    err
                );
            }
        );

        Ok(())
    }
}
