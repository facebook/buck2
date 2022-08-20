/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_common::file_ops::FileOps;
use buck2_common::legacy_configs::LegacyBuckConfigs;
use buck2_common::pattern::resolve::resolve_target_patterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_core::cells::CellInstance;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::package::Package;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::PatternType;
use buck2_core::target::TargetLabel;
use cli_proto::ClientContext;
use gazebo::dupe::Dupe;
use gazebo::prelude::*;

pub struct PatternParser {
    cell: CellInstance,
    cwd: Package,
    target_alias_resolver: BuckConfigTargetAliasResolver,
}

impl PatternParser {
    pub fn new(
        cell_resolver: &CellResolver,
        config: &LegacyBuckConfigs,
        cwd: &ProjectRelativePath,
    ) -> anyhow::Result<Self> {
        let cwd = Package::from_cell_path(&cell_resolver.get_cell_path(&cwd)?);
        let cell_name = cwd.as_cell_path().cell();

        // Targets with cell aliases should be resolved against the cell mapping
        // as defined the cell derived from the cwd.
        let cell = cell_resolver
            .get(cell_name)
            .with_context(|| format!("Cell does not exist: `{}`", cell_name))?
            .dupe();

        let target_alias_resolver = config.get(cell_name)?.target_alias_resolver();

        Ok(Self {
            cell,
            cwd,
            target_alias_resolver,
        })
    }

    pub fn parse_pattern<T: PatternType>(&self, pattern: &str) -> anyhow::Result<ParsedPattern<T>> {
        ParsedPattern::parse_relaxed(
            &self.target_alias_resolver,
            self.cell.cell_alias_resolver(),
            &self.cwd,
            pattern,
        )
    }
}

/// Parse target patterns out of command line arguments.
///
/// The format allowed here is more relaxed than in build files and elsewhere, so only use this
/// with strings passed by the user on the CLI.
/// See `ParsedPattern::parse_relaxed` for details.
pub fn parse_patterns_from_cli_args<T: PatternType>(
    target_patterns: &[buck2_data::TargetPattern],
    cell_resolver: &CellResolver,
    configs: &LegacyBuckConfigs,
    cwd: &ProjectRelativePath,
) -> anyhow::Result<Vec<ParsedPattern<T>>> {
    let parser = PatternParser::new(cell_resolver, configs, cwd)?;

    target_patterns.try_map(|value| parser.parse_pattern(&value.value))
}

pub async fn resolve_patterns<T: PatternType>(
    patterns: &[ParsedPattern<T>],
    cell_resolver: &CellResolver,
    file_ops: &dyn FileOps,
) -> anyhow::Result<ResolvedPattern<T>> {
    resolve_target_patterns(cell_resolver, patterns.iter(), file_ops).await
}

/// Extract target configuration (platform) label from [`ClientContext`].
pub async fn target_platform_from_client_context(
    client_context: Option<&ClientContext>,
    cell_resolver: &CellResolver,
    working_dir: &ProjectRelativePathBuf,
) -> anyhow::Result<Option<TargetLabel>> {
    let cwd = cell_resolver.get_cell_path(working_dir)?;
    let cell_alias_resolver = cell_resolver.get(cwd.cell()).unwrap().cell_alias_resolver();

    Ok(match client_context {
        Some(client_context) => {
            let target_platform = &client_context.target_platform;
            if !target_platform.is_empty() {
                Some(
                    ParsedPattern::parse_precise(cell_alias_resolver, target_platform)?
                        .as_target_label(target_platform)?,
                )
            } else {
                None
            }
        }
        None => None,
    })
}
