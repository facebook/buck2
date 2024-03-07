/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::ClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::global_cfg_options::GlobalCfgOptions;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::pattern::pattern_type::PatternType;
use buck2_core::pattern::ParsedPattern;
use dice::DiceComputations;
use gazebo::prelude::*;

use crate::ctx::ServerCommandContextTrait;

pub struct PatternParser {
    cell_resolver: CellResolver,
    cell_alias_resolver: CellAliasResolver,
    cwd: CellPath,
    target_alias_resolver: BuckConfigTargetAliasResolver,
}

impl PatternParser {
    pub async fn new(
        ctx: &mut DiceComputations<'_>,
        cwd: &ProjectRelativePath,
    ) -> anyhow::Result<Self> {
        let cell_resolver = ctx.get_cell_resolver().await?;

        let cwd = cell_resolver.get_cell_path(&cwd)?;
        let cell_name = cwd.cell();

        let target_alias_resolver = ctx.target_alias_resolver_for_cell(cell_name).await?;
        let cell_alias_resolver = ctx.get_cell_alias_resolver(cell_name).await?;

        Ok(Self {
            cell_resolver,
            cell_alias_resolver,
            cwd,
            target_alias_resolver,
        })
    }

    pub fn parse_pattern<T: PatternType>(&self, pattern: &str) -> anyhow::Result<ParsedPattern<T>> {
        ParsedPattern::parse_relaxed(
            &self.target_alias_resolver,
            self.cwd.as_ref(),
            pattern,
            &self.cell_resolver,
            &self.cell_alias_resolver,
        )
    }
}

/// Parse target patterns out of command line arguments.
///
/// The format allowed here is more relaxed than in build files and elsewhere, so only use this
/// with strings passed by the user on the CLI.
/// See `ParsedPattern::parse_relaxed` for details.
pub async fn parse_patterns_from_cli_args<T: PatternType>(
    ctx: &mut DiceComputations<'_>,
    target_patterns: &[buck2_data::TargetPattern],
    cwd: &ProjectRelativePath,
) -> anyhow::Result<Vec<ParsedPattern<T>>> {
    let parser = PatternParser::new(ctx, cwd).await?;

    target_patterns.try_map(|value| parser.parse_pattern(&value.value))
}

/// Extract target configuration components from [`ClientContext`].
pub async fn global_cfg_options_from_client_context(
    client_context: &ClientContext,
    server_ctx: &dyn ServerCommandContextTrait,
    dice_ctx: &mut DiceComputations<'_>,
) -> anyhow::Result<GlobalCfgOptions> {
    let cell_resolver: &CellResolver = &dice_ctx.get_cell_resolver().await?;
    let working_dir: &ProjectRelativePath = server_ctx.working_dir();
    let cell_alias_resolver = cell_resolver.get_cwd_cell_alias_resolver(working_dir)?;
    let cwd = cell_resolver.get_cell_path(working_dir)?;
    let target_platform = &client_context.target_platform;
    let target_platform_label = if !target_platform.is_empty() {
        Some(
            ParsedPattern::parse_precise(
                target_platform,
                cwd.cell(),
                cell_resolver,
                cell_alias_resolver,
            )?
            .as_target_label(target_platform)?,
        )
    } else {
        None
    };

    Ok(GlobalCfgOptions {
        target_platform: target_platform_label,
        cli_modifiers: client_context.cli_modifiers.clone().into(),
    })
}
