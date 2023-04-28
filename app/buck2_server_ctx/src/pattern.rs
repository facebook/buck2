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
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::pattern::pattern_type::PatternType;
use buck2_core::pattern::ParsedPattern;
use buck2_core::target::label::TargetLabel;
use dice::DiceComputations;
use dice::DiceTransaction;
use gazebo::prelude::*;

use crate::ctx::ServerCommandContextTrait;

pub struct PatternParser {
    cell_resolver: CellResolver,
    cwd: CellPath,
    target_alias_resolver: BuckConfigTargetAliasResolver,
}

impl PatternParser {
    pub async fn new(ctx: &DiceComputations, cwd: &ProjectRelativePath) -> anyhow::Result<Self> {
        let cell_resolver = ctx.get_cell_resolver().await?;

        let cwd = cell_resolver.get_cell_path(&cwd)?;
        let cell_name = cwd.cell();

        let target_alias_resolver = ctx.target_alias_resolver_for_cell(cell_name).await?;

        Ok(Self {
            cell_resolver,
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
        )
    }
}

/// Parse target patterns out of command line arguments.
///
/// The format allowed here is more relaxed than in build files and elsewhere, so only use this
/// with strings passed by the user on the CLI.
/// See `ParsedPattern::parse_relaxed` for details.
pub async fn parse_patterns_from_cli_args<T: PatternType>(
    ctx: &DiceComputations,
    target_patterns: &[buck2_data::TargetPattern],
    cwd: &ProjectRelativePath,
) -> anyhow::Result<Vec<ParsedPattern<T>>> {
    let parser = PatternParser::new(ctx, cwd).await?;

    target_patterns.try_map(|value| parser.parse_pattern(&value.value))
}

/// Extract target configuration (platform) label from [`ClientContext`].
pub async fn target_platform_from_client_context(
    client_ctx: &ClientContext,
    server_ctx: &dyn ServerCommandContextTrait,
    dice_ctx: &DiceTransaction,
) -> anyhow::Result<Option<TargetLabel>> {
    target_platform_from_client_context_impl(
        client_ctx,
        &dice_ctx.get_cell_resolver().await?,
        server_ctx.working_dir(),
    )
    .await
}

async fn target_platform_from_client_context_impl(
    client_context: &ClientContext,
    cell_resolver: &CellResolver,
    working_dir: &ProjectRelativePath,
) -> anyhow::Result<Option<TargetLabel>> {
    let cwd = cell_resolver.get_cell_path(working_dir)?;

    let target_platform = &client_context.target_platform;
    if !target_platform.is_empty() {
        Ok(Some(
            ParsedPattern::parse_precise(target_platform, cwd.cell(), cell_resolver)?
                .as_target_label(target_platform)?,
        ))
    } else {
        Ok(None)
    }
}
