/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::TargetCfg;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::global_cfg_options::GlobalCfgOptions;
use buck2_common::pattern::resolve::ResolveTargetPatterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::pattern::pattern_type::PatternType;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use dice::DiceComputations;
use dupe::Dupe;
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

pub async fn parse_and_resolve_patterns_from_cli_args<T: PatternType>(
    ctx: &mut DiceComputations<'_>,
    target_patterns: &[buck2_data::TargetPattern],
    cwd: &ProjectRelativePath,
) -> anyhow::Result<ResolvedPattern<T>> {
    let patterns = parse_patterns_from_cli_args(ctx, target_patterns, cwd).await?;
    ResolveTargetPatterns::resolve(ctx, &patterns).await
}

pub async fn parse_and_resolve_patterns_to_targets_from_cli_args<T: PatternType>(
    ctx: &mut DiceComputations<'_>,
    target_patterns: &[buck2_data::TargetPattern],
    cwd: &ProjectRelativePath,
) -> anyhow::Result<Vec<(TargetLabel, T)>> {
    let resolved_pattern =
        parse_and_resolve_patterns_from_cli_args::<T>(ctx, target_patterns, cwd).await?;
    let mut result_targets = Vec::new();
    for (package, spec) in resolved_pattern.specs {
        match spec {
            buck2_core::pattern::PackageSpec::Targets(targets) => {
                result_targets.extend(targets.into_map(|(name, extra)| {
                    (TargetLabel::new(package.dupe(), name.as_ref()), extra)
                }))
            }
            buck2_core::pattern::PackageSpec::All => {
                // Note this code is not parallel. Careful if used in performance sensitive code.
                let interpreter_results = ctx.get_interpreter_results(package.dupe()).await?;
                result_targets.extend(
                    interpreter_results
                        .targets()
                        .keys()
                        .map(|target| (TargetLabel::new(package.dupe(), target), T::default())),
                );
            }
        }
    }
    Ok(result_targets)
}

pub async fn parse_and_resolve_provider_labels_from_cli_args(
    ctx: &mut DiceComputations<'_>,
    target_patterns: &[buck2_data::TargetPattern],
    cwd: &ProjectRelativePath,
) -> anyhow::Result<Vec<ProvidersLabel>> {
    let targets = parse_and_resolve_patterns_to_targets_from_cli_args::<ProvidersPatternExtra>(
        ctx,
        target_patterns,
        cwd,
    )
    .await?;
    Ok(targets
        .into_map(|(label, providers)| providers.into_providers_label(label.pkg(), label.name())))
}

/// Extract target configuration components.
pub async fn global_cfg_options_from_client_context(
    target_cfg: &TargetCfg,
    server_ctx: &dyn ServerCommandContextTrait,
    dice_ctx: &mut DiceComputations<'_>,
) -> anyhow::Result<GlobalCfgOptions> {
    let cell_resolver: &CellResolver = &dice_ctx.get_cell_resolver().await?;
    let working_dir: &ProjectRelativePath = server_ctx.working_dir();
    let cell_alias_resolver = cell_resolver.get_cwd_cell_alias_resolver(working_dir)?;
    let cwd = cell_resolver.get_cell_path(working_dir)?;
    let target_platform = &target_cfg.target_platform;
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
        cli_modifiers: target_cfg.cli_modifiers.clone().into(),
    })
}
