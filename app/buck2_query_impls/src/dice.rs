/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_build_api::configure_targets::load_compatible_patterns;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::dice::file_ops::DiceFileComputations;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::package_listing::dice::DicePackageListingResolver;
use buck2_common::package_listing::resolver::PackageListingResolver;
use buck2_common::pattern::resolve::ResolveTargetPatterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern::TargetParsingRel;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::query_file_literal::parse_query_file_literal;
use buck2_core::provider::label::ProvidersName;
use buck2_core::soft_error;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_node::load_patterns::load_patterns;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_query::query::syntax::simple::eval::file_set::FileNode;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use dice::DiceComputations;
use dice::LinearRecomputeDiceComputations;
use futures::FutureExt;
use gazebo::prelude::*;
use indexmap::indexset;

use crate::cquery::environment::CqueryDelegate;
use crate::uquery::environment::QueryLiterals;
use crate::uquery::environment::UqueryDelegate;

pub(crate) mod aquery;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum LiteralParserError {
    #[error("Expected a target pattern without providers, got: `{0}`")]
    ExpectingTargetPatternWithoutProviders(String),
}

pub(crate) struct LiteralParser {
    // file and target literals are resolved relative to the working dir.
    working_dir: CellPath,
    working_dir_abs: AbsNormPathBuf,
    project_root: ProjectRoot,
    cell_resolver: CellResolver,
    cell_alias_resolver: CellAliasResolver,
    target_alias_resolver: BuckConfigTargetAliasResolver,
}

impl LiteralParser {
    // We allow provider names and flavors in the value and it gets stripped out for the result as queries operate on the target graphs.
    fn parse_target_pattern(
        &self,
        value: &str,
    ) -> buck2_error::Result<ParsedPattern<TargetPatternExtra>> {
        let providers_pattern = self.parse_providers_pattern(value)?;
        let target_pattern = match providers_pattern {
            ParsedPattern::Target(package, target_name, ProvidersPatternExtra { providers }) => {
                if providers != ProvidersName::Default {
                    // After converting this to hard error, replace this function body
                    // with direct call to `ParsedPattern::parse_relative`,
                    // as `parse_providers_pattern` does.
                    soft_error!(
                        "expecting_target_pattern_without_providers",
                        LiteralParserError::ExpectingTargetPatternWithoutProviders(
                            value.to_owned()
                        )
                        .into()
                    )?;
                }
                ParsedPattern::Target(package, target_name, TargetPatternExtra)
            }
            ParsedPattern::Package(package) => ParsedPattern::Package(package),
            ParsedPattern::Recursive(path) => ParsedPattern::Recursive(path),
        };
        Ok(target_pattern)
    }

    pub(crate) fn parse_providers_pattern(
        &self,
        value: &str,
    ) -> buck2_error::Result<ParsedPattern<ProvidersPatternExtra>> {
        ParsedPattern::parse_not_relaxed(
            value,
            TargetParsingRel::AllowRelative(self.working_dir.as_ref(), &self.target_alias_resolver),
            &self.cell_resolver,
            &self.cell_alias_resolver,
        )
    }

    fn parse_file_literal(&self, literal: &str) -> buck2_error::Result<CellPath> {
        parse_query_file_literal(
            literal,
            &self.cell_alias_resolver,
            &self.cell_resolver,
            &self.working_dir_abs,
            &self.project_root,
        )
    }
}

/// A Uquery delegate that resolves TargetNodes with the provided
/// InterpreterCalculation.
pub(crate) struct DiceQueryDelegate<'c, 'd> {
    ctx: &'c LinearRecomputeDiceComputations<'d>,
    query_data: Arc<DiceQueryData>,
}

pub(crate) struct DiceQueryData {
    literal_parser: LiteralParser,
    global_cfg_options: GlobalCfgOptions,
}

impl DiceQueryData {
    pub(crate) fn new(
        global_cfg_options: GlobalCfgOptions,
        cell_resolver: CellResolver,
        cell_alias_resolver: CellAliasResolver,
        working_dir: &ProjectRelativePath,
        project_root: ProjectRoot,
        target_alias_resolver: BuckConfigTargetAliasResolver,
    ) -> buck2_error::Result<Self> {
        let cell_path = cell_resolver.get_cell_path(working_dir)?;

        let working_dir_abs = project_root.resolve(working_dir);

        Ok(Self {
            literal_parser: LiteralParser {
                working_dir_abs,
                working_dir: cell_path,
                project_root,
                cell_resolver,
                cell_alias_resolver,
                target_alias_resolver,
            },
            global_cfg_options,
        })
    }

    pub(crate) fn literal_parser(&self) -> &LiteralParser {
        &self.literal_parser
    }

    pub(crate) fn global_cfg_options(&self) -> &GlobalCfgOptions {
        &self.global_cfg_options
    }
}

impl<'c, 'd> DiceQueryDelegate<'c, 'd> {
    pub(crate) fn new(
        ctx: &'c LinearRecomputeDiceComputations<'d>,
        query_data: Arc<DiceQueryData>,
    ) -> Self {
        Self { ctx, query_data }
    }

    pub(crate) fn ctx<'x>(&'x self) -> DiceComputations<'x> {
        self.ctx.get()
    }

    pub(crate) fn query_data(&self) -> &Arc<DiceQueryData> {
        &self.query_data
    }
}

#[async_trait]
impl<'c, 'd> UqueryDelegate for DiceQueryDelegate<'c, 'd> {
    // get the list of potential buildfile names for each cell
    async fn get_buildfile_names_by_cell(
        &self,
    ) -> buck2_error::Result<HashMap<CellName, Arc<[FileNameBuf]>>> {
        let mut ctx = self.ctx.get();
        let resolver = ctx.get_cell_resolver().await?;
        let buildfiles = ctx
            .try_compute_join(resolver.cells(), |ctx, (name, _)| {
                async move {
                    DiceFileComputations::buildfiles(ctx, name)
                        .await
                        .map(|x| (name, x))
                }
                .boxed()
            })
            .await?;

        Ok(buildfiles.into_iter().collect())
    }

    async fn resolve_target_patterns(
        &self,
        patterns: &[&str],
    ) -> buck2_error::Result<ResolvedPattern<TargetPatternExtra>> {
        let parsed_patterns =
            patterns.try_map(|p| self.query_data.literal_parser.parse_target_pattern(p))?;
        Ok(ResolveTargetPatterns::resolve(&mut self.ctx.get(), &parsed_patterns).await?)
    }

    // This returns 1 package normally but can return multiple packages if the path is covered under `self.package_boundary_exceptions`.
    async fn get_enclosing_packages(
        &self,
        path: &CellPath,
    ) -> buck2_error::Result<Vec<PackageLabel>> {
        // Without package boundary violations, there is only 1 owning package for a path.
        // However, with package boundary violations, all parent packages of the enclosing package can also be owners.
        if let Some(enclosing_violation_path) = self
            .ctx
            .get()
            .get_package_boundary_exception(path.as_ref())
            .await?
        {
            return Ok(DicePackageListingResolver(&mut self.ctx.get())
                .get_enclosing_packages(path.as_ref(), (*enclosing_violation_path).as_ref())
                .await?
                .into_iter()
                .collect());
        }

        let package = DicePackageListingResolver(&mut self.ctx.get())
            .get_enclosing_package(path.as_ref())
            .await?;
        Ok(vec![package])
    }

    async fn eval_file_literal(&self, literal: &str) -> buck2_error::Result<FileSet> {
        let cell_path = self.query_data.literal_parser.parse_file_literal(literal)?;
        Ok(FileSet::new(indexset![FileNode(cell_path)]))
    }

    fn linear_dice_computations(&self) -> &LinearRecomputeDiceComputations<'_> {
        self.ctx
    }

    fn ctx<'a>(&'a self) -> DiceComputations<'a> {
        self.ctx.get()
    }
}

#[async_trait]
impl<'c, 'd> CqueryDelegate for DiceQueryDelegate<'c, 'd> {
    fn uquery_delegate(&self) -> &dyn UqueryDelegate {
        self
    }

    async fn get_node_for_configured_target(
        &self,
        target: &ConfiguredTargetLabel,
    ) -> buck2_error::Result<ConfiguredTargetNode> {
        Ok(self
            .ctx
            .get()
            .get_configured_target_node(target)
            .await?
            .require_compatible()?)
    }

    async fn get_node_for_default_configured_target(
        &self,
        target: &TargetLabel,
    ) -> buck2_error::Result<MaybeCompatible<ConfiguredTargetNode>> {
        let target = self.ctx.get().get_default_configured_target(target).await?;
        self.ctx.get().get_configured_target_node(&target).await
    }

    fn ctx<'a>(&'a self) -> DiceComputations<'a> {
        self.ctx.get()
    }
}

#[async_trait]
impl QueryLiterals<ConfiguredTargetNode> for DiceQueryData {
    async fn eval_literals(
        &self,
        literals: &[&str],
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<TargetSet<ConfiguredTargetNode>> {
        let parsed_patterns = literals.try_map(|p| self.literal_parser.parse_target_pattern(p))?;
        Ok(load_compatible_patterns(
            ctx,
            parsed_patterns,
            &self.global_cfg_options,
            MissingTargetBehavior::Fail,
        )
        .await?)
    }
}

#[async_trait]
impl QueryLiterals<TargetNode> for DiceQueryData {
    async fn eval_literals(
        &self,
        literals: &[&str],
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<TargetSet<TargetNode>> {
        let parsed_patterns = literals.try_map(|p| self.literal_parser.parse_target_pattern(p))?;
        let loaded_patterns =
            load_patterns(ctx, parsed_patterns, MissingTargetBehavior::Fail).await?;
        let mut target_set = TargetSet::new();
        for (_package, results) in loaded_patterns.into_iter() {
            target_set.extend(results?.into_values());
        }
        Ok(target_set)
    }
}

pub(crate) async fn get_dice_query_delegate<'a, 'c: 'a, 'd>(
    ctx: &'c LinearRecomputeDiceComputations<'d>,
    working_dir: &'a ProjectRelativePath,
    global_cfg_options: GlobalCfgOptions,
) -> buck2_error::Result<DiceQueryDelegate<'c, 'd>> {
    let cell_resolver = ctx.get().get_cell_resolver().await?;
    let cell_alias_resolver = ctx
        .get()
        .get_cell_alias_resolver_for_dir(working_dir)
        .await?;
    let target_alias_resolver = ctx.get().target_alias_resolver().await?;
    let project_root = ctx
        .get()
        .global_data()
        .get_io_provider()
        .project_root()
        .to_owned();
    Ok(DiceQueryDelegate::new(
        ctx,
        Arc::new(DiceQueryData::new(
            global_cfg_options,
            cell_resolver,
            cell_alias_resolver,
            working_dir,
            project_root,
            target_alias_resolver,
        )?),
    ))
}
