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
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::package_boundary::PackageBoundaryExceptions;
use buck2_common::package_listing::dice::HasPackageListingResolver;
use buck2_common::package_listing::resolver::PackageListingResolver;
use buck2_common::pattern::resolve::resolve_target_patterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::query_file_literal::parse_query_file_literal;
use buck2_core::pattern::ParsedPattern;
use buck2_core::provider::label::ProvidersName;
use buck2_core::soft_error;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_core::target::label::TargetLabel;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_node::load_patterns::load_patterns;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_query::query::syntax::simple::eval::file_set::FileNode;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use dice::DiceComputations;
use dupe::Dupe;
use gazebo::prelude::*;
use indexmap::indexset;

use crate::cquery::environment::CqueryDelegate;
use crate::uquery::environment::QueryLiterals;
use crate::uquery::environment::UqueryDelegate;

pub mod aquery;

#[derive(Debug, thiserror::Error)]
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
    ) -> anyhow::Result<ParsedPattern<TargetPatternExtra>> {
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
    ) -> anyhow::Result<ParsedPattern<ProvidersPatternExtra>> {
        ParsedPattern::parse_relative(
            &self.target_alias_resolver,
            self.working_dir.as_ref(),
            value,
            &self.cell_resolver,
        )
    }

    fn parse_file_literal(&self, literal: &str) -> anyhow::Result<CellPath> {
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
pub struct DiceQueryDelegate<'c> {
    ctx: &'c DiceComputations,
    cell_resolver: CellResolver,
    literal_parser: Arc<LiteralParser>,
    global_target_platform: Option<TargetLabel>,
    package_boundary_exceptions: Arc<PackageBoundaryExceptions>,
}

impl<'c> DiceQueryDelegate<'c> {
    pub fn new<'a>(
        ctx: &'c DiceComputations,
        working_dir: &'a ProjectRelativePath,
        project_root: ProjectRoot,
        cell_resolver: CellResolver,
        global_target_platform: Option<TargetLabel>,
        package_boundary_exceptions: Arc<PackageBoundaryExceptions>,
        target_alias_resolver: BuckConfigTargetAliasResolver,
    ) -> anyhow::Result<Self> {
        let cell_path = cell_resolver.get_cell_path(working_dir)?;
        let cell_alias_resolver = cell_resolver
            .get(cell_path.cell())?
            .cell_alias_resolver()
            .dupe();
        let working_dir_abs = project_root.resolve(working_dir);

        Ok(Self {
            ctx,
            global_target_platform,
            cell_resolver: cell_resolver.dupe(),
            literal_parser: Arc::new(LiteralParser {
                working_dir_abs,
                working_dir: cell_path,
                project_root,
                cell_resolver,
                cell_alias_resolver,
                target_alias_resolver,
            }),
            package_boundary_exceptions,
        })
    }

    pub(crate) fn ctx(&self) -> &'c DiceComputations {
        self.ctx
    }

    pub(crate) fn literal_parser(&self) -> &LiteralParser {
        &self.literal_parser
    }

    pub(crate) fn global_target_platform(&self) -> Option<&TargetLabel> {
        self.global_target_platform.as_ref()
    }
}

#[async_trait]
impl<'c> UqueryDelegate for DiceQueryDelegate<'c> {
    async fn eval_build_file(
        &self,
        package: PackageLabel,
    ) -> anyhow::Result<Arc<EvaluationResult>> {
        self.ctx.get_interpreter_results(package).await
    }

    async fn eval_module_imports(&self, path: &ImportPath) -> anyhow::Result<Vec<ImportPath>> {
        //TODO(benfoxman): Don't need to get the whole module, just parse the imports.
        let module = self.ctx.get_loaded_module_from_import_path(path).await?;
        Ok(module.imports().cloned().collect())
    }

    // get the list of potential buildfile names for each cell
    fn get_buildfile_names_by_cell(&self) -> anyhow::Result<HashMap<CellName, &[FileNameBuf]>> {
        let resolver = &self.cell_resolver;
        let mut buildfile_names_by_cell = HashMap::<CellName, &[FileNameBuf]>::new();
        for (cell, _) in resolver.cells() {
            let prev = buildfile_names_by_cell.insert(cell, resolver.get(cell)?.buildfiles());
            assert!(prev.is_none());
        }
        Ok(buildfile_names_by_cell)
    }

    async fn resolve_target_patterns(
        &self,
        patterns: &[&str],
    ) -> anyhow::Result<ResolvedPattern<TargetPatternExtra>> {
        let parsed_patterns = patterns.try_map(|p| self.literal_parser.parse_target_pattern(p))?;
        let file_ops = self.ctx.file_ops();
        resolve_target_patterns(&self.cell_resolver, &parsed_patterns, &file_ops).await
    }

    // This returns 1 package normally but can return multiple packages if the path is covered under `self.package_boundary_exceptions`.
    async fn get_enclosing_packages(&self, path: &CellPath) -> anyhow::Result<Vec<PackageLabel>> {
        let package_listing_resolver = self.ctx.get_package_listing_resolver();

        // Without package boundary violations, there is only 1 owning package for a path.
        // However, with package boundary violations, all parent packages of the enclosing package can also be owners.
        if let Some(enclosing_violation_path) = self
            .package_boundary_exceptions
            .get_package_boundary_exception_path(path)
        {
            return Ok(package_listing_resolver
                .get_enclosing_packages(path.as_ref(), enclosing_violation_path.as_ref())
                .await?
                .into_iter()
                .collect());
        }

        let package = package_listing_resolver
            .get_enclosing_package(path.as_ref())
            .await?;
        Ok(vec![package])
    }

    async fn eval_file_literal(&self, literal: &str) -> anyhow::Result<FileSet> {
        let cell_path = self.literal_parser.parse_file_literal(literal)?;
        Ok(FileSet::new(indexset![FileNode(cell_path)]))
    }
}

#[async_trait]
impl<'c> CqueryDelegate for DiceQueryDelegate<'c> {
    fn uquery_delegate(&self) -> &dyn UqueryDelegate {
        self
    }

    async fn get_node_for_target(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<MaybeCompatible<ConfiguredTargetNode>> {
        let target = self
            .ctx
            .get_configured_target(target, self.global_target_platform.as_ref())
            .await?;
        Ok(self.ctx.get_configured_target_node(&target).await?)
    }

    async fn get_node_for_configured_target(
        &self,
        target: &ConfiguredTargetLabel,
    ) -> anyhow::Result<ConfiguredTargetNode> {
        Ok(self
            .ctx
            .get_configured_target_node(target)
            .await?
            .require_compatible()?)
    }

    async fn get_node_for_default_configured_target(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<MaybeCompatible<ConfiguredTargetNode>> {
        let target = self.ctx.get_default_configured_target(target).await?;
        self.ctx.get_configured_target_node(&target).await
    }

    async fn get_configured_target(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<ConfiguredTargetLabel> {
        self.ctx
            .get_configured_target(target, self.global_target_platform.as_ref())
            .await
    }
}

#[async_trait]
impl<'c> QueryLiterals<ConfiguredTargetNode> for DiceQueryDelegate<'c> {
    async fn eval_literals(
        &self,
        literals: &[&str],
    ) -> anyhow::Result<TargetSet<ConfiguredTargetNode>> {
        let parsed_patterns = literals.try_map(|p| self.literal_parser.parse_target_pattern(p))?;
        load_compatible_patterns(
            self.ctx,
            parsed_patterns,
            self.global_target_platform.dupe(),
            MissingTargetBehavior::Fail,
        )
        .await
    }
}

#[async_trait]
impl<'c> QueryLiterals<TargetNode> for DiceQueryDelegate<'c> {
    async fn eval_literals(&self, literals: &[&str]) -> anyhow::Result<TargetSet<TargetNode>> {
        let parsed_patterns = literals.try_map(|p| self.literal_parser.parse_target_pattern(p))?;
        let loaded_patterns =
            load_patterns(self.ctx, parsed_patterns, MissingTargetBehavior::Fail).await?;
        let mut target_set = TargetSet::new();
        for (_package, results) in loaded_patterns.into_iter() {
            target_set.extend(results?.into_values());
        }
        Ok(target_set)
    }
}

pub(crate) async fn get_dice_query_delegate<'a, 'c: 'a>(
    ctx: &'c DiceComputations,
    working_dir: &'a ProjectRelativePath,
    global_target_platform: Option<TargetLabel>,
) -> anyhow::Result<DiceQueryDelegate<'c>> {
    let cell_resolver = ctx.get_cell_resolver().await?;
    let package_boundary_exceptions = ctx.get_package_boundary_exceptions().await?;
    let target_alias_resolver = ctx
        .target_alias_resolver_for_working_dir(working_dir)
        .await?;
    let project_root = ctx
        .global_data()
        .get_io_provider()
        .project_root()
        .to_owned();
    DiceQueryDelegate::new(
        ctx,
        working_dir,
        project_root,
        cell_resolver,
        global_target_platform,
        package_boundary_exceptions,
        target_alias_resolver,
    )
}
