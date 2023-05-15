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
use buck2_build_api::query::bxl::BxlCqueryFunctions;
use buck2_build_api::query::bxl::NEW_BXL_CQUERY_FUNCTIONS;
use buck2_build_api::query::oneshot::CqueryOwnerBehavior;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::target::label::TargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::compatibility::MaybeCompatible;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::functions::helpers::CapturedExpr;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctions;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use dice::DiceComputations;
use dupe::Dupe;

use crate::cquery::environment::CqueryEnvironment;
use crate::dice::DiceQueryDelegate;

fn cquery_functions<'v>() -> DefaultQueryFunctions<CqueryEnvironment<'v>> {
    DefaultQueryFunctions::new()
}

struct BxlCqueryFunctionsImpl<'c> {
    ctx: &'c DiceComputations,
    target_platform: Option<TargetLabel>,
    project_root: ProjectRoot,
    working_dir: ProjectRelativePathBuf,
}

impl<'c> BxlCqueryFunctionsImpl<'c> {
    async fn cquery_env(&self) -> anyhow::Result<CqueryEnvironment<'c>> {
        let cell_resolver = self.ctx.get_cell_resolver().await?;

        let package_boundary_exceptions = self.ctx.get_package_boundary_exceptions().await?;
        let target_alias_resolver = self
            .ctx
            .target_alias_resolver_for_working_dir(&self.working_dir)
            .await?;

        let dice_query_delegate = Arc::new(DiceQueryDelegate::new(
            self.ctx,
            &self.working_dir,
            self.project_root.dupe(),
            cell_resolver,
            self.target_platform.dupe(),
            package_boundary_exceptions,
            target_alias_resolver,
        )?);
        Ok(CqueryEnvironment::new(
            dice_query_delegate.dupe(),
            dice_query_delegate,
            // TODO(nga): add universe.
            None,
            // TODO(nga): use proper owner behavior.
            CqueryOwnerBehavior::Deprecated,
        ))
    }
}

#[async_trait]
impl<'c> BxlCqueryFunctions<'c> for BxlCqueryFunctionsImpl<'c> {
    async fn allpaths(
        &self,
        from: &TargetSet<ConfiguredTargetNode>,
        to: &TargetSet<ConfiguredTargetNode>,
    ) -> anyhow::Result<TargetSet<ConfiguredTargetNode>> {
        Ok(cquery_functions()
            .allpaths(&self.cquery_env().await?, from, to)
            .await?)
    }

    async fn somepath(
        &self,
        from: &TargetSet<ConfiguredTargetNode>,
        to: &TargetSet<ConfiguredTargetNode>,
    ) -> anyhow::Result<TargetSet<ConfiguredTargetNode>> {
        Ok(cquery_functions()
            .somepath(&self.cquery_env().await?, from, to)
            .await?)
    }

    async fn owner(&self, file_set: &FileSet) -> anyhow::Result<TargetSet<ConfiguredTargetNode>> {
        Ok(cquery_functions()
            .owner(&self.cquery_env().await?, file_set)
            .await?)
    }

    async fn deps(
        &self,
        targets: &TargetSet<ConfiguredTargetNode>,
        deps: Option<i32>,
        captured_expr: Option<&CapturedExpr>,
    ) -> anyhow::Result<TargetSet<ConfiguredTargetNode>> {
        Ok(cquery_functions()
            .deps(
                &self.cquery_env().await?,
                &DefaultQueryFunctionsModule::new(),
                targets,
                deps,
                captured_expr,
            )
            .await?)
    }

    async fn rdeps(
        &self,
        universe: &TargetSet<ConfiguredTargetNode>,
        targets: &TargetSet<ConfiguredTargetNode>,
        depth: Option<i32>,
    ) -> anyhow::Result<TargetSet<ConfiguredTargetNode>> {
        Ok(cquery_functions()
            .rdeps(&self.cquery_env().await?, universe, targets, depth)
            .await?)
    }

    async fn testsof(
        &self,
        targets: &TargetSet<ConfiguredTargetNode>,
    ) -> anyhow::Result<TargetSet<ConfiguredTargetNode>> {
        Ok(cquery_functions()
            .testsof(&self.cquery_env().await?, targets)
            .await?)
    }

    async fn testsof_with_default_target_platform(
        &self,
        targets: &TargetSet<ConfiguredTargetNode>,
    ) -> anyhow::Result<Vec<MaybeCompatible<ConfiguredTargetNode>>> {
        Ok(cquery_functions()
            .testsof_with_default_target_platform(&self.cquery_env().await?, targets)
            .await?)
    }
}

pub(crate) fn init_new_bxl_cquery_functions() {
    NEW_BXL_CQUERY_FUNCTIONS.init(|ctx, target_platform, project_root, cell_name| {
        Box::pin(async move {
            let cell_resolver = ctx.get_cell_resolver().await?;
            let cell = cell_resolver.get(cell_name)?;
            // TODO(nga): working as as cell root is not right.
            //   Should be either the project root or user's current working directory.
            let working_dir = cell.path().as_project_relative_path().to_buf();

            Result::<Box<dyn BxlCqueryFunctions>, _>::Ok(Box::new(BxlCqueryFunctionsImpl {
                ctx,
                target_platform,
                project_root,
                working_dir,
            }))
        })
    })
}
