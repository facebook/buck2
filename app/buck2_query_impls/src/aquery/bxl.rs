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
use buck2_build_api::actions::query::ActionQueryNode;
use buck2_build_api::query::bxl::BxlAqueryFunctions;
use buck2_build_api::query::bxl::NEW_BXL_AQUERY_FUNCTIONS;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::target::label::TargetLabel;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::functions::helpers::CapturedExpr;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctions;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use dice::DiceComputations;
use dupe::Dupe;

use crate::aquery::environment::AqueryEnvironment;
use crate::dice::aquery::DiceAqueryDelegate;
use crate::dice::DiceQueryData;
use crate::dice::DiceQueryDelegate;

fn aquery_functions<'v>() -> DefaultQueryFunctions<AqueryEnvironment<'v>> {
    DefaultQueryFunctions::new()
}

struct BxlAqueryFunctionsImpl {
    target_platform: Option<TargetLabel>,
    project_root: ProjectRoot,
    working_dir: ProjectRelativePathBuf,
}

impl BxlAqueryFunctionsImpl {
    async fn aquery_delegate<'c>(
        &self,
        dice: &'c mut DiceComputations,
    ) -> anyhow::Result<Arc<DiceAqueryDelegate<'c>>> {
        let cell_resolver = dice.get_cell_resolver().await?;

        let package_boundary_exceptions = dice.get_package_boundary_exceptions().await?;
        let target_alias_resolver = dice
            .target_alias_resolver_for_working_dir(&self.working_dir)
            .await?;

        let query_data = Arc::new(DiceQueryData::new(
            self.target_platform.clone(),
            cell_resolver.dupe(),
            &self.working_dir,
            self.project_root.dupe(),
            target_alias_resolver,
        )?);
        let query_delegate =
            DiceQueryDelegate::new(dice, cell_resolver, package_boundary_exceptions, query_data);

        Ok(Arc::new(DiceAqueryDelegate::new(query_delegate).await?))
    }

    async fn aquery_env<'c>(
        &self,
        delegate: &Arc<DiceAqueryDelegate<'c>>,
    ) -> anyhow::Result<AqueryEnvironment<'c>> {
        let literals = delegate.query_data().dupe();
        Ok(AqueryEnvironment::new(delegate.dupe(), literals))
    }
}

#[async_trait]
impl BxlAqueryFunctions for BxlAqueryFunctionsImpl {
    async fn allpaths(
        &self,
        dice: &mut DiceComputations,
        from: &TargetSet<ActionQueryNode>,
        to: &TargetSet<ActionQueryNode>,
    ) -> anyhow::Result<TargetSet<ActionQueryNode>> {
        Ok(aquery_functions()
            .allpaths(
                &self.aquery_env(&self.aquery_delegate(dice).await?).await?,
                from,
                to,
            )
            .await?)
    }
    async fn somepath(
        &self,
        dice: &mut DiceComputations,
        from: &TargetSet<ActionQueryNode>,
        to: &TargetSet<ActionQueryNode>,
    ) -> anyhow::Result<TargetSet<ActionQueryNode>> {
        Ok(aquery_functions()
            .somepath(
                &self.aquery_env(&self.aquery_delegate(dice).await?).await?,
                from,
                to,
            )
            .await?)
    }
    async fn deps(
        &self,
        dice: &mut DiceComputations,
        targets: &TargetSet<ActionQueryNode>,
        deps: Option<i32>,
        captured_expr: Option<&CapturedExpr>,
    ) -> anyhow::Result<TargetSet<ActionQueryNode>> {
        Ok(aquery_functions()
            .deps(
                &self.aquery_env(&self.aquery_delegate(dice).await?).await?,
                &DefaultQueryFunctionsModule::new(),
                targets,
                deps,
                captured_expr,
            )
            .await?)
    }
    async fn rdeps(
        &self,
        dice: &mut DiceComputations,
        universe: &TargetSet<ActionQueryNode>,
        targets: &TargetSet<ActionQueryNode>,
        depth: Option<i32>,
    ) -> anyhow::Result<TargetSet<ActionQueryNode>> {
        Ok(aquery_functions()
            .rdeps(
                &self.aquery_env(&self.aquery_delegate(dice).await?).await?,
                universe,
                targets,
                depth,
            )
            .await?)
    }
    async fn testsof(
        &self,
        dice: &mut DiceComputations,
        targets: &TargetSet<ActionQueryNode>,
    ) -> anyhow::Result<TargetSet<ActionQueryNode>> {
        Ok(aquery_functions()
            .testsof(
                &self.aquery_env(&self.aquery_delegate(dice).await?).await?,
                targets,
            )
            .await?)
    }
    async fn owner(
        &self,
        dice: &mut DiceComputations,
        file_set: &FileSet,
    ) -> anyhow::Result<TargetSet<ActionQueryNode>> {
        Ok(aquery_functions()
            .owner(
                &self.aquery_env(&self.aquery_delegate(dice).await?).await?,
                file_set,
            )
            .await?)
    }
}

pub(crate) fn init_new_bxl_aquery_functions() {
    NEW_BXL_AQUERY_FUNCTIONS.init(|target_platform, project_root, cell_name, cell_resolver| {
        Box::pin(async move {
            let cell = cell_resolver.get(cell_name)?;
            let working_dir = cell.path().as_project_relative_path().to_buf();

            Result::<Box<dyn BxlAqueryFunctions>, _>::Ok(Box::new(BxlAqueryFunctionsImpl {
                target_platform,
                project_root,
                working_dir,
            }))
        })
    })
}
