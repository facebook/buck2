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
use buck2_build_api::query::bxl::BxlUqueryFunctions;
use buck2_build_api::query::bxl::NEW_BXL_UQUERY_FUNCTIONS;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::functions::helpers::CapturedExpr;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctions;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use dice::DiceComputations;
use dupe::Dupe;

use crate::dice::DiceQueryDelegate;
use crate::uquery::environment::UqueryEnvironment;

fn uquery_functions<'v>() -> DefaultQueryFunctions<UqueryEnvironment<'v>> {
    DefaultQueryFunctions::new()
}

struct BxlUqueryFunctionsImpl {
    project_root: ProjectRoot,
    working_dir: ProjectRelativePathBuf,
}

impl BxlUqueryFunctionsImpl {
    async fn uquery_env<'c>(
        &self,
        dice: &'c DiceComputations,
    ) -> anyhow::Result<UqueryEnvironment<'c>> {
        let cell_resolver = dice.get_cell_resolver().await?;

        let package_boundary_exceptions = dice.get_package_boundary_exceptions().await?;
        let target_alias_resolver = dice
            .target_alias_resolver_for_working_dir(&self.working_dir)
            .await?;

        let dice_query_delegate = Arc::new(DiceQueryDelegate::new(
            dice,
            &self.working_dir,
            self.project_root.dupe(),
            cell_resolver,
            None,
            package_boundary_exceptions,
            target_alias_resolver,
        )?);
        Ok(UqueryEnvironment::new(
            dice_query_delegate.dupe(),
            dice_query_delegate,
        ))
    }
}

#[async_trait]
impl BxlUqueryFunctions for BxlUqueryFunctionsImpl {
    async fn allpaths(
        &self,
        dice: &DiceComputations,
        from: &TargetSet<TargetNode>,
        to: &TargetSet<TargetNode>,
    ) -> anyhow::Result<TargetSet<TargetNode>> {
        Ok(uquery_functions()
            .allpaths(&self.uquery_env(dice).await?, from, to)
            .await?)
    }
    async fn somepath(
        &self,
        dice: &DiceComputations,
        from: &TargetSet<TargetNode>,
        to: &TargetSet<TargetNode>,
    ) -> anyhow::Result<TargetSet<TargetNode>> {
        Ok(uquery_functions()
            .somepath(&self.uquery_env(dice).await?, from, to)
            .await?)
    }
    async fn deps(
        &self,
        dice: &DiceComputations,
        targets: &TargetSet<TargetNode>,
        deps: Option<i32>,
        captured_expr: Option<&CapturedExpr>,
    ) -> anyhow::Result<TargetSet<TargetNode>> {
        Ok(uquery_functions()
            .deps(
                &self.uquery_env(dice).await?,
                &DefaultQueryFunctionsModule::new(),
                targets,
                deps,
                captured_expr,
            )
            .await?)
    }
    async fn rdeps(
        &self,
        dice: &DiceComputations,
        universe: &TargetSet<TargetNode>,
        targets: &TargetSet<TargetNode>,
        depth: Option<i32>,
    ) -> anyhow::Result<TargetSet<TargetNode>> {
        Ok(uquery_functions()
            .rdeps(&self.uquery_env(dice).await?, universe, targets, depth)
            .await?)
    }
    async fn testsof(
        &self,
        dice: &DiceComputations,
        targets: &TargetSet<TargetNode>,
    ) -> anyhow::Result<TargetSet<TargetNode>> {
        Ok(uquery_functions()
            .testsof(&self.uquery_env(dice).await?, targets)
            .await?)
    }
    async fn owner(
        &self,
        dice: &DiceComputations,
        file_set: &FileSet,
    ) -> anyhow::Result<TargetSet<TargetNode>> {
        Ok(uquery_functions()
            .owner(&self.uquery_env(dice).await?, file_set)
            .await?)
    }
}

pub(crate) fn init_new_bxl_uquery_functions() {
    NEW_BXL_UQUERY_FUNCTIONS.init(|project_root, cell_name, cell_resolver| {
        Box::pin(async move {
            let cell = cell_resolver.get(cell_name)?;
            // TODO(nga): working as as cell root is not right.
            //   Should be either the project root or user's current working directory.
            let working_dir = cell.path().as_project_relative_path().to_buf();

            Result::<Box<dyn BxlUqueryFunctions>, _>::Ok(Box::new(BxlUqueryFunctionsImpl {
                project_root,
                working_dir,
            }))
        })
    })
}
