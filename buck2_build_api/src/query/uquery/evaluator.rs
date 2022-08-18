/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the cli and query_* attr query language.
use std::sync::Arc;

use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::target::TargetLabel;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use dice::DiceComputations;
use gazebo::prelude::*;

use crate::query::analysis::evaluator::eval_query;
use crate::query::dice::get_dice_query_delegate;
use crate::query::dice::DiceQueryDelegate;
use crate::query::uquery::environment::PreresolvedQueryLiterals;
use crate::query::uquery::environment::UqueryEnvironment;

pub struct UqueryEvaluator<'c> {
    dice_query_delegate: Arc<DiceQueryDelegate<'c>>,
    functions: DefaultQueryFunctionsModule<UqueryEnvironment<'c>>,
}

impl UqueryEvaluator<'_> {
    pub async fn eval_query(
        &self,
        query: &str,
        query_args: &[String],
    ) -> anyhow::Result<QueryEvaluationResult<TargetNode>> {
        eval_query(&self.functions, query, query_args, async move |literals| {
            let resolved_literals =
                PreresolvedQueryLiterals::pre_resolve(&*self.dice_query_delegate, &literals).await;
            Ok(UqueryEnvironment::new(
                self.dice_query_delegate.dupe(),
                Arc::new(resolved_literals),
            ))
        })
        .await
    }
}

/// Evaluates some query expression. TargetNodes are resolved via the interpreter from
/// the provided DiceCtx.
pub async fn get_uquery_evaluator<'c>(
    ctx: &'c DiceComputations,
    working_dir: &ProjectRelativePath,
    project_root: ProjectRoot,
    global_target_platform: Option<TargetLabel>,
) -> anyhow::Result<UqueryEvaluator<'c>> {
    let dice_query_delegate = Arc::new(
        get_dice_query_delegate(ctx, working_dir, project_root, global_target_platform).await?,
    );
    let functions = DefaultQueryFunctionsModule::new();

    Ok(UqueryEvaluator {
        dice_query_delegate,
        functions,
    })
}
