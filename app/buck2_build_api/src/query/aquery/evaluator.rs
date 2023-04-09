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

use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::target::label::TargetLabel;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use dice::DiceComputations;
use dupe::Dupe;

use crate::query::analysis::evaluator::eval_query;
use crate::query::aquery::environment::ActionQueryNode;
use crate::query::aquery::environment::AqueryEnvironment;
use crate::query::dice::aquery::DiceAqueryDelegate;
use crate::query::dice::get_dice_query_delegate;
use crate::query::uquery::environment::PreresolvedQueryLiterals;

pub struct AqueryEvaluator<'c> {
    dice_query_delegate: Arc<DiceAqueryDelegate<'c>>,
    functions: DefaultQueryFunctionsModule<AqueryEnvironment<'c>>,
}

impl AqueryEvaluator<'_> {
    pub async fn eval_query(
        &self,
        query: &str,
        query_args: &[String],
    ) -> anyhow::Result<QueryEvaluationResult<ActionQueryNode>> {
        eval_query(&self.functions, query, query_args, async move |literals| {
            let resolved_literals =
                PreresolvedQueryLiterals::pre_resolve(&*self.dice_query_delegate, &literals).await;
            Ok(AqueryEnvironment::new(
                self.dice_query_delegate.dupe(),
                Arc::new(resolved_literals),
            ))
        })
        .await
    }
}

/// Evaluates some query expression. TargetNodes are resolved via the interpreter from
/// the provided DiceCtx.
pub async fn get_aquery_evaluator<'a, 'c: 'a>(
    ctx: &'c DiceComputations,
    working_dir: &'a ProjectRelativePath,
    global_target_platform: Option<TargetLabel>,
) -> anyhow::Result<AqueryEvaluator<'c>> {
    let dice_query_delegate =
        get_dice_aquery_delegate(ctx, working_dir, global_target_platform).await?;
    let functions = DefaultQueryFunctionsModule::new();
    Ok(AqueryEvaluator {
        dice_query_delegate,
        functions,
    })
}

// Provides the dice query delegate for aquery evaluator
pub async fn get_dice_aquery_delegate<'a, 'c: 'a>(
    ctx: &'c DiceComputations,
    working_dir: &'a ProjectRelativePath,
    global_target_platform: Option<TargetLabel>,
) -> anyhow::Result<Arc<DiceAqueryDelegate<'c>>> {
    let dice_query_delegate =
        get_dice_query_delegate(ctx, working_dir, global_target_platform).await?;
    let dice_query_delegate = Arc::new(DiceAqueryDelegate::new(dice_query_delegate).await?);
    Ok(dice_query_delegate)
}
