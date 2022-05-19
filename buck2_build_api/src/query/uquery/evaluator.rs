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

use buck2_core::{
    fs::{paths::AbsPathBuf, project::ProjectRelativePathBuf},
    target::TargetLabel,
};
use buck2_query::query::syntax::simple::{
    eval::values::QueryEvaluationResult, functions::DefaultQueryFunctions,
};
use dice::DiceComputations;
use gazebo::prelude::*;

use crate::{
    nodes::unconfigured::TargetNode,
    query::{
        analysis::evaluator::eval_query,
        dice::{get_dice_query_delegate, DiceQueryDelegate},
        uquery::environment::{PreresolvedQueryLiterals, UqueryEnvironment},
    },
};

pub struct UqueryEvaluator<'c> {
    dice_query_delegate: Arc<DiceQueryDelegate<'c>>,
    functions: DefaultQueryFunctions<UqueryEnvironment<'c>>,
}

impl UqueryEvaluator<'_> {
    pub async fn eval_query(
        &self,
        query: &str,
        query_args: Vec<String>,
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
    working_dir: ProjectRelativePathBuf,
    project_root: AbsPathBuf,
    global_target_platform: Option<TargetLabel>,
) -> anyhow::Result<UqueryEvaluator<'c>> {
    let dice_query_delegate = Arc::new(
        get_dice_query_delegate(ctx, working_dir, project_root, global_target_platform).await?,
    );
    let functions = DefaultQueryFunctions::new();

    Ok(UqueryEvaluator {
        dice_query_delegate,
        functions,
    })
}
