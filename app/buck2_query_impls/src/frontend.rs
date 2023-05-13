/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_build_api::actions::query::ActionQueryNode;
use buck2_build_api::query::oneshot::CqueryOwnerBehavior;
use buck2_build_api::query::oneshot::QueryFrontend;
use buck2_build_api::query::oneshot::QUERY_FRONTEND;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::target::label::TargetLabel;
use buck2_node::configured_universe::CqueryUniverse;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use ctor::ctor;
use dice::DiceComputations;

use crate::aquery::evaluator::get_aquery_evaluator;
use crate::cquery::evaluator::get_cquery_evaluator;
use crate::cquery::evaluator::preresolve_literals_and_build_universe;
use crate::dice::get_dice_query_delegate;
use crate::uquery::evaluator::get_uquery_evaluator;

struct QueryFrontendImpl;

#[ctor]
fn init_query_frontend() {
    QUERY_FRONTEND.init(&QueryFrontendImpl);
}

#[async_trait]
impl QueryFrontend for QueryFrontendImpl {
    async fn eval_uquery(
        &self,
        ctx: &DiceComputations,
        working_dir: &ProjectRelativePath,
        query: &str,
        query_args: &[String],
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<QueryEvaluationResult<TargetNode>> {
        let evaluator = get_uquery_evaluator(ctx, working_dir, global_target_platform).await?;

        evaluator.eval_query(query, query_args).await
    }

    async fn eval_cquery(
        &self,
        ctx: &DiceComputations,
        working_dir: &ProjectRelativePath,
        owner_behavior: CqueryOwnerBehavior,
        query: &str,
        query_args: &[String],
        global_target_platform: Option<TargetLabel>,
        target_universe: Option<&[String]>,
    ) -> anyhow::Result<QueryEvaluationResult<ConfiguredTargetNode>> {
        let evaluator =
            get_cquery_evaluator(ctx, working_dir, global_target_platform, owner_behavior).await?;

        // TODO(nga): this should support configured target patterns
        //   similarly to what we do for `build` command.
        //   Something like this should work:
        //   ```
        //   buck2 cquery --target-universe android//:binary 'deps("some//:lib (<arm32>)")'
        //   ```
        evaluator
            .eval_query(query, query_args, target_universe.as_ref().map(|v| &v[..]))
            .await
    }

    async fn eval_aquery(
        &self,
        ctx: &DiceComputations,
        working_dir: &ProjectRelativePath,
        query: &str,
        query_args: &[String],
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<QueryEvaluationResult<ActionQueryNode>> {
        let evaluator = get_aquery_evaluator(ctx, working_dir, global_target_platform).await?;

        evaluator.eval_query(query, query_args).await
    }

    async fn universe_from_literals(
        &self,
        ctx: &DiceComputations,
        cwd: &ProjectRelativePath,
        literals: &[String],
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<CqueryUniverse> {
        let query_delegate = get_dice_query_delegate(ctx, cwd, global_target_platform).await?;
        Ok(
            preresolve_literals_and_build_universe(&query_delegate, literals)
                .await?
                .0,
        )
    }
}
