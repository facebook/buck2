/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::target::label::TargetLabel;
use buck2_node::configured_universe::CqueryUniverse;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use dupe::Dupe;

use crate::actions::query::ActionQueryNode;

/// [Context](https://fburl.com/adiagq2f).
#[derive(Copy, Clone, Dupe)]
pub enum CqueryOwnerBehavior {
    Deprecated,
    Correct,
}

#[async_trait]
pub trait QueryFrontend: Send + Sync + 'static {
    async fn eval_uquery(
        &self,
        ctx: &DiceComputations,
        working_dir: &ProjectRelativePath,
        query: &str,
        query_args: &[String],
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<QueryEvaluationResult<TargetNode>>;

    async fn eval_cquery(
        &self,
        ctx: &DiceComputations,
        working_dir: &ProjectRelativePath,
        owner_behavior: CqueryOwnerBehavior,
        query: &str,
        query_args: &[String],
        global_target_platform: Option<TargetLabel>,
        target_universe: Option<&[String]>,
    ) -> anyhow::Result<QueryEvaluationResult<ConfiguredTargetNode>>;

    async fn eval_aquery(
        &self,
        ctx: &DiceComputations,
        working_dir: &ProjectRelativePath,
        query: &str,
        query_args: &[String],
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<QueryEvaluationResult<ActionQueryNode>>;

    async fn universe_from_literals(
        &self,
        ctx: &DiceComputations,
        cwd: &ProjectRelativePath,
        literals: &[String],
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<CqueryUniverse>;
}

pub static QUERY_FRONTEND: LateBinding<&'static dyn QueryFrontend> =
    LateBinding::new("QUERY_FRONTEND");
