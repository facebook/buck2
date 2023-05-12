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

use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_ref::ConfiguredGraphNodeRef;
use buck2_query::query::syntax::simple::eval::evaluator::QueryEvaluator;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use dice::DiceComputations;

use crate::query::analysis::configured_graph::AnalysisConfiguredGraphQueryDelegate;
use crate::query::analysis::configured_graph::AnalysisDiceQueryDelegate;
use crate::query::analysis::environment::ConfiguredGraphQueryEnvironment;

pub(crate) async fn eval_analysis_query(
    ctx: &DiceComputations,
    query: &str,
    resolved_literals: HashMap<String, ConfiguredTargetNode>,
) -> anyhow::Result<TargetSet<ConfiguredGraphNodeRef>> {
    let dice_query_delegate = Arc::new(AnalysisDiceQueryDelegate { ctx });
    let delegate = AnalysisConfiguredGraphQueryDelegate {
        dice_query_delegate,
        resolved_literals,
    };

    let functions = ConfiguredGraphQueryEnvironment::functions();
    let env = ConfiguredGraphQueryEnvironment::new(&delegate);
    let evaluator = QueryEvaluator::new(&env, &functions);

    let result = evaluator.eval_query(query).await?;
    result.try_into_targets()
}
