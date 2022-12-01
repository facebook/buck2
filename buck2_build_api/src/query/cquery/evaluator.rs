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

use buck2_common::result::ToSharedResultExt;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::target::TargetLabel;
use buck2_events::dispatch::console_message;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use dice::DiceComputations;
use futures::stream::FuturesUnordered;
use futures::StreamExt;
use gazebo::prelude::*;

use crate::query::analysis::evaluator::eval_query;
use crate::query::cquery::environment::CqueryEnvironment;
use crate::query::cquery::environment::CqueryOwnerBehavior;
use crate::query::cquery::universe::CqueryUniverse;
use crate::query::dice::get_dice_query_delegate;
use crate::query::dice::DiceQueryDelegate;
use crate::query::uquery::environment::PreresolvedQueryLiterals;
use crate::query::uquery::environment::QueryLiterals;
use crate::query::uquery::environment::UqueryDelegate;

pub struct CqueryEvaluator<'c> {
    dice_query_delegate: Arc<DiceQueryDelegate<'c>>,
    functions: DefaultQueryFunctionsModule<CqueryEnvironment<'c>>,
    owner_behavior: CqueryOwnerBehavior,
}

impl CqueryEvaluator<'_> {
    pub async fn eval_query<A: AsRef<str>, U: AsRef<str>>(
        &self,
        query: &str,
        query_args: &[A],
        target_universe: Option<&[U]>,
    ) -> anyhow::Result<QueryEvaluationResult<ConfiguredTargetNode>> {
        eval_query(&self.functions, query, query_args, async move |literals| {
            let (universe, resolved_literals) = match target_universe {
                None => {
                    if literals.is_empty() {
                        console_message(
                            "Query has no target literals and `--target-universe` is not specified.\n\
                            Such query is correct, but the result is always empty.\n\
                            Consider specifying `--target-universe` for this query\n\
                            or using `uquery` instead of `cquery`".to_owned());
                    }
                    preresolve_literals_and_build_universe(&self.dice_query_delegate, &literals)
                        .await?
                }
                Some(universe) => {
                    resolve_literals_in_universe(&self.dice_query_delegate, &literals, universe)
                        .await?
                }
            };
            Ok(CqueryEnvironment::new(
                self.dice_query_delegate.dupe(),
                Arc::new(resolved_literals),
                Some(universe),
                self.owner_behavior,
            ))
        })
        .await
    }
}

async fn preresolve_literals_and_build_universe(
    dice_query_delegate: &DiceQueryDelegate<'_>,
    literals: &[String],
) -> anyhow::Result<(
    CqueryUniverse,
    PreresolvedQueryLiterals<ConfiguredTargetNode>,
)> {
    let resolved_literals =
        PreresolvedQueryLiterals::pre_resolve(dice_query_delegate, literals).await;
    let universe = CqueryUniverse::build(&resolved_literals.literals()?).await?;
    Ok((universe, resolved_literals))
}

pub async fn universe_from_literals(
    dice_query_delegate: &DiceQueryDelegate<'_>,
    literals: &[String],
) -> anyhow::Result<CqueryUniverse> {
    Ok(
        preresolve_literals_and_build_universe(dice_query_delegate, literals)
            .await?
            .0,
    )
}

/// Evaluates some query expression. TargetNodes are resolved via the interpreter from
/// the provided DiceCtx.
pub async fn get_cquery_evaluator<'a, 'c: 'a>(
    ctx: &'c DiceComputations,
    working_dir: &'a ProjectRelativePath,
    global_target_platform: Option<TargetLabel>,
    owner_behavior: CqueryOwnerBehavior,
) -> anyhow::Result<CqueryEvaluator<'c>> {
    let dice_query_delegate =
        Arc::new(get_dice_query_delegate(ctx, working_dir, global_target_platform).await?);
    let functions = DefaultQueryFunctionsModule::new();
    Ok(CqueryEvaluator {
        dice_query_delegate,
        functions,
        owner_behavior,
    })
}

// This will first resolve the universe to configured nodes and then gather all
// the deps. From there, it resolves the literals to any matching nodes in the universe deps.
async fn resolve_literals_in_universe<L: AsRef<str>, U: AsRef<str>>(
    dice_query_delegate: &DiceQueryDelegate<'_>,
    literals: &[L],
    universe: &[U],
) -> anyhow::Result<(
    CqueryUniverse,
    PreresolvedQueryLiterals<ConfiguredTargetNode>,
)> {
    // TODO(cjhopman): We should probably also resolve the literals to TargetNode so that
    // we can get errors for packages or targets that don't exist or fail to load.
    let refs: Vec<_> = universe.map(|v| v.as_ref());
    let universe_resolved = dice_query_delegate.eval_literals(&refs).await?;

    let universe = CqueryUniverse::build(&universe_resolved).await?;

    // capture a reference so the ref can be moved into the future below.
    let universe_ref = &universe;

    // TODO(cjhopman): Using the default resolution for recursive literals is inefficient.
    // If we can have a package-trie or cellpath-trie we can do the resolution directly
    // against the universe.
    let resolution_futs: FuturesUnordered<_> = literals
        .iter()
        .map(|lit| async move {
            let lit = lit.as_ref();
            let result: anyhow::Result<_> = try {
                let resolved_pattern = dice_query_delegate.resolve_target_patterns(&[lit]).await?;
                universe_ref.get(&resolved_pattern)
            };

            (lit.to_owned(), result.shared_error())
        })
        .collect();

    let resolved = resolution_futs.collect().await;
    Ok((universe, PreresolvedQueryLiterals::new(resolved)))
}
