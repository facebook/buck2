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

use buck2_build_api::query::oneshot::CqueryOwnerBehavior;
use buck2_common::events::HasEvents;
use buck2_events::dispatch::console_message;
use buck2_node::configured_universe::CqueryUniverse;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::StreamExt;
use gazebo::prelude::*;

use crate::analysis::evaluator::eval_query;
use crate::cquery::environment::CqueryEnvironment;
use crate::dice::DiceQueryData;
use crate::dice::DiceQueryDelegate;
use crate::uquery::environment::PreresolvedQueryLiterals;
use crate::uquery::environment::QueryLiterals;
use crate::uquery::environment::UqueryDelegate;

pub(crate) async fn eval_cquery(
    dice_query_delegate: DiceQueryDelegate<'_, '_>,
    owner_behavior: CqueryOwnerBehavior,
    query: &str,
    query_args: &[String],
    target_universe: Option<&[String]>,
) -> anyhow::Result<QueryEvaluationResult<ConfiguredTargetNode>> {
    let dispatcher = dice_query_delegate
        .ctx()
        .per_transaction_data()
        .get_dispatcher()
        .dupe();
    let functions = DefaultQueryFunctionsModule::new();
    let dice_query_delegate = &dice_query_delegate;
    eval_query(
        dispatcher,
        &functions,
        query,
        query_args,
        |literals| async move {
            let (universe, resolved_literals) = match target_universe {
                None => {
                    if literals.is_empty() {
                        console_message(
                        "Query has no target literals and `--target-universe` is not specified.\n\
                        Such query is correct, but the result is always empty.\n\
                        Consider specifying `--target-universe` for this query\n\
                        or using `uquery` instead of `cquery`".to_owned());
                    }
                    // In the absence of a user-provided target universe, we use the target
                    // literals in the cquery as the universe.
                    resolve_literals_in_universe(&dice_query_delegate, &literals, &literals).await?
                }
                Some(universe) => {
                    resolve_literals_in_universe(&dice_query_delegate, &literals, universe).await?
                }
            };
            Ok(CqueryEnvironment::new(
                dice_query_delegate,
                Arc::new(resolved_literals),
                Some(universe),
                owner_behavior,
            ))
        },
    )
    .await
}

pub(crate) async fn preresolve_literals_and_build_universe(
    dice_query_delegate: &DiceQueryDelegate<'_, '_>,
    dice_query_data: &DiceQueryData,
    literals: &[String],
) -> anyhow::Result<(
    CqueryUniverse,
    PreresolvedQueryLiterals<ConfiguredTargetNode>,
)> {
    let resolved_literals = PreresolvedQueryLiterals::pre_resolve(
        dice_query_data,
        literals,
        &mut dice_query_delegate.ctx(),
    )
    .await;
    let universe = CqueryUniverse::build(&resolved_literals.literals()?)?;
    Ok((universe, resolved_literals))
}

// This will first resolve the universe to configured nodes and then gather all
// the deps. From there, it resolves the literals to any matching nodes in the universe deps.
async fn resolve_literals_in_universe(
    dice_query_delegate: &DiceQueryDelegate<'_, '_>,
    literals: &[String],
    universe: &[String],
) -> anyhow::Result<(
    CqueryUniverse,
    PreresolvedQueryLiterals<ConfiguredTargetNode>,
)> {
    let query_literals = dice_query_delegate.query_data();

    // TODO(cjhopman): We should probably also resolve the literals to TargetNode so that
    // we can get errors for packages or targets that don't exist or fail to load.
    let refs: Vec<_> = universe.map(|v| v.as_str());
    let universe_resolved = query_literals
        .eval_literals(&refs, &mut dice_query_delegate.ctx())
        .await?;

    let universe = CqueryUniverse::build(&universe_resolved)?;

    // capture a reference so the ref can be moved into the future below.
    let universe_ref = &universe;

    // TODO(cjhopman): Using the default resolution for recursive literals is inefficient.
    // If we can have a package-trie or cellpath-trie we can do the resolution directly
    // against the universe.
    let resolution_futs: FuturesUnordered<_> = literals
        .iter()
        .map(|lit| async move {
            let result: anyhow::Result<_> = try {
                let resolved_pattern = dice_query_delegate
                    .resolve_target_patterns(&[lit.as_str()])
                    .await?;
                universe_ref.get(&resolved_pattern)
            };

            (lit.to_owned(), result.map_err(buck2_error::Error::from))
        })
        .collect();

    let resolved = resolution_futs.collect().await;
    Ok((universe, PreresolvedQueryLiterals::new(resolved)))
}
