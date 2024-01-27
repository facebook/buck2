/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of common cquery/uquery pieces.

use anyhow::Context;
use buck2_common::scope::scope_and_collect_with_dispatcher;
use buck2_events::dispatch::EventDispatcher;
use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::syntax::simple::eval::evaluator::QueryEvaluator;
use buck2_query::query::syntax::simple::eval::literals::extract_target_literals;
use buck2_query::query::syntax::simple::eval::multi_query::MultiQueryResult;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_query::query::syntax::simple::functions::QueryFunctions;
use buck2_query_parser::multi_query::MaybeMultiQuery;
use buck2_query_parser::multi_query::MultiQueryItem;
use futures::Future;
use starlark::collections::SmallSet;

pub(crate) async fn eval_query<
    F: QueryFunctions<Env = Env>,
    Env: QueryEnvironment,
    Fut: Future<Output = anyhow::Result<Env>>,
    A: AsRef<str>,
>(
    dispatcher: EventDispatcher,
    functions: &F,
    query: &str,
    query_args: &[A],
    environment: impl FnOnce(Vec<String>) -> Fut,
) -> anyhow::Result<QueryEvaluationResult<Env::Target>> {
    let query = MaybeMultiQuery::parse(query, query_args)?;
    match query {
        MaybeMultiQuery::MultiQuery(queries) => {
            let results = process_multi_query(dispatcher, functions, environment, &queries).await?;
            Ok(QueryEvaluationResult::Multiple(results))
        }
        MaybeMultiQuery::SingleQuery(query) => {
            let mut literals = SmallSet::new();
            extract_target_literals(functions, &query, &mut literals)?;
            let env = environment(literals.into_iter().collect()).await?;
            Ok(QueryEvaluationResult::Single(
                QueryEvaluator::new(&env, functions)
                    .eval_query(&query)
                    .await?,
            ))
        }
    }
}

async fn process_multi_query<Env, EnvFut, Qf>(
    dispatcher: EventDispatcher,
    functions: &Qf,
    env: impl FnOnce(Vec<String>) -> EnvFut,
    queries: &[MultiQueryItem],
) -> anyhow::Result<MultiQueryResult<Env::Target>>
where
    Qf: QueryFunctions<Env = Env>,
    Env: QueryEnvironment,
    EnvFut: Future<Output = anyhow::Result<Env>>,
{
    let mut literals = SmallSet::new();
    for q in queries {
        extract_target_literals(functions, &q.query, &mut literals)?;
    }
    // TODO(nga): we create one environment shared by all queries.
    //   This is fine for `uquery`, but for `cquery` if universe is inferred from arguments,
    //   it should be inferred only from current query, not from all the queries.
    let env = env(literals.into_iter().collect()).await?;

    // SAFETY: it is safe as long as we don't forget the future. We don't do that.
    let ((), future_results) = unsafe {
        scope_and_collect_with_dispatcher(dispatcher, |scope| {
            for (i, query) in queries.iter().enumerate() {
                let arg: String = query.arg.clone();
                let arg_1: String = arg.clone();
                let evaluator = QueryEvaluator::new(&env, functions);
                scope.spawn_cancellable(
                    async move {
                        let result = evaluator.eval_query(&query.query).await;
                        (i, arg, result)
                    },
                    move || (i, arg_1, Err(anyhow::anyhow!("future was cancelled"))),
                )
            }
        })
        .await
    };

    let mut results = Vec::with_capacity(future_results.len());
    for query_result in future_results {
        let (i, query, result) = query_result.context("scope_and_collect failed")?;
        results.push((i, query, result));
    }
    results.sort_by_key(|(i, _, _)| *i);

    let map = results
        .into_iter()
        .map(|(_, query, result)| (query, result))
        .collect();
    Ok(MultiQueryResult(map))
}
