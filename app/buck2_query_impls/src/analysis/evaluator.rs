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
use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::syntax::simple::eval::evaluator::QueryEvaluator;
use buck2_query::query::syntax::simple::eval::literals::extract_target_literals;
use buck2_query::query::syntax::simple::eval::multi_query::MultiQueryResult;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationValue;
use buck2_query::query::syntax::simple::functions::QueryFunctions;
use buck2_query_parser::placeholder::QUERY_PERCENT_S_PLACEHOLDER;
use futures::Future;
use gazebo::prelude::*;
use starlark::collections::SmallSet;

#[derive(Debug, buck2_error::Error)]
enum EvalQueryError {
    #[error("Query args supplied without any `%s` placeholder in the query, got args {}", .0.map(|x| format!("`{}`", x)).join(", "))]
    ArgsWithoutPlaceholder(Vec<String>),
    #[error("Placeholder `%s` in query argument `{0}`")]
    PlaceholderInPattern(String),
}

pub async fn eval_query<
    F: QueryFunctions<Env = Env>,
    Env: QueryEnvironment,
    Fut: Future<Output = anyhow::Result<Env>>,
    A: AsRef<str>,
>(
    functions: &F,
    query: &str,
    query_args: &[A],
    environment: impl FnOnce(Vec<String>) -> Fut,
) -> anyhow::Result<QueryEvaluationResult<Env::Target>> {
    let mut literals = SmallSet::new();
    if query.contains(QUERY_PERCENT_S_PLACEHOLDER) {
        // We'd really like the query args to only be literals (file or target).
        // If that didn't work, we'd really like query args to be well-formed expressions.
        // Unfortunately Buck1 just substitutes in arbitrarily strings, where the query
        // or query_args may not form anything remotely valid.
        // We have to be backwards compatible :(
        for q in query_args {
            let q = q.as_ref();
            if q.contains(QUERY_PERCENT_S_PLACEHOLDER) {
                return Err(EvalQueryError::PlaceholderInPattern(q.to_owned()).into());
            }
            extract_target_literals(
                functions,
                &query.replace(QUERY_PERCENT_S_PLACEHOLDER, q),
                &mut literals,
            )?;
        }
        // TODO(nga): we create one environment shared by all queries.
        //   This is fine for `uquery`, but for `cquery` if universe is inferred from arguments,
        //   it should be inferred only from current query, not from all the queries.
        let env = environment(literals.into_iter().collect()).await?;
        let results = process_multi_query(query, query_args, |input, query| {
            let evaluator = QueryEvaluator::new(&env, functions);
            async move { (input, evaluator.eval_query(&query).await) }
        })
        .await?;
        Ok(QueryEvaluationResult::Multiple(results))
    } else if !query_args.is_empty() {
        Err(
            EvalQueryError::ArgsWithoutPlaceholder(query_args.map(|s| s.as_ref().to_owned()))
                .into(),
        )
    } else {
        extract_target_literals(functions, query, &mut literals)?;
        let env = environment(literals.into_iter().collect()).await?;
        Ok(QueryEvaluationResult::Single(
            QueryEvaluator::new(&env, functions)
                .eval_query(query)
                .await?,
        ))
    }
}

async fn process_multi_query<T, Fut, F, A: AsRef<str>>(
    query: &str,
    query_args: &[A],
    func: F,
) -> anyhow::Result<MultiQueryResult<T>>
where
    T: QueryTarget,
    Fut: Future<Output = (String, anyhow::Result<QueryEvaluationValue<T>>)> + Send,
    F: Fn(String, String) -> Fut,
{
    // SAFETY: it is safe as long as we don't forget the future. We don't do that.
    let ((), future_results) = unsafe {
        async_scoped::TokioScope::scope_and_collect(|scope| {
            for (i, arg) in query_args.iter().enumerate() {
                let input = arg.as_ref().to_owned();
                let query: String = query.replace(QUERY_PERCENT_S_PLACEHOLDER, &input);
                let input_1 = input.clone();
                let fut = func(input_1, query);
                scope.spawn_cancellable(
                    async move {
                        let (query, result) = fut.await;
                        (i, query, result)
                    },
                    move || (i, input, Err(anyhow::anyhow!("future was cancelled"))),
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
