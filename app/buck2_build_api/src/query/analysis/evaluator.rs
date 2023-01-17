/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of common cquery/uquery pieces.

use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::syntax::simple::eval::evaluator::QueryEvaluator;
use buck2_query::query::syntax::simple::eval::literals::extract_target_literals;
use buck2_query::query::syntax::simple::eval::multi_query::process_multi_query;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use futures::Future;
use gazebo::prelude::*;
use starlark::collections::SmallSet;
use thiserror::Error;

#[derive(Debug, Error)]
enum EvalQueryError {
    #[error("Query args supplied without any `%s` placeholder in the query, got args {}", .0.map(|x| format!("`{}`", x)).join(", "))]
    ArgsWithoutPlaceholder(Vec<String>),
    #[error("Placeholder `%s` in query argument `{0}`")]
    PlaceholderInPattern(String),
}

pub async fn eval_query<
    Env: QueryEnvironment,
    Fut: Future<Output = anyhow::Result<Env>>,
    A: AsRef<str>,
>(
    functions: &DefaultQueryFunctionsModule<Env>,
    query: &str,
    query_args: &[A],
    environment: impl FnOnce(Vec<String>) -> Fut,
) -> anyhow::Result<QueryEvaluationResult<Env::Target>> {
    let mut literals = SmallSet::new();
    if query.contains("%s") {
        // We'd really like the query args to only be literals (file or target).
        // If that didn't work, we'd really like query args to be well-formed expressions.
        // Unfortunately Buck1 just substitutes in arbitrarily strings, where the query
        // or query_args may not form anything remotely valid.
        // We have to be backwards compatible :(
        for q in query_args {
            let q = q.as_ref();
            if q.contains("%s") {
                return Err(EvalQueryError::PlaceholderInPattern(q.to_owned()).into());
            }
            extract_target_literals(functions, &query.replace("%s", q), &mut literals)?;
        }
        let env = environment(literals.into_iter().collect()).await?;
        let results = process_multi_query(query, query_args, |input, query| {
            let evaluator = QueryEvaluator::new(&env, functions);
            async move { (input, evaluator.eval_query(&query).await) }
        })
        .await;
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
