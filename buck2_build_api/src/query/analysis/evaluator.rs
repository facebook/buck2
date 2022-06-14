/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of common cquery/uquery pieces.

use std::collections::HashSet;

use buck2_query::query::{
    environment::QueryEnvironment,
    syntax::simple::{
        eval::{
            evaluator::QueryEvaluator, literals::extract_target_literals,
            multi_query::process_multi_query, values::QueryEvaluationResult,
        },
        functions::DefaultQueryFunctionsModule,
    },
};
use futures::Future;
use gazebo::prelude::*;
use thiserror::Error;

#[derive(Debug, Error)]
enum EvalQueryError {
    #[error("Query args supplied without any `%s` placeholder in the query, got args {}", .0.map(|x| format!("`{}`", x)).join(", "))]
    ArgsWithoutPlaceholder(Vec<String>),
    #[error("Placeholder `%s` in query argument `{0}`")]
    PlaceholderInPattern(String),
}

pub async fn eval_query<Env: QueryEnvironment, Fut: Future<Output = anyhow::Result<Env>>>(
    functions: &DefaultQueryFunctionsModule<Env>,
    query: &str,
    query_args: Vec<String>,
    environment: impl FnOnce(Vec<String>) -> Fut,
) -> anyhow::Result<QueryEvaluationResult<Env::Target>> {
    let mut literals = HashSet::new();
    if query.contains("%s") {
        extract_target_literals(functions, query, &mut literals)?;
        for q in &query_args {
            if q.contains("%s") {
                return Err(EvalQueryError::PlaceholderInPattern(q.to_owned()).into());
            }
            extract_target_literals(functions, q, &mut literals)?;
        }
        let env = environment(literals.into_iter().collect()).await?;
        let results = process_multi_query(query, query_args, |input, query| {
            let evaluator = QueryEvaluator::new(&env, functions);
            async move { (input, evaluator.eval_query(&query).await) }
        })
        .await;
        Ok(QueryEvaluationResult::Multiple(results))
    } else if !query_args.is_empty() {
        Err(EvalQueryError::ArgsWithoutPlaceholder(query_args).into())
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
