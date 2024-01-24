/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the cli and query_* attr query language.
use buck2_error::Context;
use buck2_query_parser::placeholder::QUERY_PERCENT_S_PLACEHOLDER;
use futures::Future;
use indexmap::IndexMap;

use crate::query::environment::QueryTarget;
use crate::query::syntax::simple::eval::set::TargetSet;
use crate::query::syntax::simple::eval::values::QueryEvaluationValue;

/// Used to represent the results for a "multi-query" (one that contains a "%s" and potentially is applied against multiple literals).
pub struct MultiQueryResult<T: QueryTarget>(
    pub IndexMap<String, anyhow::Result<QueryEvaluationValue<T>>>,
);

impl<T: QueryTarget> MultiQueryResult<T> {
    pub fn merged(self) -> anyhow::Result<QueryEvaluationValue<T>> {
        let mut iter = self.0.into_iter();
        let (first_literal, mut results) = match iter.next() {
            Some((literal, value)) => (literal, value?),
            None => {
                return Ok(QueryEvaluationValue::TargetSet(TargetSet::new()));
            }
        };
        for (name, value) in iter {
            let value = value?;
            match (value, &mut results) {
                (
                    QueryEvaluationValue::TargetSet(value),
                    QueryEvaluationValue::TargetSet(results),
                ) => results.extend(&value),
                (QueryEvaluationValue::FileSet(value), QueryEvaluationValue::FileSet(results)) => {
                    results.insert_all(&value)
                }
                _ => unreachable!(
                    "no queries should return different types for different literals, but somehow that happened for `{}` and `{}`",
                    first_literal, name
                ),
            }
        }
        Ok(results)
    }
}

pub async fn process_multi_query<T, Fut, F, A: AsRef<str>>(
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
