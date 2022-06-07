/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the cli and query_* attr query language.
use futures::{stream::FuturesOrdered, Future, StreamExt};
use indexmap::IndexMap;

use crate::query::{
    environment::QueryTarget,
    syntax::simple::eval::{set::TargetSet, values::QueryEvaluationValue},
};

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
                ) => results.insert_all(&value),
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

pub async fn process_multi_query<T, Fut, F>(
    query: &str,
    query_args: Vec<String>,
    func: F,
) -> MultiQueryResult<T>
where
    T: QueryTarget,
    Fut: Future<Output = (String, anyhow::Result<QueryEvaluationValue<T>>)>,
    F: Fn(String, String) -> Fut,
{
    let mut queue: FuturesOrdered<_> = query_args
        .iter()
        .map(|input| {
            let query = query.replace("%s", input);
            let input = input.to_owned();
            func(input, query)
        })
        .collect();

    let mut results = IndexMap::new();
    while let Some((query, result)) = queue.next().await {
        results.insert(query, result);
    }
    MultiQueryResult(results)
}
