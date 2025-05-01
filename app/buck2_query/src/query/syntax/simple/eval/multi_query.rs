/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the cli and query_* attr query language.

use std::iter;

use dupe::Dupe;
use indexmap::IndexMap;
use itertools::Either;

use crate::query::environment::QueryTarget;
use crate::query::syntax::simple::eval::set::TargetSet;
use crate::query::syntax::simple::eval::values::QueryEvaluationValue;

/// Used to represent the results for a "multi-query" (one that contains a "%s" and potentially is applied against multiple literals).
pub struct MultiQueryResult<T: QueryTarget>(
    pub IndexMap<String, buck2_error::Result<QueryEvaluationValue<T>>>,
);

impl<T: QueryTarget> MultiQueryResult<T> {
    pub fn merged(self) -> buck2_error::Result<QueryEvaluationValue<T>> {
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

    pub(crate) fn targets(&self) -> impl Iterator<Item = buck2_error::Result<&T>> {
        self.0.values().flat_map(|r| match r {
            Ok(v) => Either::Left(v.targets()),
            Err(e) => Either::Right(iter::once(Err(e.dupe()))),
        })
    }
}
