/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the cli and query_* attr query language.

use buck2_query_parser::spanned::Spanned;
use gazebo::variants::VariantName;

use crate::query::environment::QueryTarget;
use crate::query::syntax::simple::eval::error::QueryError;
use crate::query::syntax::simple::eval::file_set::FileSet;
use crate::query::syntax::simple::eval::multi_query::MultiQueryResult;
use crate::query::syntax::simple::eval::set::TargetSet;

pub enum QueryEvaluationResult<T: QueryTarget> {
    Single(QueryEvaluationValue<T>),
    Multiple(MultiQueryResult<T>),
}

/// Used as a value in query evaluation, may appear in arguments to functions, results of functions etc.
#[derive(Debug, VariantName, Eq, PartialEq)]
pub enum QueryValue<T: QueryTarget> {
    String(String),
    Integer(u64),
    TargetSet(TargetSet<T>),
    FileSet(FileSet),
}

/// Used as the final result of evaluating a query. A literal at the top-level is treated specially and so this has
/// a more limited set of possibilities than a general QueryValue (for example `//foo/...` becomes a TargetSet in
/// `buck query //foo/...` rather than being a String).
#[derive(Debug, VariantName)]
pub enum QueryEvaluationValue<T: QueryTarget> {
    TargetSet(TargetSet<T>),
    FileSet(FileSet),
}

impl<T: QueryTarget> QueryEvaluationValue<T> {
    pub fn try_into_targets(self) -> anyhow::Result<TargetSet<T>> {
        match self {
            QueryEvaluationValue::TargetSet(targets) => Ok(targets),
            v => {
                return Err(QueryError::InvalidType {
                    expected: "targets",
                    actual: v.variant_name(),
                }
                .into());
            }
        }
    }
}

pub type QueryResult<T> = Result<Spanned<T>, Spanned<QueryError>>;

pub trait QueryResultExt<T> {
    fn into_anyhow(self, query: &str) -> anyhow::Result<T>;
}

impl<T> QueryResultExt<T> for QueryResult<T> {
    fn into_anyhow(self, query: &str) -> anyhow::Result<T> {
        match self {
            Ok(v) => Ok(v.value),
            Err(err) => Err(QueryError::convert_error(err, query)),
        }
    }
}

impl<T: QueryTarget> From<TargetSet<T>> for QueryValue<T> {
    fn from(v: TargetSet<T>) -> Self {
        QueryValue::TargetSet(v)
    }
}

impl<T: QueryTarget> From<FileSet> for QueryValue<T> {
    fn from(v: FileSet) -> Self {
        QueryValue::FileSet(v)
    }
}
