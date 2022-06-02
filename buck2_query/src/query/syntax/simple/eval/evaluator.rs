/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the cli and query_* attr query language.

use buck2_query_parser::{parse_expr, spanned::Spanned, Expr};
use futures::FutureExt;
use gazebo::{prelude::*, variants::VariantName};

use crate::query::{
    environment::QueryEnvironment,
    syntax::simple::{
        eval::{
            error::QueryError,
            set::TargetSet,
            values::{QueryEvaluationValue, QueryResult, QueryValue},
        },
        functions::QueryFunctions,
    },
};
pub struct QueryEvaluator<'e, Env: QueryEnvironment> {
    env: &'e Env,
    functions: &'e dyn QueryFunctions<Env>,
}

impl<'e, Env: QueryEnvironment> QueryEvaluator<'e, Env> {
    pub fn new(env: &'e Env, functions: &'e dyn QueryFunctions<Env>) -> Self {
        Self { env, functions }
    }

    pub fn env(&self) -> &Env {
        self.env
    }

    pub fn functions(&self) -> &dyn QueryFunctions<Env> {
        self.functions
    }

    async fn resolve_literal(&self, literal: &str) -> anyhow::Result<TargetSet<Env::Target>> {
        self.env.eval_literals(&[literal]).await
    }

    pub async fn eval_targets(
        &self,
        expr: &Spanned<Expr<'_>>,
    ) -> QueryResult<TargetSet<Env::Target>> {
        self.eval(expr)
            .await?
            .async_into_map_res(async move |value| match value {
                QueryValue::TargetSet(val) => Ok(val),
                QueryValue::String(literal) => Ok(self.resolve_literal(&literal).await?),
                _ => Err(QueryError::InvalidType {
                    expected: "targets",
                    actual: value.variant_name(),
                }),
            })
            .await
    }

    pub fn eval_integer(&self, expr: &Spanned<Expr>) -> QueryResult<u64> {
        expr.map_res(|expr| match expr {
            Expr::Integer(val) => Ok(*val),
            _ => Err(QueryError::ExpectedLiteral {
                actual: expr.variant_name(),
            }),
        })
    }

    pub fn eval_string<'a>(&self, expr: &'a Spanned<Expr>) -> QueryResult<&'a str> {
        expr.map_res(|expr| match expr {
            Expr::String(val) => Ok(*val),
            _ => Err(QueryError::ExpectedLiteral {
                actual: expr.variant_name(),
            }),
        })
    }

    async fn eval_internal(&self, expr: &Expr<'_>) -> Result<QueryValue<Env::Target>, QueryError> {
        // TODO(cjhopman): We should extract these functions to a map of name->functionobj and attach
        // more information to them like documentation and signature. Potentially we could generalize
        // the function impls there to work across bql and here, but not sure if that's worth the
        // complexity or not.
        match expr {
            Expr::String(word) => Ok(QueryValue::String((*word).to_owned())),
            Expr::Integer(value) => Ok(QueryValue::Integer(*value)),
            Expr::Function {
                function_name,
                args,
            } => match self.functions.get(function_name) {
                Some(func) => func.invoke(self, args).await,
                None => Err(QueryError::UnknownFunction(
                    (*function_name.fragment()).to_owned(),
                )),
            },
            Expr::BinaryOpSequence(left, exprs) => {
                let mut value = self.eval(left).await?.value;
                for (op, right) in exprs {
                    let right = self.eval(right).await?;
                    value = right
                        .async_into_map_res(|right| async move {
                            match self.functions.get_op(*op) {
                                Some(func) => func.invoke(self.env(), value, right).await,
                                None => Err(QueryError::UnsupportedBinaryOp(op.to_string())),
                            }
                        })
                        .await?
                        .value;
                }
                Ok(value)
            }
            Expr::Set(args) => {
                let patterns: Vec<_> = args.map(|v| v.fragment());
                // TODO(cjhopman): evaluating the literals in this way does not preserver the ordering from
                // the user, instead the result will be package-ordered. We may need to change this to
                // preserve order.
                Ok(self.env.eval_literals(&patterns).await?.into())
            }
        }
    }

    pub fn eval<'a>(
        &'a self,
        expr: &'a Spanned<Expr<'a>>,
    ) -> std::pin::Pin<
        Box<dyn std::future::Future<Output = QueryResult<QueryValue<Env::Target>>> + Send + 'a>,
    > {
        async move { expr.span(self.eval_internal(&expr.value).await) }.boxed()
    }

    pub async fn eval_query<'a>(
        &self,
        query: &str,
    ) -> anyhow::Result<QueryEvaluationValue<Env::Target>> {
        let parsed_query = parse_expr(query)?;
        match self.eval_parsed_query(&parsed_query).await {
            Ok(v) => Ok(v.value),
            Err(e) => Err(QueryError::convert_error(e, query)),
        }
    }

    pub async fn eval_parsed_query<'a>(
        &self,
        expr: &Spanned<Expr<'a>>,
    ) -> QueryResult<QueryEvaluationValue<Env::Target>> {
        self.eval(expr)
            .await?
            .async_into_map_res(async move |value| {
                match value {
                    // A top-level string we treat as a target pattern and resolve it. This allows something like
                    // `buck2 query //lib/...` to resolve to the corresponding targets.
                    QueryValue::String(word) => Ok(QueryEvaluationValue::TargetSet(
                        self.resolve_literal(&word).await?,
                    )),
                    QueryValue::TargetSet(targets) => Ok(QueryEvaluationValue::TargetSet(targets)),
                    QueryValue::FileSet(files) => Ok(QueryEvaluationValue::FileSet(files)),
                    _ => Err(QueryError::InvalidType {
                        expected: "targets",
                        actual: value.variant_name(),
                    }),
                }
            })
            .await
    }
}
