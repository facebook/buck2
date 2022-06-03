/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;

use async_trait::async_trait;
use buck2_query_proc_macro::query_module;
use gazebo::variants::VariantName;

// Required for use of #[query_module] within the buck_query crate.
use crate::query::{
    environment::{QueryEnvironment, QueryTarget, TraversalFilter},
    syntax::simple::{
        eval::{
            error::QueryError,
            evaluator::QueryEvaluator,
            set::TargetSet,
            values::{QueryEvaluationValue, QueryValue},
        },
        functions::{helpers::CapturedExpr, AugmentedQueryFunctions, QueryFunctions},
    },
};

pub(crate) struct DepsContextFunctions<'a, Env: QueryEnvironment> {
    target: &'a Env::Target,
}

#[query_module(Env)]
impl<'a, Env: QueryEnvironment> DepsContextFunctions<'a, Env> {
    async fn first_order_deps(&self, env: &Env) -> Result<QueryValue<Env::Target>, QueryError> {
        let mut deps = TargetSet::new();
        for dep in self.target.deps() {
            deps.insert(env.get_node(dep).await?);
        }
        Ok(QueryValue::TargetSet(deps))
    }

    async fn exec_deps(&self, env: &Env) -> Result<QueryValue<Env::Target>, QueryError> {
        let mut deps = TargetSet::new();
        for dep in self.target.exec_deps() {
            deps.insert(env.get_node(dep).await?);
        }
        Ok(QueryValue::TargetSet(deps))
    }

    async fn target_deps(&self, env: &Env) -> Result<QueryValue<Env::Target>, QueryError> {
        let mut deps = TargetSet::new();
        for dep in self.target.target_deps() {
            deps.insert(env.get_node(dep).await?);
        }
        Ok(QueryValue::TargetSet(deps))
    }
}

pub(crate) struct DepsFunction<Env: QueryEnvironment> {
    pub(crate) _marker: PhantomData<Env>,
}

impl<Env: QueryEnvironment> DepsFunction<Env> {
    pub(crate) async fn invoke_deps(
        &self,
        evaluator: &QueryEvaluator<'_, Env>,
        targets: &TargetSet<Env::Target>,
        depth: Option<i32>,
        captured_expr: Option<&CapturedExpr<'_>>,
    ) -> anyhow::Result<TargetSet<Env::Target>> {
        let filter = match captured_expr {
            Some(expr) => {
                struct Filter<'a, Env: QueryEnvironment> {
                    inner_env: &'a Env,
                    functions: &'a dyn QueryFunctions<Env>,
                    expr: &'a CapturedExpr<'a>,
                }

                #[async_trait]
                impl<'a, T: QueryTarget, Env: QueryEnvironment<Target = T>> TraversalFilter<T> for Filter<'a, Env> {
                    async fn get_children(&self, target: &T) -> anyhow::Result<TargetSet<T>> {
                        let augmented_functions = AugmentedQueryFunctions::augment(
                            self.functions,
                            box DepsContextFunctions { target },
                        );
                        let evaluator = QueryEvaluator::new(self.inner_env, &augmented_functions);
                        match evaluator.eval_parsed_query(self.expr.expr).await {
                            Ok(v) => match v.value {
                                QueryEvaluationValue::TargetSet(v) => Ok(v),
                                v => Err(QueryError::InvalidType {
                                    expected: "targets",
                                    actual: v.variant_name(),
                                }
                                .into()),
                            },
                            Err(e) => Err(QueryError::drop_spans(e)),
                        }
                    }
                }

                Some(Filter {
                    inner_env: evaluator.env(),
                    functions: evaluator.functions(),
                    expr,
                })
            }
            None => None,
        };

        let filter_ref = filter
            .as_ref()
            .map(|v| v as &dyn TraversalFilter<Env::Target>);

        evaluator.env().deps(targets, depth, filter_ref).await
    }
}
