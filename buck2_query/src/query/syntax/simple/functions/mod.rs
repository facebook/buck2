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
use buck2_query_parser::{spanned::Spanned, BinaryOp, Expr};
use buck2_query_proc_macro::query_module;
use gazebo::variants::VariantName;

use crate::query::{
    environment::QueryEnvironment,
    syntax::simple::{
        eval::{
            error::QueryError,
            evaluator::QueryEvaluator,
            file_set::FileSet,
            set::{TargetSet, TargetSetExt},
            values::{QueryResult, QueryValue},
        },
        functions::{
            deps::DepsFunction,
            docs::ModuleDescription,
            helpers::{CapturedExpr, QueryArgType, QueryBinaryOp, QueryFunction},
        },
    },
};

pub mod deps;
pub mod docs;
pub mod helpers;

pub trait QueryLiteralVisitor {
    fn target_pattern(&mut self, pattern: &str) -> anyhow::Result<()>;
}

pub trait HasModuleDescription {
    fn describe() -> ModuleDescription;
}

#[async_trait]
pub trait QueryFunctions<Env: QueryEnvironment>: Send + Sync {
    fn get(&self, name: &str) -> Option<&dyn QueryFunction<Env>>;

    fn get_op(&self, op: BinaryOp) -> Option<&dyn QueryBinaryOp<Env>>;
}

pub trait QueryFunctionsExt<Env: QueryEnvironment> {
    fn visit_literals(
        &self,
        visitor: &mut dyn QueryLiteralVisitor,
        expr: &Spanned<Expr>,
    ) -> QueryResult<()>;
}

impl<Env: QueryEnvironment, F: QueryFunctions<Env>> QueryFunctionsExt<Env> for F {
    fn visit_literals(
        &self,
        visitor: &mut dyn QueryLiteralVisitor,
        expr: &Spanned<Expr>,
    ) -> QueryResult<()> {
        fn visit_literals_recurse<Env: QueryEnvironment, F: QueryFunctions<Env>>(
            this: &F,
            visitor: &mut dyn QueryLiteralVisitor,
            expr: &Expr,
        ) -> Result<(), QueryError> {
            match expr {
                Expr::Function {
                    function_name,
                    args,
                } => match this.get(function_name) {
                    Some(func) => {
                        for (i, arg) in args.iter().enumerate() {
                            visit_literals_item(
                                this,
                                visitor,
                                arg,
                                matches!(func.arg_type(i)?, QueryArgType::TargetSet),
                            )?;
                        }
                        Ok(())
                    }
                    None => Err(QueryError::UnknownFunction(
                        (*function_name.fragment()).to_owned(),
                    )),
                },
                Expr::BinaryOpSequence(left, exprs) => {
                    visit_literals_item(this, visitor, left, true)?;
                    // All binary ops are on targetsets currently.
                    for (_, right) in exprs {
                        visit_literals_item(this, visitor, right, true)?;
                    }
                    Ok(())
                }
                Expr::Set(args) => {
                    for arg in args {
                        visitor.target_pattern(arg)?;
                    }
                    Ok(())
                }
                Expr::String(..) | Expr::Integer(..) => {
                    panic!(
                        "This shouldn't be called with literals, they should be handled in the caller"
                    )
                }
            }
        }

        fn visit_literals_item<Env: QueryEnvironment, F: QueryFunctions<Env>>(
            this: &F,
            visitor: &mut dyn QueryLiteralVisitor,
            expr: &Spanned<Expr>,
            is_target_expr: bool,
        ) -> QueryResult<()> {
            expr.map_res(|value| -> Result<(), QueryError> {
                match value {
                    Expr::String(val) => {
                        if is_target_expr {
                            visitor.target_pattern(val)?;
                        }
                    }
                    Expr::Integer(..) => {
                        // ignored
                    }
                    _ => visit_literals_recurse(this, visitor, value)?,
                }
                Ok(())
            })
        }

        visit_literals_item(self, visitor, expr, true)
    }
}

pub struct DefaultQueryFunctions<Env: QueryEnvironment> {
    _marker: std::marker::PhantomData<Env>,
}

impl<Env: QueryEnvironment> DefaultQueryFunctions<Env> {
    pub fn new() -> Self {
        Self {
            _marker: PhantomData,
        }
    }
}

type QueryFuncResult<Env> =
    std::result::Result<QueryValue<<Env as QueryEnvironment>::Target>, QueryError>;

async fn accept_target_set<Env: QueryEnvironment>(
    env: &Env,
    val: QueryValue<Env::Target>,
) -> Result<TargetSet<Env::Target>, QueryError> {
    match val {
        QueryValue::TargetSet(x) => Ok(x),
        QueryValue::String(literal) => Ok(env.eval_literals(&[&literal]).await?),
        _ => Err(QueryError::InvalidType {
            expected: "target_set",
            actual: val.variant_name(),
        }),
    }
}

/// Common query functions
#[query_module(Env)]
impl<Env: QueryEnvironment> DefaultQueryFunctions<Env> {
    /// Computes all dependency paths.
    ///
    /// The `allpaths(from, to)` function evaluates to the graph formed by paths between the target expressions from and to, following the dependencies between nodes. For example, the value of
    ///
    /// ```ignore
    /// buck query "allpaths('//foo:bar', '//foo/bar/lib:baz')"
    /// ```
    /// is the dependency graph rooted at the single target node `//foo:bar`, that includes all target nodes that depend on `//foo/bar/lib:baz`.
    ///
    /// The two arguments to `allpaths()` can themselves be expressions. For example, the command:
    ///
    /// ```ignore
    /// buck query "allpaths(kind(java_library, '//...'), '//foo:bar')"
    /// ```
    ///
    /// shows all the paths between any java_library in the repository and the target `//foo:bar`.
    ///
    /// We recommend using `allpaths()` with the `--output-format=dot` parameter to generate a graphviz file that can then be rendered as an image. For example:
    ///
    /// ```ignore
    /// $ buck query "allpaths('//foo:bar', '//foo/bar/lib:baz')" --output-format=dot --output-file=result.dot
    /// $ dot -Tpng result.dot -o image.png
    /// ```
    /// Graphviz is an open-source graph-visualization software tool. Graphviz uses the dot language to describe graphs.
    async fn allpaths(
        &self,
        env: &Env,
        from: TargetSet<Env::Target>,
        to: TargetSet<Env::Target>,
    ) -> QueryFuncResult<Env> {
        Ok(env.allpaths(&from, &to).await?.into())
    }

    async fn somepath(&self) -> QueryFuncResult<Env> {
        Err(QueryError::FunctionUnimplemented("somepath"))
    }

    async fn attrfilter(
        &self,
        attr: String,
        value: String,
        targets: TargetSet<Env::Target>,
    ) -> QueryFuncResult<Env> {
        Ok(targets.attrfilter(&attr, &|v| Ok(v == value))?.into())
    }

    async fn nattrfilter(
        &self,
        attr: String,
        value: String,
        targets: TargetSet<Env::Target>,
    ) -> QueryFuncResult<Env> {
        Ok(targets.nattrfilter(&attr, &|v| Ok(v == value))?.into())
    }

    async fn attrregexfilter(
        &self,
        attr: String,
        value: String,
        targets: TargetSet<Env::Target>,
    ) -> QueryFuncResult<Env> {
        Ok(targets.attrregexfilter(&attr, &value)?.into())
    }

    async fn buildfile(&self, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(targets.buildfile().into())
    }

    async fn deps(
        &self,
        evaluator: &QueryEvaluator<'_, Env>,
        targets: TargetSet<Env::Target>,
        depth: Option<u64>,
        captured_expr: Option<CapturedExpr<'_>>,
    ) -> QueryFuncResult<Env> {
        DepsFunction::<Env> {
            _marker: PhantomData,
        }
        .invoke_deps(evaluator, targets, depth, captured_expr)
        .await
    }

    async fn filter(&self, regex: String, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(targets.filter_name(&regex)?.into())
    }

    async fn inputs(&self, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(targets.inputs()?.into())
    }

    async fn kind(&self, regex: String, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(targets.kind(&regex)?.into())
    }

    async fn labels(&self, _targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Err(QueryError::FunctionUnimplemented("labels"))
    }

    async fn owner(&self, env: &Env, files: FileSet) -> QueryFuncResult<Env> {
        Ok(env.owner(&files).await?.into())
    }

    async fn rdeps(
        &self,
        env: &Env,
        universe: TargetSet<Env::Target>,
        targets: TargetSet<Env::Target>,
        depth: Option<u64>,
    ) -> QueryFuncResult<Env> {
        Ok(env
            .rdeps(&universe, &targets, depth.map(|v| v as i32))
            .await?
            .into())
    }

    async fn testsof(&self, env: &Env, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(env.testsof(&targets).await?.into())
    }

    // These three functions are intentionally implemented as errors. They are only available within the context
    // of a deps functions 3rd parameter expr. When used in that context, the QueryFunctions will be augmented to
    // have non-erroring implementations.
    async fn first_order_deps(&self) -> QueryFuncResult<Env> {
        Err(QueryError::NotAvailableInContext("first_order_deps"))
    }
    async fn target_deps(&self) -> QueryFuncResult<Env> {
        Err(QueryError::NotAvailableInContext("target_deps"))
    }
    async fn exec_deps(&self) -> QueryFuncResult<Env> {
        Err(QueryError::NotAvailableInContext("exec_deps"))
    }

    #[binary_op(BinaryOp::Intersect)]
    async fn intersect(
        &self,
        env: &Env,
        left: QueryValue<Env::Target>,
        right: QueryValue<Env::Target>,
    ) -> Result<QueryValue<Env::Target>, QueryError> {
        let left = accept_target_set(env, left).await?;
        let right = accept_target_set(env, right).await?;
        Ok(QueryValue::TargetSet(left.intersect(&right)?))
    }

    #[binary_op(BinaryOp::Except)]
    async fn except(
        &self,
        env: &Env,
        left: QueryValue<Env::Target>,
        right: QueryValue<Env::Target>,
    ) -> Result<QueryValue<Env::Target>, QueryError> {
        let left = accept_target_set(env, left).await?;
        let right = accept_target_set(env, right).await?;
        Ok(QueryValue::TargetSet(left.difference(&right)?))
    }

    #[binary_op(BinaryOp::Union)]
    async fn union(
        &self,
        env: &Env,
        left: QueryValue<Env::Target>,
        right: QueryValue<Env::Target>,
    ) -> Result<QueryValue<Env::Target>, QueryError> {
        let left = accept_target_set(env, left).await?;
        let right = accept_target_set(env, right).await?;
        Ok(QueryValue::TargetSet(left.union(&right)))
    }
}
pub struct AugmentedQueryFunctions<'a, Env: QueryEnvironment> {
    inner: &'a dyn QueryFunctions<Env>,
    extra: Box<dyn QueryFunctions<Env> + 'a>,
}

impl<'a, Env: QueryEnvironment> AugmentedQueryFunctions<'a, Env> {
    pub fn augment(
        inner: &'a dyn QueryFunctions<Env>,
        extra: Box<dyn QueryFunctions<Env> + 'a>,
    ) -> Self {
        Self { inner, extra }
    }
}

impl<'a, Env: QueryEnvironment> QueryFunctions<Env> for AugmentedQueryFunctions<'a, Env> {
    fn get(&self, name: &str) -> Option<&dyn QueryFunction<Env>> {
        match self.extra.get(name) {
            None => self.inner.get(name),
            Some(v) => Some(v),
        }
    }

    fn get_op(&self, op: BinaryOp) -> Option<&dyn QueryBinaryOp<Env>> {
        match self.extra.get_op(op) {
            None => self.inner.get_op(op),
            Some(v) => Some(v),
        }
    }
}
