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
use buck2_query_derive::query_module;
use buck2_query_parser::spanned::Spanned;
use buck2_query_parser::BinaryOp;
use buck2_query_parser::Expr;
use gazebo::variants::VariantName;

use crate::query::environment::QueryEnvironment;
use crate::query::syntax::simple::eval::error::QueryError;
use crate::query::syntax::simple::eval::evaluator::QueryEvaluator;
use crate::query::syntax::simple::eval::file_set::FileSet;
use crate::query::syntax::simple::eval::set::TargetSet;
use crate::query::syntax::simple::eval::set::TargetSetExt;
use crate::query::syntax::simple::eval::values::QueryResult;
use crate::query::syntax::simple::eval::values::QueryValue;
use crate::query::syntax::simple::functions::deps::DepsFunction;
use crate::query::syntax::simple::functions::docs::ModuleDescription;
use crate::query::syntax::simple::functions::helpers::CapturedExpr;
use crate::query::syntax::simple::functions::helpers::QueryArgType;
use crate::query::syntax::simple::functions::helpers::QueryBinaryOp;
use crate::query::syntax::simple::functions::helpers::QueryFunction;

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
pub trait QueryFunctions: Send + Sync {
    type Env: QueryEnvironment;

    fn get(&self, name: &str) -> Option<&dyn QueryFunction<Self::Env>>;

    fn get_op(&self, op: BinaryOp) -> Option<&dyn QueryBinaryOp<Self::Env>>;
}

pub trait QueryFunctionsExt {
    fn visit_literals(
        &self,
        visitor: &mut dyn QueryLiteralVisitor,
        expr: &Spanned<Expr>,
    ) -> QueryResult<()>;
}

impl<F: QueryFunctions> QueryFunctionsExt for F {
    fn visit_literals(
        &self,
        visitor: &mut dyn QueryLiteralVisitor,
        expr: &Spanned<Expr>,
    ) -> QueryResult<()> {
        fn visit_literals_recurse<F: QueryFunctions>(
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
                Expr::FileSet(_args) => Ok(()),
                Expr::String(..) | Expr::Integer(..) => {
                    panic!(
                        "This shouldn't be called with literals, they should be handled in the caller"
                    )
                }
            }
        }

        fn visit_literals_item<F: QueryFunctions>(
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

pub struct DefaultQueryFunctionsModule<Env: QueryEnvironment> {
    implementation: DefaultQueryFunctions<Env>,
}

impl<Env: QueryEnvironment> DefaultQueryFunctionsModule<Env> {
    pub fn new() -> Self {
        Self {
            implementation: DefaultQueryFunctions::new(),
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
impl<Env: QueryEnvironment> DefaultQueryFunctionsModule<Env> {
    /// Computes all dependency paths.
    ///
    /// The `allpaths(from, to)` function evaluates to the graph formed by paths between the target expressions from and to, following the dependencies between nodes. For example, the value of
    /// `buck query "allpaths('//foo:bar', '//foo/bar/lib:baz')"`
    /// is the dependency graph rooted at the single target node `//foo:bar`, that includes all target nodes that depend (transitively) on `//foo/bar/lib:baz`.
    ///
    /// The two arguments to `allpaths()` can themselves be expressions. For example, the command:
    /// `buck query "allpaths(kind(java_library, '//...'), '//foo:bar')"`
    /// shows all the paths between any java_library in the repository and the target `//foo:bar`.
    ///
    /// We recommend using `allpaths()` with the `--output-format=dot` parameter to generate a graphviz file that can then be rendered as an image. For example:
    ///
    /// ```ignore
    /// $ buck query "allpaths('//foo:bar', '//foo/bar/lib:baz')" --output-format=dot --output-file=result.dot
    /// $ dot -Tpng result.dot -o image.png
    /// ```
    ///
    /// Graphviz is an open-source graph-visualization software tool. Graphviz uses the dot language to describe graphs.
    async fn allpaths(
        &self,
        env: &Env,
        from: TargetSet<Env::Target>,
        to: TargetSet<Env::Target>,
    ) -> QueryFuncResult<Env> {
        Ok(self.implementation.allpaths(env, &from, &to).await?.into())
    }

    async fn somepath(
        &self,
        env: &Env,
        from: TargetSet<Env::Target>,
        to: TargetSet<Env::Target>,
    ) -> QueryFuncResult<Env> {
        Ok(self.implementation.somepath(env, &from, &to).await?.into())
    }

    async fn attrfilter(
        &self,
        attr: String,
        value: String,
        targets: TargetSet<Env::Target>,
    ) -> QueryFuncResult<Env> {
        Ok(self
            .implementation
            .attrfilter(&attr, &value, &targets)?
            .into())
    }

    async fn nattrfilter(
        &self,
        attr: String,
        value: String,
        targets: TargetSet<Env::Target>,
    ) -> QueryFuncResult<Env> {
        Ok(self
            .implementation
            .nattrfilter(&attr, &value, &targets)?
            .into())
    }

    async fn attrregexfilter(
        &self,
        attr: String,
        value: String,
        targets: TargetSet<Env::Target>,
    ) -> QueryFuncResult<Env> {
        Ok(self
            .implementation
            .attrregexfilter(&attr, &value, &targets)?
            .into())
    }

    async fn buildfile(&self, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(self.implementation.buildfile(&targets).into())
    }

    async fn rbuildfiles(
        &self,
        env: &Env,
        universe: FileSet,
        argset: FileSet,
    ) -> QueryFuncResult<Env> {
        Ok(self
            .implementation
            .rbuildfiles(env, &universe, &argset)
            .await?
            .into())
    }

    async fn allbuildfiles(
        &self,
        env: &Env,
        universe: TargetSet<Env::Target>,
    ) -> QueryFuncResult<Env> {
        Ok(self
            .implementation
            .allbuildfiles(env, &universe)
            .await?
            .into())
    }

    async fn deps(
        &self,
        evaluator: &QueryEvaluator<'_, Env>,
        targets: TargetSet<Env::Target>,
        depth: Option<u64>,
        captured_expr: Option<CapturedExpr<'_>>,
    ) -> QueryFuncResult<Env> {
        Ok(self
            .implementation
            .deps(
                evaluator.env(),
                evaluator.functions(),
                &targets,
                depth.map(|v| v as i32),
                captured_expr.as_ref(),
            )
            .await?
            .into())
    }

    async fn filter(&self, regex: String, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(self.implementation.filter(&regex, &targets)?.into())
    }

    async fn inputs(&self, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(self.implementation.inputs(&targets)?.into())
    }

    async fn kind(&self, regex: String, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(self.implementation.kind(&regex, &targets)?.into())
    }

    async fn labels(&self, attr: String, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        self.implementation.labels(&attr, &targets)
    }

    async fn owner(&self, env: &Env, files: FileSet) -> QueryFuncResult<Env> {
        Ok(self.implementation.owner(env, &files).await?.into())
    }

    async fn rdeps(
        &self,
        env: &Env,
        universe: TargetSet<Env::Target>,
        targets: TargetSet<Env::Target>,
        depth: Option<u64>,
    ) -> QueryFuncResult<Env> {
        Ok(self
            .implementation
            .rdeps(env, &universe, &targets, depth.map(|v| v as i32))
            .await?
            .into())
    }

    async fn testsof(&self, env: &Env, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(self.implementation.testsof(env, &targets).await?.into())
    }

    // These three functions are intentionally implemented as errors. They are only available within the context
    // of a deps functions 3rd parameter expr. When used in that context, the QueryFunctions will be augmented to
    // have non-erroring implementations.
    async fn first_order_deps(&self) -> QueryFuncResult<Env> {
        self.implementation.first_order_deps()
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
        self.implementation.intersect(env, left, right).await
    }

    #[binary_op(BinaryOp::Except)]
    async fn except(
        &self,
        env: &Env,
        left: QueryValue<Env::Target>,
        right: QueryValue<Env::Target>,
    ) -> Result<QueryValue<Env::Target>, QueryError> {
        self.implementation.except(env, left, right).await
    }

    #[binary_op(BinaryOp::Union)]
    async fn union(
        &self,
        env: &Env,
        left: QueryValue<Env::Target>,
        right: QueryValue<Env::Target>,
    ) -> Result<QueryValue<Env::Target>, QueryError> {
        self.implementation.union(env, left, right).await
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

impl<Env: QueryEnvironment> DefaultQueryFunctions<Env> {
    pub async fn allpaths(
        &self,
        env: &Env,
        from: &TargetSet<Env::Target>,
        to: &TargetSet<Env::Target>,
    ) -> Result<TargetSet<Env::Target>, QueryError> {
        Ok(env.allpaths(from, to).await?)
    }

    pub async fn somepath(
        &self,
        env: &Env,
        from: &TargetSet<Env::Target>,
        to: &TargetSet<Env::Target>,
    ) -> Result<TargetSet<Env::Target>, QueryError> {
        Ok(env.somepath(from, to).await?)
    }

    pub fn attrfilter(
        &self,
        attr: &str,
        value: &str,
        targets: &TargetSet<Env::Target>,
    ) -> anyhow::Result<TargetSet<Env::Target>> {
        targets.attrfilter(attr, &|v| Ok(v == value))
    }

    pub fn nattrfilter(
        &self,
        attr: &str,
        value: &str,
        targets: &TargetSet<Env::Target>,
    ) -> anyhow::Result<TargetSet<Env::Target>> {
        targets.nattrfilter(attr, &|v| Ok(v == value))
    }

    pub fn attrregexfilter(
        &self,
        attr: &str,
        value: &str,
        targets: &TargetSet<Env::Target>,
    ) -> anyhow::Result<TargetSet<Env::Target>> {
        targets.attrregexfilter(attr, value)
    }

    pub fn buildfile(&self, targets: &TargetSet<Env::Target>) -> FileSet {
        targets.buildfile()
    }

    pub async fn allbuildfiles(
        &self,
        env: &Env,
        universe: &TargetSet<Env::Target>,
    ) -> anyhow::Result<FileSet> {
        env.allbuildfiles(universe).await
    }

    pub async fn rbuildfiles(
        &self,
        env: &Env,
        universe: &FileSet,
        argset: &FileSet,
    ) -> anyhow::Result<FileSet> {
        env.rbuildfiles(universe, argset).await
    }

    pub async fn deps(
        &self,
        env: &Env,
        functions: &dyn QueryFunctions<Env = Env>,
        targets: &TargetSet<Env::Target>,
        depth: Option<i32>,
        captured_expr: Option<&CapturedExpr<'_>>,
    ) -> anyhow::Result<TargetSet<Env::Target>> {
        DepsFunction::<Env> {
            _marker: PhantomData,
        }
        .invoke_deps(env, functions, targets, depth, captured_expr)
        .await
    }

    pub fn filter(
        &self,
        regex: &str,
        targets: &TargetSet<Env::Target>,
    ) -> anyhow::Result<TargetSet<Env::Target>> {
        targets.filter_name(regex)
    }

    pub fn inputs(&self, targets: &TargetSet<Env::Target>) -> anyhow::Result<FileSet> {
        targets.inputs()
    }

    pub fn kind(
        &self,
        regex: &str,
        targets: &TargetSet<Env::Target>,
    ) -> anyhow::Result<TargetSet<Env::Target>> {
        targets.kind(regex)
    }

    pub fn labels(
        &self,
        _attr: &str,
        _targets: &TargetSet<Env::Target>,
    ) -> Result<QueryValue<Env::Target>, QueryError> {
        Err(QueryError::FunctionUnimplemented("labels"))
    }

    pub async fn owner(
        &self,
        env: &Env,
        files: &FileSet,
    ) -> anyhow::Result<TargetSet<Env::Target>> {
        env.owner(files).await
    }

    pub async fn rdeps(
        &self,
        env: &Env,
        universe: &TargetSet<Env::Target>,
        targets: &TargetSet<Env::Target>,
        depth: Option<i32>,
    ) -> anyhow::Result<TargetSet<Env::Target>> {
        env.rdeps(universe, targets, depth).await
    }

    pub async fn testsof(
        &self,
        env: &Env,
        targets: &TargetSet<Env::Target>,
    ) -> anyhow::Result<TargetSet<Env::Target>> {
        env.testsof(targets).await
    }

    // These three functions are intentionally implemented as errors. They are only available within the context
    // of a deps functions 3rd parameter expr. When used in that context, the QueryFunctions will be augmented to
    // have non-erroring implementations.
    pub fn first_order_deps(&self) -> QueryFuncResult<Env> {
        Err(QueryError::NotAvailableInContext("first_order_deps"))
    }
    pub fn target_deps(&self) -> QueryFuncResult<Env> {
        Err(QueryError::NotAvailableInContext("target_deps"))
    }
    pub fn exec_deps(&self) -> QueryFuncResult<Env> {
        Err(QueryError::NotAvailableInContext("exec_deps"))
    }

    pub async fn intersect(
        &self,
        env: &Env,
        left: QueryValue<Env::Target>,
        right: QueryValue<Env::Target>,
    ) -> Result<QueryValue<Env::Target>, QueryError> {
        let left = accept_target_set(env, left).await?;
        let right = accept_target_set(env, right).await?;
        Ok(QueryValue::TargetSet(left.intersect(&right)?))
    }

    pub async fn except(
        &self,
        env: &Env,
        left: QueryValue<Env::Target>,
        right: QueryValue<Env::Target>,
    ) -> Result<QueryValue<Env::Target>, QueryError> {
        let left = accept_target_set(env, left).await?;
        let right = accept_target_set(env, right).await?;
        Ok(QueryValue::TargetSet(left.difference(&right)?))
    }

    pub async fn union(
        &self,
        env: &Env,
        left: QueryValue<Env::Target>,
        right: QueryValue<Env::Target>,
    ) -> Result<QueryValue<Env::Target>, QueryError> {
        // If the operations are of the same type, which + join them.
        // If one is a string, and the other a FileSet or TargetSet, we can promote the string
        match (left, right) {
            (QueryValue::TargetSet(l), QueryValue::TargetSet(r)) => {
                Ok(QueryValue::TargetSet(l.union(&r)))
            }
            (QueryValue::String(l), QueryValue::TargetSet(r)) => {
                let l = env.eval_literals(&[&l]).await?;
                Ok(QueryValue::TargetSet(l.union(&r)))
            }
            (QueryValue::TargetSet(l), QueryValue::String(r)) => {
                let r = env.eval_literals(&[&r]).await?;
                Ok(QueryValue::TargetSet(l.union(&r)))
            }
            (QueryValue::String(l), QueryValue::String(r)) => {
                // Important that String + treats both as target literals, since that's what
                // buck1 does - we blur the lines between string and targetset
                Ok(QueryValue::TargetSet(env.eval_literals(&[&l, &r]).await?))
            }
            (QueryValue::FileSet(l), QueryValue::FileSet(r)) => {
                Ok(QueryValue::FileSet(l.union(&r)))
            }
            (QueryValue::String(l), QueryValue::FileSet(r)) => {
                let l = env.eval_file_literal(&l).await?;
                Ok(QueryValue::FileSet(l.union(&r)))
            }
            (QueryValue::FileSet(l), QueryValue::String(r)) => {
                let r = env.eval_file_literal(&r).await?;
                Ok(QueryValue::FileSet(l.union(&r)))
            }
            (left, right) => Err(QueryError::UnionIncompatibleTypes(
                left.variant_name(),
                right.variant_name(),
            )),
        }
    }
}

pub struct AugmentedQueryFunctions<'a, Env: QueryEnvironment> {
    inner: &'a dyn QueryFunctions<Env = Env>,
    extra: Box<dyn QueryFunctions<Env = Env> + 'a>,
}

impl<'a, Env: QueryEnvironment> AugmentedQueryFunctions<'a, Env> {
    pub fn augment(
        inner: &'a dyn QueryFunctions<Env = Env>,
        extra: Box<dyn QueryFunctions<Env = Env> + 'a>,
    ) -> Self {
        Self { inner, extra }
    }
}

impl<'a, Env: QueryEnvironment> QueryFunctions for AugmentedQueryFunctions<'a, Env> {
    type Env = Env;
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
