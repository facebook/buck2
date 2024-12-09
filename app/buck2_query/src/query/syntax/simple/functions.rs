/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::marker::PhantomData;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::configuration::compatibility::MaybeCompatible;
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
use crate::query::syntax::simple::eval::values::QueryResult;
use crate::query::syntax::simple::eval::values::QueryValue;
use crate::query::syntax::simple::eval::values::QueryValueSet;
use crate::query::syntax::simple::functions::deps::DepsFunction;
use crate::query::syntax::simple::functions::docs::ModuleDescription;
use crate::query::syntax::simple::functions::helpers::CapturedExpr;
use crate::query::syntax::simple::functions::helpers::QueryArgType;
use crate::query::syntax::simple::functions::helpers::QueryBinaryOp;
use crate::query::syntax::simple::functions::helpers::QueryFunction;

pub mod deps;
pub mod description;
pub mod docs;
pub mod helpers;

pub trait QueryLiteralVisitor {
    fn target_pattern(&mut self, pattern: &str) -> buck2_error::Result<()>;
}

pub trait HasModuleDescription {
    fn describe() -> ModuleDescription;
}

#[async_trait]
pub trait QueryFunctions: Debug + Send + Sync {
    type Env: QueryEnvironment;

    fn get(&self, name: &str) -> Option<&dyn QueryFunction<Self::Env>>;

    fn get_op(&self, op: BinaryOp) -> Option<&dyn QueryBinaryOp<Self::Env>>;
}

pub trait QueryFunctionsVisitLiterals: Debug + Send + Sync {
    fn visit_literals(
        &self,
        visitor: &mut dyn QueryLiteralVisitor,
        expr: &Spanned<Expr>,
    ) -> QueryResult<()>;
}

impl<F: QueryFunctions> QueryFunctionsVisitLiterals for F {
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
                                matches!(
                                    func.arg_type(i)?,
                                    QueryArgType::TargetSet
                                        | QueryArgType::Set
                                        | QueryArgType::Value
                                ),
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

#[derive(Allocative)]
#[allocative(bound = "")]
pub struct DefaultQueryFunctionsModule<Env: QueryEnvironment> {
    implementation: DefaultQueryFunctions<Env>,
}

impl<Env: QueryEnvironment> Debug for DefaultQueryFunctionsModule<Env> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DefaultQueryFunctionsModule")
            .finish_non_exhaustive()
    }
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
        evaluator: &QueryEvaluator<'_, Env>,
        from: TargetSet<Env::Target>,
        to: TargetSet<Env::Target>,
        captured_expr: Option<CapturedExpr<'_>>,
    ) -> QueryFuncResult<Env> {
        Ok(self
            .implementation
            .allpaths(
                evaluator.env(),
                evaluator.functions(),
                &from,
                &to,
                captured_expr.as_ref(),
            )
            .await?
            .into())
    }

    /// Shortest dependency path between two sets of targets.
    ///
    /// * The first parameter `from` represents the upstream targets (e.g., final binary).
    /// * The second parameter `to` represents the downstream targets (e.g., a library).
    ///
    /// Results are returned in order from top to bottom (upstream to downstream).
    ///
    /// If multiple paths exist, the returned path is unspecified. If no path exists, an empty set is returned.
    ///
    /// For example:
    ///
    /// ```text
    /// $ buck2 uquery 'somepath(//buck2:buck2, //buck2/app/buck2_node:buck2_node)'
    ///
    /// //buck2:buck2
    /// //buck2/app/buck2:buck2-bin
    /// //buck2/app/buck2_analysis:buck2_analysis
    /// //buck2/app/buck2_node:buck2_node
    /// ```
    async fn somepath(
        &self,
        evaluator: &QueryEvaluator<'_, Env>,
        from: TargetSet<Env::Target>,
        to: TargetSet<Env::Target>,
        captured_expr: Option<CapturedExpr<'_>>,
    ) -> QueryFuncResult<Env> {
        Ok(self
            .implementation
            .somepath(
                evaluator.env(),
                evaluator.functions(),
                &from,
                &to,
                captured_expr.as_ref(),
            )
            .await?
            .into())
    }

    /// Rule attribute filtering.
    ///
    /// The `attrfilter(attribute, value, targets)` operator evaluates the given target expression and filters the resulting build targets to those where the specified attribute contains the specified value.
    /// In this context, the term attribute refers to an argument in a build rule, such as name, headers, srcs, or deps.
    ///
    /// - If the attribute is a single value, say `name`, it is compared to the specified value, and the target is returned if they match.
    /// - If the attribute is a list, the target is returned if that list contains the specified value.
    /// - If the attribute is a dictionary, the target is returned if the value exists in either the keys or the values of the dictionary.
    ///
    /// For example:
    /// `buck2 query "attrfilter(deps, '//foo:bar', '//...')"` returns the build targets in the repository that depend on `//foo:bar`, or more precisely: those build targets that include `//foo:bar` in their deps argument list.
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

    /// Negative rule attribute filtering. It is the opposite of `attrfilter`.
    ///
    /// The `nattrfilter(attribute, value, targets)` operator evaluates the given target expression and filters the resulting build targets to those where the specified attribute doesn't contain the specified value.
    /// In this context, the term attribute refers to an argument in a build rule, such as name, headers, srcs, or deps.
    ///
    /// - If the attribute is a single value, say `name`, it is compared to the specified value, and the target is returned if they don't match.
    /// - If the attribute is a list, the target is returned if that list doesn't contain the specified value.
    /// - If the attribute is a dictionary, the target is returned if the value doesn't exist in both the keys and the values of the dictionary.
    ///
    /// For example:
    /// `buck2 query "nattrfilter(deps, '//foo:bar', '//...')"` returns the build targets in the repository that don't depend on `//foo:bar`, or more precisely: those build targets that don't include `//foo:bar` in their deps argument list.
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

    /// Rule attribute filtering with regex.
    ///
    /// The `attrregexfilter(attribute, value, targets)` operator is identical to the `attrfilter(attribute, value, targets)` operator except that it takes a regular expression as the second argument.
    /// It evaluates the given target expression and filters the resulting build targets to those where the specified attribute matches the specified pattern.
    /// In this context, the term attribute refers to an argument in a build rule, such as name, headers, srcs, or deps.
    ///
    /// - If the attribute is a single value, say name, it is matched against the specified pattern, and the target is returned if they match.
    /// - If the attribute is a list, the target is returned if that list contains a value that matches the specified pattern.
    /// - If the attribute is a dictionary, the target is returned if the pattern match is found in either the keys or the values of the dictionary.
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

    /// Finds the build file where given target is defined.
    ///
    /// The `targets` parameter is a specific target or target pattern. It specifies the targets to find build file dependencies for.
    /// In order to find the build file associated with a source file, combine the owner operator with buildfile.
    /// Examples:
    /// `buck2 uquery "buildfile(//buck2/app/buck2_action_impl_tests:buck2_action_impl_tests)"`
    /// `buck2 uquery "buildfile(owner(context.rs))"`
    /// Both return the build file location:
    /// `fbcode/buck2/app/buck2_action_impl_tests/TARGETS`
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

    /// Filter using regex partial match.
    /// Target are matched against their fully qualified name.
    /// Files are matched against their repo path like `repo//foo/bar/baz.py`.
    async fn filter(&self, regex: String, set: QueryValueSet<Env::Target>) -> QueryFuncResult<Env> {
        match set {
            QueryValueSet::TargetSet(targets) => Ok(self
                .implementation
                .filter_target_set(&regex, &targets)?
                .into()),
            QueryValueSet::FileSet(files) => {
                Ok(self.implementation.filter_file_set(&regex, &files)?.into())
            }
        }
    }

    /// Returns all inputs non-transitively
    /// Returns the files that are inputs to the `targets` expression, ignoring all dependencies.
    /// Returns only the files which are an immediate input to the rule function and thus are needed to go through analysis phase (i.e. produce providers).
    /// You could consider the `inputs()` and `owner()` operators to be inverses of each other.
    ///
    /// `buck2 cquery "inputs(fbcode//buck2/dice/...)"` returns the input files for the `fbcode//buck2/dice/...` targets.
    async fn inputs(&self, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(self.implementation.inputs(&targets)?.into())
    }

    /// Filter targets by rule type.
    ///
    /// Returns a subset of `targets` where the rule type matches the specified `regex`. The specified pattern can be a regular expression.
    ///
    /// For example:
    /// ```text
    /// $ buck2 query "kind('java.*', deps('//foo:bar'))"
    /// ```
    /// This command returns targets matching rule type `java.*` (e.g., `java_library`, `java_binary`) in the transitive dependencies of `//foo:bar`.
    async fn kind(&self, regex: String, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(targets.kind(&regex)?.into())
    }

    /// Not implemented.
    ///
    /// This function won't be implemented in the future, because buck2 query core does not support returning both files and targets from a single function.
    ///
    /// In buck1 it returns targets and files referenced by the given attribute in the given targets.
    ///
    /// <FbInternalOnly>
    /// For more context see discussion in T126638795.
    /// </FbInternalOnly>
    async fn labels(&self, attr: String, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        self.implementation.labels(&attr, &targets)
    }

    /// Targets owning the given file.
    ///
    /// Returns all targets that have a specified file as an input.
    ///
    /// `owner()` and `inputs()` functions are inverses of each other.
    ///
    /// If the specified file has multiple owning targets, a set of targets is returned. If no owner exists, an empty set is returned.
    ///
    /// For example:
    /// ```text
    /// $ buck2 uquery "owner('app/buck2/src/lib.rs')"
    ///
    /// //buck2/app/buck2:buck2-unittest
    /// //buck2/app/buck2:buck2
    /// ```
    async fn owner(&self, env: &Env, files: FileSet) -> QueryFuncResult<Env> {
        Ok(self.implementation.owner(env, &files).await?.into())
    }

    async fn targets_in_buildfile(&self, env: &Env, files: FileSet) -> QueryFuncResult<Env> {
        Ok(self
            .implementation
            .targets_in_buildfile(env, &files)
            .await?
            .into())
    }
    /// Find the reverse dependencies of the targets in the given target universe.
    ///
    /// The first parameter `universe` defines where to look for reverse dependencies.
    /// The second parameter `targets` is a specific target or target pattern. It specifies the targets to find reverse dependencies for.
    /// The third argument `depth` is an optional integer literal specifying an upper bound on the depth of the search. A value of one (1) specifies that buck query should return only direct dependencies. If the depth parameter is omitted, the search is unbounded.
    /// The fourth argument `captured_expr` is an optional expression that can be used to filter the results.
    ///
    /// The returned values include the nodes from the `targets` argument itself.
    ///
    /// For example following uquery:
    ///
    /// ```text
    /// $ buck2 uquery "rdeps(//buck2/..., //buck2/dice/dice:dice, 1)"
    /// ```
    /// returns all targets under `//buck2/...` that depend on `//buck2/dice/dice:dice`.
    async fn rdeps(
        &self,
        evaluator: &QueryEvaluator<'_, Env>,
        universe: TargetSet<Env::Target>,
        targets: TargetSet<Env::Target>,
        depth: Option<u64>,
        captured_expr: Option<CapturedExpr<'_>>,
    ) -> QueryFuncResult<Env> {
        Ok(self
            .implementation
            .rdeps(
                evaluator.env(),
                evaluator.functions(),
                &universe,
                &targets,
                depth.map(|v| v as i32),
                captured_expr.as_ref(),
            )
            .await?
            .into())
    }

    async fn testsof(&self, env: &Env, targets: TargetSet<Env::Target>) -> QueryFuncResult<Env> {
        Ok(self.implementation.testsof(env, &targets).await?.into())
    }

    // These three functions are intentionally implemented as errors. They are only available within the context
    // of a deps functions 3rd parameter expr. When used in that context, the QueryFunctions will be augmented to
    // have non-erroring implementations.

    /// A filter function that can be used in the query expression of `deps` query function.
    /// Returns the output of deps function for the immediate dependencies of the given targets. Output is equivalent to `deps(<targets>, 1)`.
    ///
    /// Example:
    /// `buck2 cquery "deps('//foo:bar', 1, first_order_deps())"` is equivalent to `buck2 cquery "deps('//foo:bar', 1)"`
    async fn first_order_deps(&self) -> QueryFuncResult<Env> {
        Err(QueryError::NotAvailableInContext("first_order_deps"))
    }

    /// A filter function that can be used in the query expression of `deps` query function.
    /// Returns the target dependencies of each dependency of the given targets, excluding any configuration, toolchain and execution dependencies (build time dependencies)
    /// like compiler used as a part of the build.
    ///
    /// Example:
    /// `buck2 cquery "deps('//foo:bar', 1, target_deps())"`
    async fn target_deps(&self) -> QueryFuncResult<Env> {
        Err(QueryError::NotAvailableInContext("target_deps"))
    }

    /// A filter function that can be used in the query expression of `deps` query function.
    /// Returns the output of deps function for execution dependencies (build time dependencies), ex. compiler used as a part of the build.
    ///
    /// Example:
    /// `buck2 cquery "deps('//foo:bar', 1, exec_deps())"`
    async fn exec_deps(&self) -> QueryFuncResult<Env> {
        Err(QueryError::NotAvailableInContext("exec_deps"))
    }

    /// A filter function that can be used in the query expression of `deps` query function.
    /// Returns the output of deps function for configuration dependencies (that appear as conditions in selects).
    ///
    /// Example:
    /// `buck2 cquery "deps('//foo:bar', 1, configuration_deps())"`
    async fn configuration_deps(&self) -> QueryFuncResult<Env> {
        Err(QueryError::NotAvailableInContext("configuration_deps"))
    }

    /// A filter function that can be used in the query expression of `deps` query function.
    /// Returns the output of deps function for toolchain dependencies.
    ///
    /// Example:
    /// `buck2 cquery "deps('//foo:bar', 1, toolchain_deps())"`
    async fn toolchain_deps(&self) -> QueryFuncResult<Env> {
        Err(QueryError::NotAvailableInContext("configuration_deps"))
    }
    /// Computes the set intersection over the given arguments.
    /// Can be used with the `^` symbol. This operator is commutative.
    ///
    /// The parser treats this operator as left-associative and of equal precedence, so we recommend
    /// that you use parentheses if you need to ensure a specific order of evaluation. A parenthesized expression
    /// resolves to the value of the expression it encloses.
    ///
    /// Example:
    /// `buck2 aquery "deps('//foo:bar') intersect deps('//baz:lib')"` is the same as
    /// `buck2 aquery "deps('//foo:bar') ^ deps('//baz:lib')"`
    /// Both return the targets that appear in the transitive closure of `//foo:bar` and `//baz:lib`.
    #[binary_op(BinaryOp::Intersect)]
    async fn intersect(
        &self,
        env: &Env,
        left: QueryValue<Env::Target>,
        right: QueryValue<Env::Target>,
    ) -> Result<QueryValue<Env::Target>, QueryError> {
        self.implementation.intersect(env, left, right).await
    }

    /// Computes the arguments that are in argument A but not in argument B.
    /// Can be used with the `-` symbol. This operator is NOT commutative.
    ///
    /// The parser treats this operator as left-associative and of equal precedence, so we recommend
    /// that you use parentheses if you need to ensure a specific order of evaluation. A parenthesized expression
    /// resolves to the value of the expression it encloses.
    ///
    /// Example:
    /// `buck2 aquery "deps('//foo:bar') except deps('//baz:lib')"` is the same as
    /// `buck2 aquery "deps('//foo:bar') - deps('//baz:lib')"`
    /// Both return the targets that `//foo:bar` depends on and that `//baz:lib` does NOT depend on.
    #[binary_op(BinaryOp::Except)]
    async fn except(
        &self,
        env: &Env,
        left: QueryValue<Env::Target>,
        right: QueryValue<Env::Target>,
    ) -> Result<QueryValue<Env::Target>, QueryError> {
        self.implementation.except(env, left, right).await
    }

    /// Computes the set union over the given arguments.
    /// Can be used with the `+` symbol. This operator is commutative.
    ///
    /// The parser treats all this operator as left-associative and of equal precedence, so we recommend
    /// that you use parentheses if you need to ensure a specific order of evaluation. A parenthesized expression
    /// resolves to the value of the expression it encloses.
    ///
    /// Example:
    /// `buck2 aquery "deps('//foo:bar') union deps('//baz:lib')"` is the same as
    /// `buck2 aquery "deps('//foo:bar') + deps('//baz:lib')"`
    /// Both return the aggregation of the targets that `//foo:bar` and `//baz:lib` depend on.
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

#[derive(Allocative)]
#[allocative(bound = "")]
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
        functions: &dyn QueryFunctions<Env = Env>,
        from: &TargetSet<Env::Target>,
        to: &TargetSet<Env::Target>,
        captured_expr: Option<&CapturedExpr<'_>>,
    ) -> Result<TargetSet<Env::Target>, QueryError> {
        Ok(DepsFunction::<Env> {
            _marker: PhantomData,
        }
        .invoke_allpaths(env, functions, from, to, captured_expr)
        .await?)
    }

    pub async fn somepath(
        &self,
        env: &Env,
        functions: &dyn QueryFunctions<Env = Env>,
        from: &TargetSet<Env::Target>,
        to: &TargetSet<Env::Target>,
        captured_expr: Option<&CapturedExpr<'_>>,
    ) -> Result<TargetSet<Env::Target>, QueryError> {
        Ok(DepsFunction::<Env> {
            _marker: PhantomData,
        }
        .invoke_somepath(env, functions, from, to, captured_expr)
        .await?)
    }

    pub fn attrfilter(
        &self,
        attr: &str,
        value: &str,
        targets: &TargetSet<Env::Target>,
    ) -> buck2_error::Result<TargetSet<Env::Target>> {
        targets.attrfilter(attr, &|v| Ok(v == value))
    }

    pub fn nattrfilter(
        &self,
        attr: &str,
        value: &str,
        targets: &TargetSet<Env::Target>,
    ) -> buck2_error::Result<TargetSet<Env::Target>> {
        targets.nattrfilter(attr, &|v| Ok(v == value))
    }

    pub fn attrregexfilter(
        &self,
        attr: &str,
        value: &str,
        targets: &TargetSet<Env::Target>,
    ) -> buck2_error::Result<TargetSet<Env::Target>> {
        targets.attrregexfilter(attr, value)
    }

    pub fn buildfile(&self, targets: &TargetSet<Env::Target>) -> FileSet {
        targets.buildfile()
    }

    pub async fn allbuildfiles(
        &self,
        env: &Env,
        universe: &TargetSet<Env::Target>,
    ) -> buck2_error::Result<FileSet> {
        env.allbuildfiles(universe).await
    }

    pub async fn rbuildfiles(
        &self,
        env: &Env,
        universe: &FileSet,
        argset: &FileSet,
    ) -> buck2_error::Result<FileSet> {
        env.rbuildfiles(universe, argset).await
    }

    pub async fn deps(
        &self,
        env: &Env,
        functions: &dyn QueryFunctions<Env = Env>,
        targets: &TargetSet<Env::Target>,
        depth: Option<i32>,
        captured_expr: Option<&CapturedExpr<'_>>,
    ) -> buck2_error::Result<TargetSet<Env::Target>> {
        DepsFunction::<Env> {
            _marker: PhantomData,
        }
        .invoke_deps(env, functions, targets, depth, captured_expr)
        .await
    }

    /// Filter targets by fully qualified name using regex partial match.
    pub fn filter_target_set(
        &self,
        regex: &str,
        targets: &TargetSet<Env::Target>,
    ) -> buck2_error::Result<TargetSet<Env::Target>> {
        targets.filter_name(regex)
    }

    pub fn filter_file_set(&self, regex: &str, files: &FileSet) -> buck2_error::Result<FileSet> {
        files.filter_name(regex)
    }

    pub fn inputs(&self, targets: &TargetSet<Env::Target>) -> buck2_error::Result<FileSet> {
        targets.inputs()
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
    ) -> buck2_error::Result<TargetSet<Env::Target>> {
        env.owner(files).await
    }

    pub async fn targets_in_buildfile(
        &self,
        env: &Env,
        paths: &FileSet,
    ) -> buck2_error::Result<TargetSet<Env::Target>> {
        env.targets_in_buildfile(paths).await
    }

    pub async fn rdeps(
        &self,
        env: &Env,
        functions: &dyn QueryFunctions<Env = Env>,
        universe: &TargetSet<Env::Target>,
        targets: &TargetSet<Env::Target>,
        depth: Option<i32>,
        captured_expr: Option<&CapturedExpr<'_>>,
    ) -> buck2_error::Result<TargetSet<Env::Target>> {
        DepsFunction::<Env> {
            _marker: PhantomData,
        }
        .invoke_rdeps(env, functions, universe, targets, depth, captured_expr)
        .await
    }

    pub async fn testsof(
        &self,
        env: &Env,
        targets: &TargetSet<Env::Target>,
    ) -> buck2_error::Result<TargetSet<Env::Target>> {
        env.testsof(targets).await
    }

    pub async fn testsof_with_default_target_platform(
        &self,
        env: &Env,
        targets: &TargetSet<Env::Target>,
    ) -> buck2_error::Result<Vec<MaybeCompatible<Env::Target>>> {
        env.testsof_with_default_target_platform(targets).await
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

impl<'a, Env: QueryEnvironment> Debug for AugmentedQueryFunctions<'a, Env> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AugmentedQueryFunctions")
            .finish_non_exhaustive()
    }
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
