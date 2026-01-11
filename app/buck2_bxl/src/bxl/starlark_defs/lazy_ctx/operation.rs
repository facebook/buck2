/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use allocative::Allocative;
use async_recursion::async_recursion;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_interpreter::types::package_path::StarlarkPackagePath;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::load_patterns::load_patterns;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use cquery::LazyCqueryOperation;
use cquery::LazyCqueryResult;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use either::Either;
use futures::FutureExt;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::list::AllocList;
use starlark::values::starlark_value;

use crate::bxl::starlark_defs::analysis_result::StarlarkAnalysisResult;
use crate::bxl::starlark_defs::artifacts::LazyBuildArtifact;
use crate::bxl::starlark_defs::context::BxlContextCoreData;
use crate::bxl::starlark_defs::eval_extra::BxlEvalExtra;
use crate::bxl::starlark_defs::lazy_ctx::operation::uquery::LazyUqueryOperation;
use crate::bxl::starlark_defs::lazy_ctx::operation::uquery::LazyUqueryResult;
use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;
use crate::bxl::starlark_defs::result::StarlarkError;
use crate::bxl::starlark_defs::result::StarlarkResultGen;
use crate::bxl::starlark_defs::target_list_expr::OwnedConfiguredTargetNodeArg;
use crate::bxl::starlark_defs::target_list_expr::OwnedTargetNodeArg;
use crate::bxl::starlark_defs::target_list_expr::SingleOrCompatibleConfiguredTargets;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

pub(crate) mod cquery;
pub(crate) mod uquery;

#[derive(Derivative, Debug, Allocative)]
enum LazyOperation {
    Analysis(ConfiguredProvidersLabel),
    ConfiguredTargetNode {
        arg: OwnedConfiguredTargetNodeArg,
        global_cfg_options: buck2_error::Result<GlobalCfgOptions>,
    },
    UnconfiguredTargetNode(OwnedTargetNodeArg),
    UnconfiguredTargetNodeKeepGoing(String),
    Uquery(LazyUqueryOperation),
    Cquery(LazyCqueryOperation),
    BuildArtifact(LazyBuildArtifact),
    Join(Arc<LazyOperation>, Arc<LazyOperation>),
    Batch(Vec<Arc<LazyOperation>>),
    Catch(Arc<LazyOperation>),
}

enum LazyResult {
    Analysis(StarlarkAnalysisResult),
    ConfiguredTargetNode(SingleOrCompatibleConfiguredTargets),
    UnconfiguredTargetNode(Either<StarlarkTargetNode, StarlarkTargetSet<TargetNode>>),
    UnconfiguredTargetNodeKeepGoing(
        (
            StarlarkTargetSet<TargetNode>,
            SmallMap<StarlarkPackagePath, StarlarkError>,
        ),
    ),
    BuildArtifact(StarlarkArtifact),
    Uquery(LazyUqueryResult),
    Cquery(LazyCqueryResult),
    Join(Box<(LazyResult, LazyResult)>),
    Batch(Vec<LazyResult>),
    Catch(Box<buck2_error::Result<LazyResult>>),
}

impl LazyResult {
    fn into_value<'v>(
        self,
        heap: Heap<'v>,
        bxl_eval_extra: &BxlEvalExtra,
    ) -> buck2_error::Result<Value<'v>> {
        match self {
            LazyResult::Analysis(analysis_res) => Ok(heap.alloc(analysis_res)),
            LazyResult::ConfiguredTargetNode(res) => res.into_value(heap, bxl_eval_extra),
            LazyResult::UnconfiguredTargetNode(node) => Ok(heap.alloc(node)),
            LazyResult::UnconfiguredTargetNodeKeepGoing((success_targets, error_packages)) => {
                Ok(heap.alloc((success_targets, error_packages)))
            }
            LazyResult::BuildArtifact(artifact) => Ok(heap.alloc(artifact)),
            LazyResult::Uquery(res) => res.into_value(heap),
            LazyResult::Cquery(res) => res.into_value(heap),
            LazyResult::Join(res) => Ok(heap.alloc((
                res.0.into_value(heap, bxl_eval_extra)?,
                res.1.into_value(heap, bxl_eval_extra)?,
            ))),
            LazyResult::Batch(res) => Ok(heap.alloc(AllocList(
                res.into_iter()
                    .map(|v| v.into_value(heap, bxl_eval_extra))
                    .collect::<buck2_error::Result<Vec<_>>>()?,
            ))),
            LazyResult::Catch(res) => {
                let val = match *res {
                    Ok(res) => Ok(res.into_value(heap, bxl_eval_extra)?),
                    Err(e) => Err(e),
                };
                Ok(heap.alloc(StarlarkResultGen::from_result(val)))
            }
        }
    }
}

impl LazyOperation {
    #[async_recursion]
    async fn resolve(
        &self,
        dice: &mut DiceComputations<'_>,
        core_data: &BxlContextCoreData,
    ) -> buck2_error::Result<LazyResult> {
        match self {
            LazyOperation::Analysis(label) => {
                Ok(LazyResult::Analysis(analysis(dice, label).await?))
            }
            LazyOperation::ConfiguredTargetNode {
                arg,
                global_cfg_options,
            } => {
                let global_cfg_options = global_cfg_options.as_ref().map_err(|e| e.clone())?;
                let res = arg
                    .to_configured_target_node(global_cfg_options, core_data, dice)
                    .await?;
                Ok(LazyResult::ConfiguredTargetNode(res))
            }
            LazyOperation::UnconfiguredTargetNode(expr) => {
                let node = expr.to_unconfigured_target_node(core_data, dice).await?;
                Ok(LazyResult::UnconfiguredTargetNode(node))
            }
            LazyOperation::UnconfiguredTargetNodeKeepGoing(pattern) => {
                // Parse the target pattern
                let parsed_pattern = ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                    core_data.target_alias_resolver(),
                    CellPathRef::new(core_data.cell_name(), CellRelativePath::empty()),
                    pattern,
                    core_data.cell_resolver(),
                    core_data.cell_alias_resolver(),
                )?;

                let mut success_targets = TargetSet::new();
                let mut error_packages = SmallMap::new();

                match parsed_pattern {
                    ParsedPattern::Target(package, name, TargetPatternExtra) => {
                        // Single target case
                        let label = TargetLabel::new(package, name.as_ref());
                        let node = dice.get_target_node(&label).await;
                        match node {
                            Ok(node) => {
                                success_targets.insert(node);
                            }
                            Err(e) => {
                                error_packages.insert(
                                    StarlarkPackagePath::new(package),
                                    StarlarkError::new(e),
                                );
                            }
                        }
                    }
                    pattern => {
                        let loaded_patterns =
                            load_patterns(dice, vec![pattern], MissingTargetBehavior::Fail).await?;
                        for (package, targets) in loaded_patterns.into_iter() {
                            match targets {
                                Ok(package_targets) => {
                                    // Successfully loaded targets from this package
                                    success_targets.extend(package_targets.into_values());
                                }
                                Err(e) => {
                                    // Failed to load targets from this package
                                    error_packages.insert(
                                        StarlarkPackagePath::new(package.package),
                                        StarlarkError::new(e),
                                    );
                                }
                            }
                        }
                    }
                }

                Ok(LazyResult::UnconfiguredTargetNodeKeepGoing((
                    StarlarkTargetSet(success_targets),
                    error_packages,
                )))
            }
            LazyOperation::Uquery(op) => op.resolve(dice, core_data).await.map(LazyResult::Uquery),
            LazyOperation::Cquery(op) => op.resolve(dice, core_data).await.map(LazyResult::Cquery),
            LazyOperation::BuildArtifact(artifact) => {
                artifact.build_artifacts(dice).await?;
                Ok(LazyResult::BuildArtifact(artifact.artifact()))
            }
            LazyOperation::Join(lazy0, lazy1) => {
                let compute0 = DiceComputations::declare_closure(|dice| {
                    async move { lazy0.resolve(dice, core_data).await }.boxed()
                });
                let compute1 = DiceComputations::declare_closure(|dice| {
                    async move { lazy1.resolve(dice, core_data).await }.boxed()
                });
                let (res0, res1) = dice.try_compute2(compute0, compute1).await?;
                Ok(LazyResult::Join(Box::new((res0, res1))))
            }
            LazyOperation::Batch(lazies) => {
                let res = dice
                    .try_compute_join(lazies, |dice, lazy| {
                        async move { lazy.resolve(dice, core_data).await }.boxed()
                    })
                    .await?;
                Ok(LazyResult::Batch(res))
            }
            LazyOperation::Catch(lazy) => {
                let res = lazy.resolve(dice, core_data).await;
                Ok(LazyResult::Catch(Box::new(res)))
            }
        }
    }
}

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    Allocative,
    Clone,
    Dupe
)]
#[derivative(Debug)]
#[display("{:?}", self)]
pub(crate) struct StarlarkLazy {
    lazy: Arc<LazyOperation>,
}

starlark_simple_value!(StarlarkLazy);

impl StarlarkLazy {
    pub(crate) fn new_analysis(label: ConfiguredProvidersLabel) -> Self {
        Self {
            lazy: Arc::new(LazyOperation::Analysis(label)),
        }
    }

    pub(crate) fn new_configured_target_node(
        arg: OwnedConfiguredTargetNodeArg,
        global_cfg_options: buck2_error::Result<GlobalCfgOptions>,
    ) -> Self {
        Self {
            lazy: Arc::new(LazyOperation::ConfiguredTargetNode {
                arg,
                global_cfg_options: global_cfg_options.map_err(buck2_error::Error::from),
            }),
        }
    }

    pub(crate) fn new_unconfigured_target_node(expr: OwnedTargetNodeArg) -> Self {
        Self {
            lazy: Arc::new(LazyOperation::UnconfiguredTargetNode(expr)),
        }
    }

    pub(crate) fn new_unconfigured_target_node_keep_going(pattern: String) -> Self {
        Self {
            lazy: Arc::new(LazyOperation::UnconfiguredTargetNodeKeepGoing(pattern)),
        }
    }

    pub(crate) fn new_batch<I: IntoIterator<Item = StarlarkLazy>>(lazies: I) -> Self {
        Self {
            lazy: Arc::new(LazyOperation::Batch(
                lazies.into_iter().map(|v| v.lazy).collect(),
            )),
        }
    }

    pub(crate) fn new_join(lazy0: StarlarkLazy, lazy1: StarlarkLazy) -> Self {
        Self {
            lazy: Arc::new(LazyOperation::Join(lazy0.lazy, lazy1.lazy)),
        }
    }

    pub(crate) fn new_uquery(op: LazyUqueryOperation) -> Self {
        Self {
            lazy: Arc::new(LazyOperation::Uquery(op)),
        }
    }

    pub(crate) fn new_cquery(op: LazyCqueryOperation) -> Self {
        Self {
            lazy: Arc::new(LazyOperation::Cquery(op)),
        }
    }

    pub(crate) fn new_build_artifact(artifact: LazyBuildArtifact) -> Self {
        Self {
            lazy: Arc::new(LazyOperation::BuildArtifact(artifact)),
        }
    }
}

#[starlark_value(type = "bxl.Lazy")]
impl<'v> StarlarkValue<'v> for StarlarkLazy {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(lazy_operation_methods)
    }
}

async fn analysis(
    dice: &mut DiceComputations<'_>,
    label: &ConfiguredProvidersLabel,
) -> buck2_error::Result<StarlarkAnalysisResult> {
    let maybe_result = dice.get_analysis_result(label.target()).await?;
    match maybe_result {
        MaybeCompatible::Incompatible(reason) => Err(reason.to_err()),
        MaybeCompatible::Compatible(result) => StarlarkAnalysisResult::new(result, label.dupe()),
    }
}

/// bxl.Lazy can be resolved to the actual result. The computation only happens when called `.resolve()` or `.catch().resolve()`.
#[starlark_module]
fn lazy_operation_methods(builder: &mut MethodsBuilder) {
    /// Resolve the operation to the final result.
    /// When called via `.catch().resolve()`, the error will be caught and returned as a [`bxl.Result`](../Result).
    /// Otherwise, it will return the raw type without catching the error.
    ///
    /// Example:
    /// ```python
    /// def _impl(ctx):
    ///     target = ctx.configured_targets("cell//path/to:target")
    ///     analysis_result = ctx.lazy.analysis(target).resolve()
    /// ```
    fn resolve<'v>(
        this: &StarlarkLazy,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Value<'v>> {
        let heap = eval.heap();
        let bxl_eval_extra = BxlEvalExtra::from_context(eval)?;
        let lazy = this.lazy.clone();
        let res = bxl_eval_extra
            .dice
            .via(|dice| async { lazy.resolve(dice, &bxl_eval_extra.core).await }.boxed_local());

        Ok(res.and_then(|v| v.into_value(heap, bxl_eval_extra))?)
    }

    /// Make `Lazy` can be resolved later by catching the error.
    ///
    /// Example:
    /// ```python
    /// def _impl(ctx):
    ///     target = ctx.configured_targets("cell//path/to:target")
    ///     analysis_result = ctx.lazy.analysis(target).catch().resolve()
    /// ```
    fn catch(this: &StarlarkLazy) -> starlark::Result<StarlarkLazy> {
        let lazy = Arc::new(LazyOperation::Catch(this.lazy.dupe()));
        Ok(StarlarkLazy { lazy })
    }
}
