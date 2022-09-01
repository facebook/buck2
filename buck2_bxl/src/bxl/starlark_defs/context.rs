/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The context containing the available buck commands and query operations for `bxl` functions.
//!

use std::cell::RefCell;
use std::io::Write;
use std::sync::Arc;

use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::query::dice::DiceQueryDelegate;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::cells::CellInstance;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::target::TargetLabel;
use buck2_execute::artifact::fs::ArtifactFs;
use buck2_execute::bxl::types::BxlKey;
use buck2_interpreter::types::label::Label;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use either::Either;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use indexmap::IndexSet;
use itertools::Itertools;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::dict::Dict;
use starlark::values::none::NoneType;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueTyped;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::alloc_node::AllocNode;
use crate::bxl::starlark_defs::context::actions::BxlActionsCtx;
use crate::bxl::starlark_defs::context::fs::BxlFilesystem;
use crate::bxl::starlark_defs::context::output::OutputStream;
use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::cquery::get_cquery_env;
use crate::bxl::starlark_defs::cquery::StarlarkCQueryCtx;
use crate::bxl::starlark_defs::providers_expr::ProvidersExpr;
use crate::bxl::starlark_defs::target_expr::TargetExpr;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
use crate::bxl::starlark_defs::uquery::get_uquery_env;
use crate::bxl::starlark_defs::uquery::StarlarkUQueryCtx;
use crate::bxl::value_as_starlak_target_label::ValueAsStarlarkTargetLabel;

pub mod actions;
pub mod analysis;
pub mod build;
pub mod fs;
pub mod output;
pub mod starlark_async;

/// The bxl context that the top level bxl implementation receives as parameter.
/// This context contains all the core bxl functions to query, build, create actions, etc.
#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    StarlarkDocs
)]
#[starlark_docs_attrs(directory = "bxl")]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub struct BxlContext<'v> {
    #[trace(unsafe_ignore)]
    pub(crate) current_bxl: BxlKey,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    pub(crate) target_alias_resolver: BuckConfigTargetAliasResolver,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    pub(crate) cell: CellInstance,
    cli_args: Value<'v>, // Struct of the cli args
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    pub(crate) async_ctx: BxlSafeDiceComputations<'v>,
    pub(crate) state: ValueTyped<'v, AnalysisActions<'v>>,
    pub(crate) output_stream: ValueTyped<'v, OutputStream<'v>>,
}

impl<'v> BxlContext<'v> {
    pub(crate) fn new(
        heap: &'v Heap,
        current_bxl: BxlKey,
        cli_args: Value<'v>,
        target_alias_resolver: BuckConfigTargetAliasResolver,
        project_fs: ProjectRoot,
        artifact_fs: ArtifactFs,
        cell: CellInstance,
        async_ctx: BxlSafeDiceComputations<'v>,
        output_sink: RefCell<Box<dyn Write>>,
    ) -> Self {
        Self {
            current_bxl,
            target_alias_resolver,
            cell,
            cli_args,
            async_ctx,
            state: ValueTyped::new(heap.alloc(AnalysisActions {
                state: RefCell::new(None),
                attributes: Value::new_none(),
            }))
            .unwrap(),
            output_stream: ValueTyped::new(heap.alloc(OutputStream::new(
                project_fs,
                artifact_fs,
                output_sink,
            )))
            .unwrap(),
        }
    }

    pub(crate) async fn dice_query_delegate(
        ctx: &DiceComputations,
        target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<DiceQueryDelegate<'_>> {
        let cwd = AbsPathBuf::try_from(std::env::current_dir()?)?;
        let working_dir = {
            let fs = ProjectRoot::new(cwd.clone());
            fs.relativize(&cwd)?.as_ref().to_owned()
        };
        let cell_resolver = ctx.get_cell_resolver().await?;
        let project_root = ProjectRoot::new(cwd);
        let package_boundary_exceptions = ctx.get_package_boundary_exceptions().await?;
        let target_alias_resolver = ctx
            .target_alias_resolver_for_working_dir(&working_dir)
            .await?;
        DiceQueryDelegate::new(
            ctx,
            &working_dir,
            project_root,
            cell_resolver,
            target_platform,
            package_boundary_exceptions,
            target_alias_resolver,
        )
    }

    pub(crate) fn sync_dice_query_delegate(
        &self,
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<DiceQueryDelegate<'_>> {
        self.async_ctx
            .via_dice(|dice| Self::dice_query_delegate(dice, global_target_platform))
    }

    /// Must take an `AnalysisContext` and `OutputStream` which has never had `take_state` called on it before.
    pub(crate) fn take_state(
        value: ValueTyped<'v, BxlContext<'v>>,
    ) -> anyhow::Result<(Option<AnalysisRegistry<'v>>, IndexSet<ArtifactGroup>)> {
        let this = value.as_ref();
        Ok((
            this.state.as_ref().state.borrow_mut().take(),
            // artifacts should be bound by now as the bxl has finished running
            this.output_stream
                .as_ref()
                .take_artifacts()
                .into_iter()
                .map(|v| {
                    let artifact_or_err = v
                        .as_artifact()
                        .unwrap()
                        .get_bound_artifact_and_associated_artifacts();
                    match artifact_or_err {
                        Err(e) => Err(e),
                        Ok((artifact, associated_artifacts)) => {
                            let mut artifacts: IndexSet<ArtifactGroup> =
                                associated_artifacts.iter().map(|ag| ag.dupe()).collect();
                            artifacts.insert(ArtifactGroup::Artifact(artifact));
                            Ok(artifacts)
                        }
                    }
                })
                .flatten_ok()
                .collect::<anyhow::Result<IndexSet<ArtifactGroup>>>()?,
        ))
    }
}

impl<'v> StarlarkValue<'v> for BxlContext<'v> {
    starlark_type!("bxl_ctx");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_context)
    }
}

impl<'v> AllocValue<'v> for BxlContext<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkTypeRepr for &'v BxlContext<'v> {
    fn starlark_type_repr() -> String {
        BxlContext::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v BxlContext<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v BxlContext<'v>> {
        x.downcast_ref()
    }
}

#[starlark_module]
fn register_context(builder: &mut MethodsBuilder) {
    /// Gets the output stream to the console via stdout. Items written to the output stream
    /// are considered to be the results of a bxl script, which will be displayed to stdout by
    /// buck2 even when the script is cached.
    ///
    /// Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
    /// and `pprint`.
    #[starlark(attribute)]
    fn output<'v>(this: &'v BxlContext) -> anyhow::Result<Value<'v>> {
        Ok(this.output_stream.to_value())
    }

    /// Returns the absolute path to the root of the repository
    fn root(this: &BxlContext) -> anyhow::Result<String> {
        Ok(this
            .async_ctx
            .0
            .global_data()
            .get_io_provider()
            .fs()
            .root()
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("Non utf-8 path"))?
            .to_owned())
    }

    /// Gets the target nodes for the `labels`, accepting an optional `target_platform` which is the
    /// target platform configuration used to resolve configurations of any unconfigured target
    /// nodes.
    /// The `target_platform` is either a string that can be parsed as a target label, or a
    /// target label.
    ///
    /// The given `labels` is a [`TargetExpr`], which is either:
    ///     - a single string that is a `target pattern`.
    ///     - a single target node or label, configured or unconfigured
    ///     - a list of the two options above.
    ///
    /// This returns either a single [`StarlarkConfiguredTargetNode`] if the given `labels`
    /// is "singular", a dict keyed by target labels of [`StarlarkConfiguredTargetNode`] if the
    /// given `labels` is list-like
    fn configured_targets<'v>(
        this: &'v BxlContext<'v>,
        #[starlark(require = pos)] labels: Value<'v>,
        #[starlark(default = NoneType)] target_platform: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let target_platform =
            target_platform.parse_target_platforms(&this.target_alias_resolver, &this.cell)?;

        let res: anyhow::Result<Value<'v>> = this.async_ctx.via_dice(|ctx| async move {
            let query_env = get_cquery_env(ctx, target_platform.dupe()).await?;
            Ok(
                match TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                    labels,
                    &target_platform,
                    this,
                    eval,
                )
                .await?
                {
                    TargetExpr::Label(label) => {
                        let node = ctx
                            .get_configured_target_node(&label)
                            .await?
                            .require_compatible()?;

                        node.alloc(eval.heap())
                    }

                    TargetExpr::Node(node) => node.alloc(eval.heap()),
                    multi => eval.heap().alloc(StarlarkTargetSet::from(
                        multi.get(&query_env).await?.into_owned(),
                    )),
                },
            )
        });

        res
    }

    /// Gets the unconfigured target nodes for the `labels`
    ///
    /// The given `labels` is either:
    ///     - a single string that is a `target pattern`.
    ///     - a single unconfigured  target node or label
    ///     - a list of the two options above.
    ///
    /// This returns either a single [`StarlarkTargetNode`] if the given `labels`
    /// is "singular", a dict keyed by target labels of [`StarlarkTargetNode`] if the
    /// given `labels` is list-like
    fn unconfigured_targets<'v>(
        this: &'v BxlContext<'v>,
        labels: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let res: anyhow::Result<Value<'v>> = this.async_ctx.via_dice(|ctx| async move {
            let query_env = get_uquery_env(ctx).await?;
            Ok(
                match TargetExpr::<'v, TargetNode>::unpack(labels, this, eval).await? {
                    TargetExpr::Label(label) => {
                        let node = ctx.get_target_node(&label).await?;

                        node.alloc(eval.heap())
                    }

                    TargetExpr::Node(node) => node.alloc(eval.heap()),
                    multi => eval.heap().alloc(StarlarkTargetSet::from(
                        multi.get(&query_env).await?.into_owned(),
                    )),
                },
            )
        });

        res
    }

    /// Returns the [`StarlarkUQueryCtx`] that holds all uquery functions.
    fn uquery<'v>(this: &'v BxlContext<'v>) -> anyhow::Result<StarlarkUQueryCtx<'v>> {
        let delegate = this.sync_dice_query_delegate(None)?;
        StarlarkUQueryCtx::new(Arc::new(delegate))
    }

    /// Returns the [`StarlarkCQueryCtx`] that holds all the cquery functions.
    /// This function takes an optional parameter `target_platform`, which is the target platform
    /// configuration used to configured any unconfigured target nodes.
    ///
    /// The `target_platform` is a target label, or a string that is a target label.
    fn cquery<'v>(
        this: &'v BxlContext<'v>,
        // TODO(brasselsprouts): I would like to strongly type this.
        #[starlark(default = NoneType)] target_platform: Value<'v>,
    ) -> anyhow::Result<StarlarkCQueryCtx<'v>> {
        this.async_ctx
            .via(|| StarlarkCQueryCtx::new(this, target_platform))
    }

    /// Returns the action context [`BxlActionsCtx`] for creating and running actions.
    #[starlark(attribute)]
    fn bxl_actions<'v>(this: ValueOf<'v, &'v BxlContext<'v>>) -> anyhow::Result<BxlActionsCtx<'v>> {
        Ok(BxlActionsCtx::new(ValueTyped::new(this.value).unwrap()))
    }

    /// Runs analysis on the given `labels`, accepting an optional `target_platform` which is the
    /// target platform configuration used to resolve configurations of any unconfigured target
    /// nodes, and an optional `skip_incompatible` boolean that indicates whether to skip analysis
    /// of nodes that are incompatible with the target platform.
    /// The `target_platform` is either a string that can be parsed as a target label, or a
    /// target label.
    ///
    /// The given `labels` is a providers expression, which is either:
    ///     - a single string that is a `target pattern`.
    ///     - a single target node or label, configured or unconfigured
    ///     - a single sub target label, configured or unconfigured
    ///     - a list of the two options above.
    ///
    /// This returns either a single [`StarlarkAnalysisResult`] if the given `labels` is "singular",
    /// or a dict keyed by sub target labels of [`StarlarkAnalysisResult`] if the given `labels`
    /// is list-like
    fn analysis<'v>(
        this: &BxlContext<'v>,
        labels: Value<'v>,
        #[starlark(default = NoneType)] target_platform: Value<'v>,
        #[starlark(default = true)] skip_incompatible: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let providers = ProvidersExpr::unpack(labels, target_platform, this, eval)?;

        let res: anyhow::Result<_> = this
            .async_ctx
            .via_dice(|ctx| async { analysis::analysis(ctx, providers, skip_incompatible).await });

        Ok(match res? {
            Either::Left(single) => eval.heap().alloc(single),
            Either::Right(many) => eval.heap().alloc(Dict::new(
                many.into_iter()
                    .map(|(t, v)| {
                        Ok((
                            eval.heap().alloc(Label::new(eval.heap(), t)).get_hashed()?,
                            eval.heap().alloc(v),
                        ))
                    })
                    .collect::<anyhow::Result<_>>()?,
            )),
        })
    }

    /// Runs a build on the given `labels`, accepting an optional `target_platform` which is the
    /// target platform configuration used to resolve configurations.
    ///
    /// The given `labels` is a providers expression, which is either:
    ///     - a single string that is a `target pattern`.
    ///     - a single target node or label, configured or unconfigured
    ///     - a single provider label, configured or unconfigured
    ///     - a list of the two options above.
    ///
    /// This returns a dict keyed by sub target labels of [`StarlarkBuildResult`] if the
    /// given `labels` is list-like
    fn build<'v>(
        this: &'v BxlContext<'v>,
        spec: Value<'v>,
        #[starlark(default = NoneType)] target_platform: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        Ok(eval
            .heap()
            .alloc(Dict::new(build::build(this, spec, target_platform, eval)?)))
    }

    /// A struct of the command line args as declared using the [`cli_args`] module.
    /// These command lines are resolved per the users input on the cli when invoking the bxl script.
    #[starlark(attribute)]
    fn cli_args<'v>(this: &BxlContext<'v>) -> anyhow::Result<Value<'v>> {
        Ok(this.cli_args)
    }

    /// Returns the [`BxlFilesystem`] for performing a basic set of filesystem operations within bxl
    #[starlark(attribute)]
    fn fs<'v>(this: &BxlContext<'v>) -> anyhow::Result<BxlFilesystem<'v>> {
        Ok(BxlFilesystem::new(&this.async_ctx))
    }
}
