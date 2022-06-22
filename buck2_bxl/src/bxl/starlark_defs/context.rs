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
use std::collections::HashSet;
use std::io::Write;
use std::sync::Arc;

use buck2_build_api::actions::artifact::Artifact;
use buck2_build_api::actions::artifact::ArtifactFs;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::query::dice::DiceQueryDelegate;
use buck2_bxl_core::BxlKey;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_common::target_aliases::TargetAliasResolver;
use buck2_core::cells::CellInstance;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::target::TargetLabel;
use buck2_docs_gen::Buck2Docs;
use buck2_interpreter::types::label::Label;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use either::Either;
use gazebo::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::dict::Dict;
use starlark::values::none::NoneType;
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

use crate::bxl::starlark_defs::context::actions::BxlActionsCtx;
use crate::bxl::starlark_defs::context::fs::BxlFilesystem;
use crate::bxl::starlark_defs::context::output::OutputStream;
use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::cquery::StarlarkCQueryCtx;
use crate::bxl::starlark_defs::providers_expr::ProvidersExpr;
use crate::bxl::starlark_defs::uquery::StarlarkUQueryCtx;

pub mod actions;
pub mod analysis;
pub mod build;
pub mod fs;
pub mod output;
pub mod starlark_async;

/// The bxl context that the top level bxl implementation receives as parameter.
/// This context contains all the core bxl functions to query, build, create actions, etc.
#[derive(ProvidesStaticType, Derivative, Display, Trace, NoSerialize, Buck2Docs)]
#[buck2_docs(register_context)]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub struct BxlContext<'v> {
    #[trace(unsafe_ignore)]
    pub(crate) current_bxl: BxlKey,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    pub(crate) target_alias_resolver: TargetAliasResolver,
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
        target_alias_resolver: TargetAliasResolver,
        project_fs: Arc<ProjectFilesystem>,
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
            let fs = ProjectFilesystem::new(cwd.clone());
            fs.relativize(&cwd)?.as_ref().to_owned()
        };
        let cell_resolver = ctx.get_cell_resolver().await?;
        let project_root = cwd;
        let package_boundary_exceptions = ctx.get_package_boundary_exceptions().await?;
        let target_alias_resolver = ctx
            .target_alias_resolver_for_working_dir(&working_dir)
            .await?;
        DiceQueryDelegate::new(
            ctx,
            working_dir,
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
    ) -> anyhow::Result<(Option<AnalysisRegistry<'v>>, HashSet<Artifact>)> {
        let this = value.as_ref();
        Ok((
            this.state.as_ref().state.borrow_mut().take(),
            // artifacts should be bound by now as the bxl has finished running
            this.output_stream
                .as_ref()
                .take_artifacts()
                .into_iter()
                .map(|v| v.as_artifact().unwrap().get_bound())
                .collect::<anyhow::Result<_>>()?,
        ))
    }
}

impl<'v> StarlarkValue<'v> for BxlContext<'v> {
    starlark_type!("context");

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

impl<'v> UnpackValue<'v> for &'v BxlContext<'v> {
    fn expected() -> String {
        BxlContext::get_type_value_static().as_str().to_owned()
    }

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
            .root
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("Non utf-8 path"))?
            .to_owned())
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
    fn unstable_fs<'v>(this: &BxlContext<'v>) -> anyhow::Result<BxlFilesystem<'v>> {
        Ok(BxlFilesystem::new(&this.async_ctx))
    }
}
