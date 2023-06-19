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
use std::iter;
use std::sync::Arc;

use allocative::Allocative;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::bxl::execution_platform::EXECUTION_PLATFORM;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_cli_proto::build_request::Materializations;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::events::HasEvents;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::pattern::query_file_literal::parse_query_file_literal;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::TargetLabel;
use buck2_execute::digest_config::DigestConfig;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use buck2_interpreter::types::label::Label;
use buck2_interpreter::types::label::StarlarkProvidersLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use dashmap::DashMap;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use either::Either;
use indexmap::IndexSet;
use itertools::Itertools;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::dict::Dict;
use starlark::values::none::NoneType;
use starlark::values::structs::AllocStruct;
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

use crate::bxl::key::BxlKey;
use crate::bxl::starlark_defs::alloc_node::AllocNode;
use crate::bxl::starlark_defs::audit::StarlarkAuditCtx;
use crate::bxl::starlark_defs::context::actions::resolve_bxl_execution_platform;
use crate::bxl::starlark_defs::context::actions::validate_action_instantiation;
use crate::bxl::starlark_defs::context::actions::BxlActions;
use crate::bxl::starlark_defs::context::fs::BxlFilesystem;
use crate::bxl::starlark_defs::context::output::EnsuredArtifactOrGroup;
use crate::bxl::starlark_defs::context::output::OutputStream;
use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::cquery::StarlarkCQueryCtx;
use crate::bxl::starlark_defs::event::to_starlark_user_event;
use crate::bxl::starlark_defs::providers_expr::ProvidersExpr;
use crate::bxl::starlark_defs::target_expr::filter_incompatible;
use crate::bxl::starlark_defs::target_expr::TargetExpr;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
use crate::bxl::starlark_defs::uquery::StarlarkUQueryCtx;
use crate::bxl::value_as_starlark_target_label::ValueAsStarlarkTargetLabel;

pub mod actions;
pub mod analysis;
pub mod build;
pub mod fs;
pub mod output;
pub mod starlark_async;

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs(directory = "bxl")]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub struct BxlContext<'v> {
    pub(crate) current_bxl: BxlKey,
    #[derivative(Debug = "ignore")]
    pub(crate) target_alias_resolver: BuckConfigTargetAliasResolver,
    pub(crate) cell_name: CellName,
    pub(crate) cell_root_abs: AbsNormPathBuf,
    #[derivative(Debug = "ignore")]
    pub(crate) cell_resolver: CellResolver,
    cli_args: Value<'v>, // Struct of the cli args
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    pub(crate) async_ctx: BxlSafeDiceComputations<'v>,
    pub(crate) state: ValueTyped<'v, AnalysisActions<'v>>,
    pub(crate) output_stream: ValueTyped<'v, OutputStream<'v>>,
    pub(crate) error_stream: ValueTyped<'v, OutputStream<'v>>,
    pub(crate) global_target_platform: Option<TargetLabel>,
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    /// Use a RefCell/Option so when we are done with it, without obtaining exclusive access,
    /// we can take the internal state without having to clone it.
    pub(crate) materializations: Arc<DashMap<BuildArtifact, ()>>,
}

impl<'v> BxlContext<'v> {
    pub(crate) fn new(
        heap: &'v Heap,
        current_bxl: BxlKey,
        cli_args: Value<'v>,
        target_alias_resolver: BuckConfigTargetAliasResolver,
        project_fs: ProjectRoot,
        artifact_fs: ArtifactFs,
        cell_resolver: CellResolver,
        cell_name: CellName,
        async_ctx: BxlSafeDiceComputations<'v>,
        output_sink: RefCell<Box<dyn Write>>,
        error_sink: RefCell<Box<dyn Write>>,
        digest_config: DigestConfig,
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<Self> {
        let cell_root_abs = project_fs.root().join(
            cell_resolver
                .get(cell_name)?
                .path()
                .as_project_relative_path(),
        );
        Ok(Self {
            current_bxl,
            target_alias_resolver,
            cell_name,
            cell_root_abs,
            cell_resolver,
            cli_args,
            async_ctx: async_ctx.clone(),
            state: heap.alloc_typed(AnalysisActions {
                state: RefCell::new(None),
                attributes: heap.alloc(AllocStruct::EMPTY),
                digest_config,
            }),
            output_stream: heap.alloc_typed(OutputStream::new(
                project_fs.clone(),
                artifact_fs.clone(),
                output_sink,
                async_ctx.clone(),
            )),
            error_stream: heap.alloc_typed(OutputStream::new(
                project_fs,
                artifact_fs,
                error_sink,
                async_ctx,
            )),
            global_target_platform,
            materializations: Arc::new(DashMap::new()),
        })
    }

    // Used for caching error logs emitted from within the BXL core.
    pub(crate) fn print_to_error_stream(&self, msg: String) -> anyhow::Result<()> {
        writeln!(self.error_stream.sink.borrow_mut(), "{}", msg)?;
        Ok(())
    }

    pub(crate) fn project_root(&self) -> &ProjectRoot {
        &self.output_stream.project_fs
    }

    /// Working dir for resolving literals.
    /// Note, unlike buck2 command line UI, we resolve targets and literals
    /// against the cell root instead of user working dir.
    pub(crate) fn working_dir(&self) -> anyhow::Result<ProjectRelativePathBuf> {
        let cell = self.cell_resolver.get(self.cell_name)?;
        Ok(cell.path().as_project_relative_path().to_owned())
    }

    pub(crate) fn parse_query_file_literal(&self, literal: &str) -> anyhow::Result<CellPath> {
        parse_query_file_literal(
            literal,
            self.cell_resolver
                .get(self.cell_name)?
                .cell_alias_resolver(),
            &self.cell_resolver,
            // NOTE(nga): we pass cell root as working directory here,
            //   which is inconsistent with the rest of buck2:
            //   The same query `owner(foo.h)` is resolved using
            //   current directory in `buck2 query`, but relative to cell root in BXL.
            &self.cell_root_abs,
            self.project_root(),
        )
    }

    /// Must take an `AnalysisContext` and `OutputStream` which has never had `take_state` called on it before.
    pub(crate) fn take_state(
        value: ValueTyped<'v, BxlContext<'v>>,
    ) -> anyhow::Result<(
        Option<AnalysisRegistry<'v>>,
        IndexSet<ArtifactGroup>,
        Arc<DashMap<BuildArtifact, ()>>,
    )> {
        let this = value.as_ref();
        Ok((
            this.state.as_ref().state.borrow_mut().take(),
            // artifacts should be bound by now as the bxl has finished running
            this.output_stream
                .as_ref()
                .take_artifacts()
                .into_iter()
                .map(|ensured_artifact_type| match ensured_artifact_type {
                    EnsuredArtifactOrGroup::Artifact(artifact) => {
                        let as_artifact = artifact.as_artifact();
                        let bound_artifact = as_artifact.get_bound_artifact()?;
                        let associated_artifacts = as_artifact.get_associated_artifacts();

                        Ok(associated_artifacts
                            .iter()
                            .flat_map(|v| v.iter())
                            .cloned()
                            .chain(iter::once(ArtifactGroup::Artifact(bound_artifact)))
                            .collect::<Vec<_>>())
                    }
                    EnsuredArtifactOrGroup::ArtifactGroup(ag) => Ok(vec![ag]),
                })
                .flatten_ok()
                .collect::<anyhow::Result<IndexSet<ArtifactGroup>>>()?,
            this.materializations.dupe(),
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

/// The bxl context that the top level bxl implementation receives as parameter.
/// This context contains all the core bxl functions to query, build, create actions, etc.
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
            .project_root()
            .root()
            .to_str()?
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
    /// Note that this function does not accept `Label` (which is a configured provider label), since this
    /// is the label of a subtarget. You can get the underlying configured target label on the `Label`
    /// using `configured_targets()` (ex: `my_label.configured_target()`).
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
        let target_platform = target_platform.parse_target_platforms(
            &this.target_alias_resolver,
            &this.cell_resolver,
            this.cell_name,
            &this.global_target_platform,
        )?;

        let res: anyhow::Result<Value<'v>> = this.async_ctx.via_dice(|ctx| async move {
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
                        let set = filter_incompatible(
                            iter::once(ctx.get_configured_target_node(&label).await?),
                            this,
                        )?;

                        // When a target label is passed in, we should only get one target node.
                        // filter_incompatible() returns a set, so lets assert the size
                        assert!(set.len() <= 1);

                        if let Some(node) = set.iter().next() {
                            node.clone().alloc(eval.heap())
                        } else {
                            Value::new_none()
                        }
                    }

                    TargetExpr::Node(node) => node.alloc(eval.heap()),
                    multi => eval
                        .heap()
                        .alloc(StarlarkTargetSet::from(filter_incompatible(
                            multi.get(this.async_ctx.0).await?.into_iter(),
                            this,
                        )?)),
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
            Ok(
                match TargetExpr::<'v, TargetNode>::unpack(labels, this, eval).await? {
                    TargetExpr::Label(label) => {
                        let node = ctx.get_target_node(&label).await?;

                        node.alloc(eval.heap())
                    }

                    TargetExpr::Node(node) => node.alloc(eval.heap()),
                    multi => eval
                        .heap()
                        .alloc(StarlarkTargetSet::from(multi.get(ctx).await?.into_owned())),
                },
            )
        });

        res
    }

    /// Returns the [`StarlarkUQueryCtx`] that holds all uquery functions.
    fn uquery<'v>(this: &'v BxlContext<'v>) -> anyhow::Result<StarlarkUQueryCtx<'v>> {
        StarlarkUQueryCtx::new(this)
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
            .via(|| StarlarkCQueryCtx::new(this, target_platform, &this.global_target_platform))
    }

    /// Returns the bxl actions to create and register actions for this
    /// bxl function. This will have the execution platform resolved according to the execution
    /// deps and toolchains you pass into this function.
    /// You'll be able to access the analysis action factory of the correct execution platform,
    /// toolchains, and execution deps of the corresponding configuration via this context.
    ///
    /// Actions created by bxl will not be built by default. Instead, they are marked to be built
    /// by `ctx.output.ensure(artifact)` on the output module of the [`BxlContext`]. Only artifacts
    /// marked by ensure will be built.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_write_action(ctx):
    ///     bxl_actions = ctx.bxl_actions()
    ///     output = bxl_actions.actions.write("my_output", "my_content")
    ///     ensured = ctx.output.ensure(output)
    ///     ctx.output.print(ensured)
    /// ```
    ///
    /// There are several optional named parameters:
    ///
    /// `exec_deps` - These are dependencies you wish to access as executables for creating the action.
    /// This is usually the same set of targets one would pass to rule's `attr.exec_dep`.
    /// `toolchains` - The set of toolchains needed for the actions you intend to create.
    /// `target_platform` - The intended target platform for your toolchains
    /// `exec_compatible_with` - Explicit list of configuration nodes (like platforms or constraints)
    /// that these actions are compatible with. This is the 'exec_compatible_with' attribute of a target.
    #[starlark(return_type = "\"bxl_actions\"")]
    fn bxl_actions<'v>(
        this: &'v BxlContext<'v>,
        #[starlark(require = named, default = NoneType)] exec_deps: Value<'v>,
        #[starlark(require = named, default = NoneType)] toolchains: Value<'v>,
        #[starlark(require = named, default = NoneType)] target_platform: Value<'v>,
        #[starlark(require = named, default = NoneType)] exec_compatible_with: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let execution_resolution = this.async_ctx.via_dice(|ctx| async {
            let target_platform = target_platform.parse_target_platforms(
                &this.target_alias_resolver,
                &this.cell_resolver,
                this.cell_name,
                &this.global_target_platform,
            )?;

            let exec_deps = if exec_deps.is_none() {
                Vec::new()
            } else {
                ProvidersExpr::<ProvidersLabel>::unpack(exec_deps, this, eval)?
                    .labels()
                    .cloned()
                    .collect()
            };

            let toolchains = if toolchains.is_none() {
                Vec::new()
            } else {
                ProvidersExpr::<ProvidersLabel>::unpack(toolchains, this, eval)?
                    .labels()
                    .cloned()
                    .collect()
            };

            let exec_compatible_with = if exec_compatible_with.is_none() {
                Vec::new()
            } else {
                TargetExpr::<TargetNode>::unpack(exec_compatible_with, this, eval)
                    .await?
                    .get(ctx)
                    .await?
                    .iter()
                    .map(|n| n.label().dupe())
                    .collect()
            };

            resolve_bxl_execution_platform(
                ctx,
                this.cell_name.dupe(),
                exec_deps,
                toolchains,
                target_platform,
                exec_compatible_with,
                eval.module(),
            )
            .await
        })?;

        validate_action_instantiation(this, execution_resolution.resolved_execution)?;

        let exec_deps = eval.heap().alloc(Dict::new(
            execution_resolution
                .exec_deps_configured
                .into_iter()
                .map(|(k, v)| {
                    Ok((
                        eval.heap()
                            .alloc(StarlarkProvidersLabel::new(k))
                            .get_hashed()?,
                        eval.heap().alloc(v),
                    ))
                })
                .collect::<anyhow::Result<_>>()?,
        ));
        let toolchains = eval.heap().alloc(Dict::new(
            execution_resolution
                .toolchain_deps_configured
                .into_iter()
                .map(|(k, v)| {
                    Ok((
                        eval.heap()
                            .alloc(StarlarkProvidersLabel::new(k))
                            .get_hashed()?,
                        eval.heap().alloc(v),
                    ))
                })
                .collect::<anyhow::Result<_>>()?,
        ));

        Ok(eval
            .heap()
            .alloc(BxlActions::new(this.state, exec_deps, toolchains)))
    }

    /// Returns the action context for creating and running actions.
    #[starlark(attribute)]
    fn actions_factory<'v>(this: ValueOf<'v, &'v BxlContext<'v>>) -> anyhow::Result<Value<'v>> {
        validate_action_instantiation(this.typed, EXECUTION_PLATFORM.dupe())?;

        Ok(this.typed.state.to_value())
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
        this: &'v BxlContext<'v>,
        labels: Value<'v>,
        #[starlark(default = NoneType)] target_platform: Value<'v>,
        #[starlark(default = true)] skip_incompatible: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let providers =
            ProvidersExpr::<ConfiguredProvidersLabel>::unpack(labels, target_platform, this, eval)?;

        let res: anyhow::Result<_> = this
            .async_ctx
            .via_dice(|ctx| analysis::analysis(ctx, providers, skip_incompatible));

        Ok(match res? {
            Either::Left(single) => eval.heap().alloc(single),
            Either::Right(many) => eval.heap().alloc(Dict::new(
                many.into_iter()
                    .map(|(t, v)| {
                        Ok((
                            eval.heap().alloc(Label::new(t)).get_hashed()?,
                            eval.heap().alloc(v),
                        ))
                    })
                    .collect::<anyhow::Result<_>>()?,
            )),
        })
    }

    /// Runs a build on the given `labels`, accepting an optional `target_platform` which is the
    /// target platform configuration used to resolve configurations. Note that when `build()` is called,
    /// the artifacts are materialized without needing to additionally call `ensure()` on them.
    ///
    /// The given `labels` is a providers expression, which is either:
    ///     - a single string that is a `target pattern`.
    ///     - a single target node or label, configured or unconfigured
    ///     - a single provider label, configured or unconfigured
    ///     - a list of the two options above.
    ///
    /// This returns a dict keyed by sub target labels of [`StarlarkBuildResult`] if the
    /// given `labels` is list-like
    ///
    fn build<'v>(
        this: &'v BxlContext<'v>,
        spec: Value<'v>,
        #[starlark(default = NoneType)] target_platform: Value<'v>,
        #[starlark(require = named, default = "default")] materializations: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        Ok(eval.heap().alloc(Dict::new(build::build(
            this,
            spec,
            target_platform,
            Materializations::from_str_name(&materializations.to_uppercase()).ok_or_else(|| {
                anyhow::anyhow!("Unknown materialization setting `{}`", materializations)
            })?,
            eval,
        )?)))
    }

    /// A struct of the command line args as declared using the [`cli_args`] module.
    /// These command lines are resolved per the users input on the cli when invoking the bxl script.
    ///
    /// If you wish to pass in a kebab-cased arg, the arg accessed from the BXL context's `cli_args`
    /// attrbute will always be in snakecase. For example, if you passed in `my-arg`, accessing it
    /// within BXL would look like `ctx.cli_args.my_arg`.
    #[starlark(attribute)]
    fn cli_args<'v>(this: &BxlContext<'v>) -> anyhow::Result<Value<'v>> {
        Ok(this.cli_args)
    }

    /// Returns the [`BxlFilesystem`] for performing a basic set of filesystem operations within bxl
    #[starlark(attribute)]
    fn fs<'v>(this: &BxlContext<'v>) -> anyhow::Result<BxlFilesystem<'v>> {
        Ok(BxlFilesystem::new(this))
    }

    /// Returns the [`StarlarkAuditCtx`] that holds all the audit functions.
    fn audit<'v>(this: &'v BxlContext<'v>) -> anyhow::Result<StarlarkAuditCtx<'v>> {
        this.async_ctx.via_dice(|ctx| async move {
            let working_dir = this
                .cell_resolver
                .get(this.cell_name)?
                .path()
                .as_project_relative_path();
            let cell_resolver = ctx.get_cell_resolver().await?;

            StarlarkAuditCtx::new(
                this,
                working_dir.to_buf(),
                cell_resolver,
                this.global_target_platform.clone(),
            )
        })
    }

    /// Awaits a promise and returns an optional value of the promise.
    ///
    /// Sample usage:
    /// ```python
    /// load("//path/to/rules:rules.bzl", "my_anon_targets_rule", "my_map_function")
    ///
    /// def _resolve_impl(ctx):
    ///     actions = ctx.actions_factory
    ///     my_attrs = {
    ///         "false": False,
    ///         "int": 42,
    ///         "list_string": ["a", "b", "c"],
    ///         "string": "a-string",
    ///         "true": True,
    ///     }
    ///
    ///     promise = actions.anon_target(my_anon_targets_rule, attrs).map(my_map_function)
    ///     providers_result = ctx.resolve(promise) # result is `provider_callable` type, which is a collection of `provider`s
    ///     ctx.output.print(providers_result[0].my_field)
    /// ```
    fn resolve<'v>(
        this: &'v BxlContext<'v>,
        action_factory: ValueTyped<'v, AnalysisActions<'v>>,
        promise: ValueTyped<'v, StarlarkPromise<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Option<Value<'v>>> {
        this.async_ctx.via_dice(|dice| {
            action_factory.run_promises(dice, eval, format!("bxl$promises:{}", &this.current_bxl))
        })?;
        Ok(promise.get())
    }

    /// Emits a user-defined instant event, taking in a required string id and a metadata dictionary where the
    /// keys are strings, and values are either strings, bools, or ints. The id is user-supplied, and used to
    /// identify the instant events in the event logs more easily.
    fn instant_event<'v>(
        this: &BxlContext<'v>,
        #[starlark(require = named)] id: &str,
        #[starlark(require = named)] metadata: Value<'v>,
    ) -> anyhow::Result<NoneType> {
        let event = to_starlark_user_event(id, metadata)?;

        this.async_ctx
            .0
            .per_transaction_data()
            .get_dispatcher()
            .instant_event(event);

        Ok(NoneType)
    }
}
