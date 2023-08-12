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
use std::fmt::Display;
use std::io::Write;
use std::iter;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::deferred::types::DeferredCtx;
use buck2_build_api::dynamic::bxl::EVAL_BXL_FOR_DYNAMIC_OUTPUT;
use buck2_build_api::dynamic::deferred::dynamic_lambda_ctx_data;
use buck2_build_api::dynamic::deferred::DynamicLambda;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::plugins::AnalysisPlugins;
use buck2_cli_proto::build_request::Materializations;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::events::HasEvents;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::base_deferred_key::BaseDeferredKeyDyn;
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
use buck2_events::dispatch::console_message;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use dashmap::DashMap;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use either::Either;
use indexmap::IndexSet;
use itertools::Itertools;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::dict::Dict;
use starlark::values::none::NoneType;
use starlark::values::starlark_value;
use starlark::values::structs::AllocStruct;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::StarlarkDocs;
use thiserror::Error;

use crate::bxl::key::BxlDynamicKey;
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

pub(crate) mod actions;
pub(crate) mod analysis;
pub(crate) mod build;
pub(crate) mod fs;
pub(crate) mod output;
pub(crate) mod starlark_async;

#[derive(Error, Debug)]
enum BxlContextDynamicError {
    #[error("`{0}()` is unsupported")]
    Unsupported(String),
    #[error("Execution platform is inherited from the root BXL")]
    RequireSameExecutionPlatformAsRoot,
}

/// Data object for `BxlContextType::Root`.
#[derive(ProvidesStaticType, Trace, NoSerialize, Allocative, Debug, Derivative)]
pub(crate) struct RootBxlContextData<'v> {
    output_stream: ValueTyped<'v, OutputStream<'v>>,
    error_stream: ValueTyped<'v, OutputStream<'v>>,
    cli_args: Value<'v>,
    /// Use a RefCell/Option so when we are done with it, without obtaining exclusive access,
    /// we can take the internal state without having to clone it.
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    materializations: Arc<DashMap<BuildArtifact, ()>>,
}

/// Data object for `BxlContextType::Dynamic`.
#[derive(ProvidesStaticType, Trace, Allocative, Debug, Derivative)]
pub(crate) struct DynamicBxlContextData {
    exec_deps: Vec<ConfiguredProvidersLabel>,
    toolchains: Vec<ConfiguredProvidersLabel>,
}

/// Environment-specific fields of `BxlContext`.
#[derive(ProvidesStaticType, Trace, NoSerialize, Allocative, Debug, Derivative)]
pub(crate) enum BxlContextType<'v> {
    /// Context passed to `ctx` parameter to BXL entry function
    Root(RootBxlContextData<'v>),
    /// Context passed to `ctx` parameter to the dynamic lambda entry function
    Dynamic(DynamicBxlContextData),
}

impl<'v> BxlContextType<'v> {
    fn unpack_root(&self) -> anyhow::Result<&'v RootBxlContextData> {
        match &self {
            BxlContextType::Root(root) => Ok(root),
            BxlContextType::Dynamic(_) => Err(anyhow::anyhow!("Expected root BXL context type")),
        }
    }
}

impl<'v> Display for BxlContextType<'v> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            BxlContextType::Root { .. } => {
                write!(f, "root")
            }
            BxlContextType::Dynamic(_) => {
                write!(f, "dynamic")
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
    StarlarkDocs,
    Allocative
)]
#[starlark_docs(directory = "bxl")]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub(crate) struct BxlContext<'v> {
    pub(crate) current_bxl: BxlKey,
    #[derivative(Debug = "ignore")]
    pub(crate) target_alias_resolver: BuckConfigTargetAliasResolver,
    pub(crate) cell_name: CellName,
    pub(crate) cell_root_abs: AbsNormPathBuf,
    #[derivative(Debug = "ignore")]
    pub(crate) cell_resolver: CellResolver,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    pub(crate) async_ctx: BxlSafeDiceComputations<'v>,
    pub(crate) state: ValueTyped<'v, AnalysisActions<'v>>,
    pub(crate) global_target_platform: Option<TargetLabel>,
    pub(crate) context_type: BxlContextType<'v>,
    pub(crate) project_fs: ProjectRoot,
    #[derivative(Debug = "ignore")]
    pub(crate) artifact_fs: ArtifactFs,
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

        let root_data = RootBxlContextData {
            cli_args,
            output_stream: heap.alloc_typed(OutputStream::new(
                project_fs.clone(),
                artifact_fs.clone(),
                output_sink,
                async_ctx.clone(),
            )),
            error_stream: heap.alloc_typed(OutputStream::new(
                project_fs.clone(),
                artifact_fs.clone(),
                error_sink,
                async_ctx.clone(),
            )),
            materializations: Arc::new(DashMap::new()),
        };

        let context_type = BxlContextType::Root(root_data);

        Ok(Self {
            current_bxl,
            target_alias_resolver,
            cell_name,
            cell_root_abs,
            cell_resolver,
            async_ctx: async_ctx.clone(),
            state: heap.alloc_typed(AnalysisActions {
                state: RefCell::new(None),
                // TODO(nga): attributes struct should not be accessible to BXL.
                attributes: ValueOfUnchecked::new_checked(heap.alloc(AllocStruct::EMPTY)).unwrap(),
                plugins: heap
                    .alloc_typed(AnalysisPlugins::new(SmallMap::new()))
                    .into(),
                digest_config,
            }),
            global_target_platform,
            context_type,
            project_fs,
            artifact_fs,
        })
    }

    pub(crate) fn new_dynamic(
        heap: &'v Heap,
        current_bxl: BxlKey,
        target_alias_resolver: BuckConfigTargetAliasResolver,
        project_fs: ProjectRoot,
        artifact_fs: ArtifactFs,
        cell_resolver: CellResolver,
        cell_name: CellName,
        async_ctx: BxlSafeDiceComputations<'v>,
        digest_config: DigestConfig,
        global_target_platform: Option<TargetLabel>,
        analysis_registry: AnalysisRegistry<'v>,
        dynamic_data: DynamicBxlContextData,
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
            async_ctx: async_ctx.clone(),
            state: heap.alloc_typed(AnalysisActions {
                state: RefCell::new(Some(analysis_registry)),
                // TODO(nga): attributes struct should not be accessible to BXL.
                attributes: ValueOfUnchecked::new_checked(heap.alloc(AllocStruct::EMPTY)).unwrap(),
                plugins: heap
                    .alloc_typed(AnalysisPlugins::new(SmallMap::new()))
                    .into(),
                digest_config,
            }),
            global_target_platform,
            context_type: BxlContextType::Dynamic(dynamic_data),
            project_fs,
            artifact_fs,
        })
    }

    // Used for caching error logs emitted from within the BXL core.
    pub(crate) fn print_to_error_stream(&self, msg: String) -> anyhow::Result<()> {
        match &self.context_type {
            BxlContextType::Root(root) => writeln!(root.error_stream.sink.borrow_mut(), "{}", msg)?,
            BxlContextType::Dynamic(_) => console_message(msg),
        }
        Ok(())
    }

    pub(crate) fn project_root(&self) -> &ProjectRoot {
        &self.project_fs
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
        let root_data = this.context_type.unpack_root()?;
        let output_stream = &root_data.output_stream;
        let materializations = &root_data.materializations;

        Ok((
            this.state.as_ref().state.borrow_mut().take(),
            // artifacts should be bound by now as the bxl has finished running
            output_stream
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
            materializations.dupe(),
        ))
    }

    /// Must take an `AnalysisContext` which has never had `take_state` called on it before.
    pub(crate) fn take_state_dynamic(&self) -> anyhow::Result<AnalysisRegistry<'v>> {
        let state = self.state.as_ref();
        state.state().assert_no_promises()?;

        Ok(state
            .state
            .borrow_mut()
            .take()
            .expect("nothing to have stolen state yet"))
    }
}

pub(crate) async fn eval_bxl_for_dynamic_output<'v>(
    base_deferred_key: &'v Arc<dyn BaseDeferredKeyDyn>,
    dynamic_lambda: &'v DynamicLambda,
    deferred_ctx: &'v mut dyn DeferredCtx,
    dice_ctx: &'v DiceComputations,
) -> anyhow::Result<Vec<ActionKey>> {
    // TODO(wendyy) emit telemetry, support profiler
    let env = Module::new();
    let liveness = deferred_ctx.liveness();
    let dynamic_key =
        BxlDynamicKey::from_base_deferred_key_dyn_impl_err(base_deferred_key.clone())?;
    let key = dynamic_key.key();
    let dynamic_data = DynamicBxlContextData {
        exec_deps: dynamic_key.0.exec_deps.clone(),
        toolchains: dynamic_key.0.toolchains.clone(),
    };
    let global_target_platform = key.global_target_platform().dupe();
    let async_ctx: BxlSafeDiceComputations<'_> =
        BxlSafeDiceComputations::new(dice_ctx.dupe(), liveness);
    let label = key.label();
    let cell_resolver = dice_ctx.get_cell_resolver().await?;
    let cell = label.bxl_path.cell();
    let bxl_cell = cell_resolver
        .get(cell)
        .with_context(|| format!("Cell does not exist: `{}`", cell))?
        .dupe();
    let cell_name = bxl_cell.name();
    let target_alias_resolver = dice_ctx.target_alias_resolver_for_cell(cell_name).await?;
    let artifact_fs = dice_ctx.get_artifact_fs().await?;
    let digest_config = dice_ctx.global_data().get_digest_config();
    let project_fs = dice_ctx
        .global_data()
        .get_io_provider()
        .project_root()
        .dupe();

    // Note: because we use `block_in_place`, that will prevent the inner future from being polled
    // and yielded. So, for cancellation observers to work properly within the dice cancellable
    // future context, we need the future that it's attached to the cancellation context can
    // yield and be polled. To ensure that, we have to spawn the future that then enters block_in_place

    let dispatcher = dice_ctx.per_transaction_data().get_dispatcher().dupe();
    let print = EventDispatcherPrintHandler(dispatcher.dupe());

    let (_, futs) = unsafe {
        // SAFETY: as long as we don't `forget` the return object from `scope_and_collect`, it is safe

        // Additional cancellation notes:
        // the `scope_and_collect` will block on drop, but it will move the blocking to a tokio
        // blocking thread, freeing up the main worker threads. Additionally, the `spawn_cancellable`
        // on the scope will be dropped at the earliest await point. If we are within the blocking
        // section of bxl, the cancellation observer will be notified and cause the blocking calls
        // to terminate.
        async_scoped::TokioScope::scope_and_collect(|s| {
            s.spawn_cancellable(
                with_dispatcher_async(dispatcher.dupe(), async move {
                    with_starlark_eval_provider(
                        dice_ctx,
                        &mut StarlarkProfilerOrInstrumentation::disabled(),
                        format!("bxl_dynamic:{}", "foo"),
                        move |provider, _| {
                            tokio::task::block_in_place(|| {
                                let mut eval = provider.make(&env)?;
                                eval.set_print_handler(&print);

                                let (analysis_registry, declared_outputs) = {
                                    let dynamic_lambda_ctx_data = dynamic_lambda_ctx_data(
                                        dynamic_lambda,
                                        deferred_ctx,
                                        &env,
                                    )?;

                                    let bxl_dynamic_ctx = BxlContext::new_dynamic(
                                        env.heap(),
                                        key,
                                        target_alias_resolver,
                                        project_fs,
                                        artifact_fs,
                                        cell_resolver,
                                        cell_name,
                                        async_ctx,
                                        digest_config,
                                        global_target_platform,
                                        dynamic_lambda_ctx_data.registry,
                                        dynamic_data,
                                    )?;

                                    let ctx = ValueTyped::<BxlContext>::new(
                                        env.heap().alloc(bxl_dynamic_ctx),
                                    )
                                    .unwrap();

                                    eval.eval_function(
                                        dynamic_lambda_ctx_data.lambda,
                                        &[
                                            ctx.to_value(),
                                            dynamic_lambda_ctx_data.artifacts,
                                            dynamic_lambda_ctx_data.outputs,
                                        ],
                                        &[],
                                    )?;

                                    (
                                        ctx.take_state_dynamic()?,
                                        dynamic_lambda_ctx_data.declared_outputs,
                                    )
                                };

                                std::mem::drop(eval);

                                let (_frozen_env, deferred) =
                                    analysis_registry.finalize(&env)?(env)?;
                                let _fake_registry =
                                    std::mem::replace(deferred_ctx.registry(), deferred);
                                let output: anyhow::Result<Vec<_>> = declared_outputs
                                    .into_iter()
                                    .map(|x| anyhow::Ok(x.ensure_bound()?.action_key().dupe()))
                                    .collect();
                                output
                            })
                        },
                    )
                    .await
                }),
                || Err(anyhow::anyhow!("cancelled")),
            )
        })
    }
    .await;

    match futs.into_iter().exactly_one() {
        Ok(res) => res?,
        Err(_) => panic!("only spawned one task"),
    }
}

pub(crate) fn init_eval_bxl_for_dynamic_output() {
    EVAL_BXL_FOR_DYNAMIC_OUTPUT.init(
        |base_deferred_key, dynamic_lambda, deferred_ctx, dice_ctx| {
            Box::pin(eval_bxl_for_dynamic_output(
                base_deferred_key,
                dynamic_lambda,
                deferred_ctx,
                dice_ctx,
            ))
        },
    );
}

#[starlark_value(type = "bxl_ctx", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for BxlContext<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(context_methods)
    }
}

impl<'v> AllocValue<'v> for BxlContext<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

/// The bxl context that the top level bxl implementation receives as parameter.
/// This context contains all the core bxl functions to query, build, create actions, etc.
#[starlark_module]
fn context_methods(builder: &mut MethodsBuilder) {
    /// Gets the output stream to the console via stdout. Items written to the output stream
    /// are considered to be the results of a bxl script, which will be displayed to stdout by
    /// buck2 even when the script is cached.
    ///
    /// Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
    /// and `pprint`.
    ///
    /// This function is not available on the `bxl_ctx` when called from `dynamic_output`.
    #[starlark(attribute)]
    fn output<'v>(this: &'v BxlContext) -> anyhow::Result<Value<'v>> {
        let output_stream = this
            .context_type
            .unpack_root()
            .context(BxlContextDynamicError::Unsupported("output".to_owned()))?
            .output_stream;
        Ok(output_stream.to_value())
    }

    /// Returns the absolute path to the root of the repository
    ///
    /// This function is not available on the `bxl_ctx` when called from `dynamic_output`.
    fn root<'v>(this: &'v BxlContext<'v>) -> anyhow::Result<String> {
        let _root_type = this
            .context_type
            .unpack_root()
            .context(BxlContextDynamicError::Unsupported("root".to_owned()))?;
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

    /// Returns the absolute path to the cell of the repository
    ///
    /// This function is not available on the `bxl_ctx` when called from `dynamic_output`.
    fn cell_root<'v>(this: &'v BxlContext<'v>) -> anyhow::Result<String> {
        let _root_type = this
            .context_type
            .unpack_root()
            .context(BxlContextDynamicError::Unsupported("root".to_owned()))?;
        Ok(this.cell_root_abs.to_owned().to_string())
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
                    ctx,
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
                match TargetExpr::<'v, TargetNode>::unpack(labels, this, ctx, eval).await? {
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

    /// Gets the unconfigured subtargets for the given `labels`
    ///
    /// The given `labels` is a providers expression, which is either:
    ///     - a single string that is a `target pattern`.
    ///     - a single target node or label, configured or unconfigured
    ///     - a single subtarget label, configured or unconfigured
    ///     - a list of the two options above.
    ///
    /// This returns either a single [`StarlarkProvidersLabel`] if the given `labels`
    /// is "singular", or dict of the subtarget string representation to the
    /// [`StarlarkProvidersLabel`] if the given `labels` is list-like.
    ///
    /// Note that this function does not check that this subtarget exists in the repo.
    fn unconfigured_sub_targets<'v>(
        this: &BxlContext<'v>,
        labels: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let providers = this
            .async_ctx
            .via_dice(|_| ProvidersExpr::<ProvidersLabel>::unpack(labels, this, eval))?;

        let res = match providers {
            ProvidersExpr::Literal(provider) => {
                eval.heap().alloc(StarlarkProvidersLabel::new(provider))
            }
            ProvidersExpr::Iterable(providers) => eval.heap().alloc(Dict::new(
                providers
                    .into_iter()
                    .map(|p| {
                        Ok((
                            eval.heap()
                                .alloc_str(&p.to_string())
                                .to_value()
                                .get_hashed()?,
                            eval.heap().alloc(StarlarkProvidersLabel::new(p)),
                        ))
                    })
                    .collect::<anyhow::Result<_>>()?,
            )),
        };

        Ok(res)
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
    /// ```python
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
    ///
    /// If you passed in `exec_deps` or `toolchains`, you can access the resolved dependencies using the `exec_deps`
    /// and `toolchains` attributes on the `bxl_actions`, which both return a `dict` of unconfigured subtarget labels
    /// and their configured/resolved `dependency` objects.
    ///
    /// ```python
    /// def _impl_run_action(ctx):
    ///    my_exec_dep = ctx.unconfigured_sub_targets("foo//bar:baz") # has some provider that you would use in the action
    ///    bxl_actions = ctx.bxl_actions(exec_deps = [my_exec_dep]) # call once, reuse wherever needed
    ///    output = bxl_actions.actions.run(
    ///        [
    ///            "python3",
    ///            bxl_actions.exec_deps[my_exec_dep][RunInfo], # access resolved exec_deps on the `bxl_actions`
    ///            out.as_output(),
    ///        ],
    ///        category = "command",
    ///        local_only = True,
    ///    )
    ///    ctx.output.ensure(output)
    /// ```
    ///
    /// When called from a `dynamic_output`, `bxl_actions()` cannot be configured with a different execution
    /// platform resolution from the parent BXL.
    fn bxl_actions<'v>(
        this: &'v BxlContext<'v>,
        #[starlark(require = named, default = NoneType)] exec_deps: Value<'v>,
        #[starlark(require = named, default = NoneType)] toolchains: Value<'v>,
        #[starlark(require = named, default = NoneType)] target_platform: Value<'v>,
        #[starlark(require = named, default = NoneType)] exec_compatible_with: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<BxlActions<'v>> {
        this.async_ctx.via_dice(|ctx| async {
            let (exec_deps, toolchains) = match &this.context_type {
                BxlContextType::Root { .. } => {
                    let target_platform = target_platform.parse_target_platforms(
                        &this.target_alias_resolver,
                        &this.cell_resolver,
                        this.cell_name,
                        &this.global_target_platform,
                    )?;

                    let exec_deps = if exec_deps.is_none() {
                        Vec::new()
                    } else {
                        ProvidersExpr::<ProvidersLabel>::unpack(exec_deps, this, eval)
                            .await?
                            .labels()
                            .cloned()
                            .collect()
                    };

                    let toolchains = if toolchains.is_none() {
                        Vec::new()
                    } else {
                        ProvidersExpr::<ProvidersLabel>::unpack(toolchains, this, eval)
                            .await?
                            .labels()
                            .cloned()
                            .collect()
                    };

                    let exec_compatible_with = if exec_compatible_with.is_none() {
                        Vec::new()
                    } else {
                        TargetExpr::<TargetNode>::unpack(exec_compatible_with, this, ctx, eval)
                            .await?
                            .get(ctx)
                            .await?
                            .iter()
                            .map(|n| n.label().dupe())
                            .collect()
                    };

                    let execution_resolution = resolve_bxl_execution_platform(
                        ctx,
                        this.cell_name,
                        exec_deps,
                        toolchains,
                        target_platform.clone(),
                        exec_compatible_with.clone(),
                        eval.module(),
                    )
                    .await?;

                    validate_action_instantiation(this, &execution_resolution)?;

                    (
                        execution_resolution.exec_deps_configured,
                        execution_resolution.toolchain_deps_configured,
                    )
                }
                BxlContextType::Dynamic(data) => {
                    if !exec_deps.is_none()
                        || !toolchains.is_none()
                        || !target_platform.is_none()
                        || !exec_compatible_with.is_none()
                    {
                        return Err(
                            BxlContextDynamicError::RequireSameExecutionPlatformAsRoot.into()
                        );
                    }
                    (data.exec_deps.clone(), data.toolchains.clone())
                }
            };

            BxlActions::new(
                this.state,
                exec_deps.to_vec(),
                toolchains.to_vec(),
                eval,
                ctx,
            )
            .await
        })
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
        #[starlark(require = named, default = true)] skip_incompatible: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let res: anyhow::Result<_> = this.async_ctx.via_dice(|dice| async {
            let providers = ProvidersExpr::<ConfiguredProvidersLabel>::unpack(
                labels,
                target_platform,
                this,
                dice,
                eval,
            )
            .await?;
            analysis::analysis(dice, this, providers, skip_incompatible).await
        });

        Ok(match res? {
            Either::Left(single) => eval.heap().alloc(single),
            Either::Right(many) => eval.heap().alloc(Dict::new(
                many.into_iter()
                    .map(|(t, v)| {
                        Ok((
                            eval.heap()
                                .alloc(StarlarkConfiguredProvidersLabel::new(t))
                                .get_hashed()?,
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
    /// This function is not available on the `bxl_ctx` when called from `dynamic_output`.
    fn build<'v>(
        this: &'v BxlContext<'v>,
        labels: Value<'v>,
        #[starlark(default = NoneType)] target_platform: Value<'v>,
        #[starlark(require = named, default = "default")] materializations: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let materialization_setting = materializations;
        let materializations = &this
            .context_type
            .unpack_root()
            .context(BxlContextDynamicError::Unsupported("build".to_owned()))?
            .materializations;
        Ok(eval.heap().alloc(Dict::new(build::build(
            this,
            materializations,
            labels,
            target_platform,
            Materializations::from_str_name(&materialization_setting.to_uppercase()).ok_or_else(
                || {
                    anyhow::anyhow!(
                        "Unknown materialization setting `{}`",
                        materialization_setting
                    )
                },
            )?,
            eval,
        )?)))
    }

    /// A struct of the command line args as declared using the [`cli_args`] module.
    /// These command lines are resolved per the users input on the cli when invoking the bxl script.
    ///
    /// If you wish to pass in a kebab-cased arg, the arg accessed from the BXL context's `cli_args`
    /// attrbute will always be in snakecase. For example, if you passed in `my-arg`, accessing it
    /// within BXL would look like `ctx.cli_args.my_arg`.
    ///
    /// This attribute is not available on the bxl context within the a dynamic lambda.
    #[starlark(attribute)]
    fn cli_args<'v>(this: &BxlContext<'v>) -> anyhow::Result<Value<'v>> {
        let cli_args = this
            .context_type
            .unpack_root()
            .context(BxlContextDynamicError::Unsupported("cli_args".to_owned()))?
            .cli_args;

        Ok(cli_args)
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
    ///     actions = ctx.bxl_actions().actions
    ///     my_attrs = {
    ///         "false": False,
    ///         "int": 42,
    ///         "list_string": ["a", "b", "c"],
    ///         "string": "a-string",
    ///         "true": True,
    ///     }
    ///
    ///     promise = actions.anon_target(my_anon_targets_rule, attrs).map(my_map_function)
    ///     providers_result = ctx.resolve(actions, promise) # result is `provider_collection` type, which is a collection of `provider`s
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
