/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The context containing the available buck commands and query operations for `bxl` functions.

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::io::Write;
use std::iter;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_action_impl::dynamic::bxl::EVAL_BXL_FOR_DYNAMIC_OUTPUT;
use buck2_action_impl::dynamic::deferred::dynamic_lambda_ctx_data;
use buck2_action_impl::dynamic::deferred::invoke_dynamic_output_lambda;
use buck2_action_impl::dynamic::deferred::DynamicLambdaArgs;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_artifact::deferred::key::DeferredHolderKey;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::analysis::registry::RecordedAnalysisValues;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::dynamic::params::FrozenDynamicLambdaParams;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_cli_proto::build_request::Materializations;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::events::HasEvents;
use buck2_common::global_cfg_options::GlobalCfgOptions;
use buck2_common::scope::scope_and_collect_with_dice;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::base_deferred_key::BaseDeferredKeyBxl;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::query_file_literal::parse_query_file_literal;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_events::dispatch::console_message;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_futures::cancellable_future::CancellationObserver;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::factory::StarlarkEvaluatorProvider;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::soft_error::Buck2StarlarkSoftErrorHandler;
use buck2_interpreter::starlark_profiler::profiler::StarlarkProfilerOpt;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
use buck2_node::configuration::resolved::ConfigurationSettingKey;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use dashmap::DashSet;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use either::Either;
use futures::FutureExt;
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
use starlark::values::none::NoneOr;
use starlark::values::none::NoneType;
use starlark::values::starlark_value;
use starlark::values::structs::StructRef;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::StarlarkDocs;

use crate::bxl::key::BxlDynamicKey;
use crate::bxl::key::BxlKey;
use crate::bxl::starlark_defs::analysis_result::StarlarkAnalysisResult;
use crate::bxl::starlark_defs::aquery::StarlarkAQueryCtx;
use crate::bxl::starlark_defs::audit::StarlarkAuditCtx;
use crate::bxl::starlark_defs::build_result::StarlarkBxlBuildResult;
use crate::bxl::starlark_defs::context::actions::resolve_bxl_execution_platform;
use crate::bxl::starlark_defs::context::actions::validate_action_instantiation;
use crate::bxl::starlark_defs::context::actions::BxlActions;
use crate::bxl::starlark_defs::context::actions::BxlExecutionResolution;
use crate::bxl::starlark_defs::context::fs::BxlFilesystem;
use crate::bxl::starlark_defs::context::output::EnsuredArtifactOrGroup;
use crate::bxl::starlark_defs::context::output::OutputStream;
use crate::bxl::starlark_defs::context::starlark_async::BxlDiceComputations;
use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::cquery::StarlarkCQueryCtx;
use crate::bxl::starlark_defs::event::StarlarkUserEventParser;
use crate::bxl::starlark_defs::nodes::configured::StarlarkConfiguredTargetNode;
use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;
use crate::bxl::starlark_defs::providers_expr::ConfiguredProvidersExprArg;
use crate::bxl::starlark_defs::providers_expr::ProviderExprArg;
use crate::bxl::starlark_defs::providers_expr::ProvidersExpr;
use crate::bxl::starlark_defs::tag::BxlEvalExtraTag;
use crate::bxl::starlark_defs::target_list_expr::filter_incompatible;
use crate::bxl::starlark_defs::target_list_expr::ConfiguredTargetListExprArg;
use crate::bxl::starlark_defs::target_list_expr::TargetListExpr;
use crate::bxl::starlark_defs::target_list_expr::TargetListExprArg;
use crate::bxl::starlark_defs::target_universe::StarlarkTargetUniverse;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
use crate::bxl::starlark_defs::uquery::StarlarkUQueryCtx;
use crate::bxl::value_as_starlark_target_label::ValueAsStarlarkTargetLabel;

pub(crate) mod actions;
pub(crate) mod analysis;
pub(crate) mod build;
pub(crate) mod fs;
pub(crate) mod output;
pub(crate) mod starlark_async;

#[derive(buck2_error::Error, Debug)]
enum BxlContextDynamicError {
    #[error("`{0}()` is unsupported")]
    Unsupported(String),
    #[error("Execution platform is inherited from the root BXL")]
    RequireSameExecutionPlatformAsRoot,
}

#[derive(buck2_error::Error, Debug)]
#[error("Expected a single target as a string literal, not a target pattern")]
struct NotATargetLabelString;

/// Data object for `BxlContextType::Root`.
#[derive(ProvidesStaticType, Trace, NoSerialize, Allocative, Debug, Derivative)]
pub(crate) struct RootBxlContextData<'v> {
    output_stream: ValueTyped<'v, OutputStream<'v>>,
    error_stream: ValueTyped<'v, OutputStream<'v>>,
    cli_args: ValueOfUnchecked<'v, StructRef<'v>>,
    /// Use a RefCell/Option so when we are done with it, without obtaining exclusive access,
    /// we can take the internal state without having to clone it.
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    materializations: Arc<DashSet<BuildArtifact>>,
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
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    pub(crate) async_ctx: Rc<RefCell<dyn BxlDiceComputations + 'v>>,
    pub(crate) data: BxlContextNoDice<'v>,
}

impl<'v> Deref for BxlContext<'v> {
    type Target = BxlContextNoDice<'v>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

#[derive(Derivative, Display, Trace, NoSerialize, Allocative)]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub(crate) struct BxlContextNoDice<'v> {
    pub(crate) state: ValueTyped<'v, AnalysisActions<'v>>,
    pub(crate) context_type: BxlContextType<'v>,
    core: BxlContextCoreData,
}

#[derive(Derivative, Display, Trace, Allocative)]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub(crate) struct BxlContextCoreData {
    current_bxl: BxlKey,
    #[derivative(Debug = "ignore")]
    target_alias_resolver: BuckConfigTargetAliasResolver,
    cell_name: CellName,
    cell_root_abs: AbsNormPathBuf,
    #[derivative(Debug = "ignore")]
    cell_resolver: CellResolver,
    #[derivative(Debug = "ignore")]
    cell_alias_resolver: CellAliasResolver,
    project_fs: ProjectRoot,
    #[derivative(Debug = "ignore")]
    artifact_fs: ArtifactFs,
}

impl BxlContextCoreData {
    pub(crate) async fn new(key: BxlKey, dice: &mut DiceComputations<'_>) -> anyhow::Result<Self> {
        let label = key.label();
        let cell_resolver = dice.get_cell_resolver().await?;
        let cell = label.bxl_path.cell();
        let bxl_cell = cell_resolver
            .get(cell)
            .with_context(|| format!("Cell does not exist: `{}`", cell))?
            .dupe();
        let cell_name = bxl_cell.name();
        let target_alias_resolver = dice.target_alias_resolver().await?;
        let cell_alias_resolver = dice.get_cell_alias_resolver(cell).await?;
        let artifact_fs = dice.get_artifact_fs().await?;
        let project_fs = dice.global_data().get_io_provider().project_root().dupe();

        let cell_root_abs = project_fs.root().join(
            cell_resolver
                .get(cell_name)?
                .path()
                .as_project_relative_path(),
        );

        Ok(Self {
            current_bxl: key,
            target_alias_resolver,
            cell_name,
            cell_root_abs,
            cell_resolver,
            cell_alias_resolver,
            project_fs,
            artifact_fs,
        })
    }

    pub(crate) fn key(&self) -> &BxlKey {
        &self.current_bxl
    }

    pub(crate) fn artifact_fs(&self) -> &ArtifactFs {
        &self.artifact_fs
    }

    pub(crate) fn project_fs(&self) -> &ProjectRoot {
        &self.project_fs
    }
}

impl<'v> BxlContext<'v> {
    pub(crate) fn new(
        heap: &'v Heap,
        core: BxlContextCoreData,
        cli_args: ValueOfUnchecked<'v, StructRef<'v>>,
        async_ctx: BxlSafeDiceComputations<'v, '_>,
        output_sink: RefCell<Box<dyn Write>>,
        error_sink: RefCell<Box<dyn Write>>,
        digest_config: DigestConfig,
    ) -> anyhow::Result<Self> {
        let async_ctx = Rc::new(RefCell::new(async_ctx));
        let root_data = RootBxlContextData {
            cli_args,
            output_stream: heap.alloc_typed(OutputStream::new(
                core.project_fs.clone(),
                core.artifact_fs.clone(),
                output_sink,
                async_ctx.dupe(),
            )),
            error_stream: heap.alloc_typed(OutputStream::new(
                core.project_fs.clone(),
                core.artifact_fs.clone(),
                error_sink,
                async_ctx.dupe(),
            )),
            materializations: Arc::new(DashSet::new()),
        };
        let context_type = BxlContextType::Root(root_data);

        Ok(Self {
            async_ctx: async_ctx.clone(),
            data: BxlContextNoDice {
                state: heap.alloc_typed(AnalysisActions {
                    state: RefCell::new(None),
                    attributes: None,
                    plugins: None,
                    digest_config,
                }),
                context_type,
                core,
            },
        })
    }

    pub(crate) fn new_dynamic(
        heap: &'v Heap,
        core: BxlContextCoreData,
        async_ctx: Rc<RefCell<BxlSafeDiceComputations<'v, '_>>>,
        digest_config: DigestConfig,
        analysis_registry: AnalysisRegistry<'v>,
        dynamic_data: DynamicBxlContextData,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            async_ctx,
            data: BxlContextNoDice {
                state: heap.alloc_typed(AnalysisActions {
                    state: RefCell::new(Some(analysis_registry)),
                    attributes: None,
                    plugins: None,
                    digest_config,
                }),
                context_type: BxlContextType::Dynamic(dynamic_data),
                core,
            },
        })
    }

    /// runs the async computation over dice as sync,
    /// This should generally only be called at the top level functions in bxl.
    /// Within the lambdas, use the existing reference to Dice provided instead of calling nested
    /// via_dice, as that breaks borrow invariants of the dice computations.
    pub(crate) fn via_dice<'a, 's, T>(
        &'a self,
        f: impl for<'x> FnOnce(
            &'x mut dyn BxlDiceComputations,
            &'a BxlContextNoDice<'v>,
        ) -> anyhow::Result<T>,
    ) -> anyhow::Result<T>
    where
        'v: 'a,
    {
        let data = &self.data;
        f(&mut *self.async_ctx.borrow_mut(), data)
    }

    /// Must take an `AnalysisContext` and `OutputStream` which has never had `take_state` called on it before.
    pub(crate) fn take_state(
        value: ValueTyped<'v, BxlContext<'v>>,
    ) -> anyhow::Result<(
        AnalysisRegistry<'v>,
        IndexSet<ArtifactGroup>,
        Arc<DashSet<BuildArtifact>>,
    )> {
        let this = value.as_ref();
        let root_data = this.data.context_type.unpack_root()?;
        let output_stream = &root_data.output_stream;

        let analysis_registry = this
            .data
            .state
            .as_ref()
            .state
            .borrow_mut()
            .take()
            .unwrap_or_else(|| {
                // BXL did not request actions, so we don't know execution platform.
                // It doesn't matter what owner/platform we put here because
                // the registry is empty, nothing will be fetched from it.
                AnalysisRegistry::new_from_owner(
                    this.core
                        .current_bxl
                        .dupe()
                        .into_base_deferred_key(BxlExecutionResolution::unspecified()),
                    ExecutionPlatformResolution::unspecified(),
                )
                .unwrap()
            });

        // artifacts should be bound by now as the bxl has finished running
        let artifacts = output_stream
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
            .collect::<anyhow::Result<IndexSet<ArtifactGroup>>>()?;

        Ok((
            analysis_registry,
            artifacts,
            root_data.materializations.dupe(),
        ))
    }

    /// Must take an `AnalysisContext` which has never had `take_state` called on it before.
    pub(crate) fn take_state_dynamic(&self) -> anyhow::Result<AnalysisRegistry<'v>> {
        let state = self.data.state.as_ref();
        state.state().assert_no_promises()?;

        Ok(state
            .state
            .borrow_mut()
            .take()
            .expect("nothing to have stolen state yet"))
    }
}

impl<'v> BxlContextNoDice<'v> {
    // Used for caching error logs emitted from within the BXL core.
    pub(crate) fn print_to_error_stream(&self, msg: String) -> anyhow::Result<()> {
        match &self.context_type {
            BxlContextType::Root(root) => writeln!(root.error_stream.sink.borrow_mut(), "{}", msg)?,
            BxlContextType::Dynamic(_) => console_message(msg),
        }
        Ok(())
    }

    pub(crate) fn project_root(&self) -> &ProjectRoot {
        &self.core.project_fs
    }

    pub(crate) fn global_cfg_options(&self) -> &GlobalCfgOptions {
        self.core.current_bxl.global_cfg_options()
    }

    pub(crate) fn target_alias_resolver(&self) -> &BuckConfigTargetAliasResolver {
        &self.core.target_alias_resolver
    }

    pub(crate) fn cell_resolver(&self) -> &CellResolver {
        &self.core.cell_resolver
    }

    pub(crate) fn cell_name(&self) -> CellName {
        self.core.cell_name
    }

    pub(crate) fn cell_root_abs(&self) -> &AbsNormPathBuf {
        &self.core.cell_root_abs
    }

    pub(crate) fn cell_alias_resolver(&self) -> &CellAliasResolver {
        &self.core.cell_alias_resolver
    }

    pub(crate) fn current_bxl(&self) -> &BxlKey {
        &self.core.current_bxl
    }

    pub(crate) fn project_fs(&self) -> &ProjectRoot {
        &self.core.project_fs
    }

    pub(crate) fn artifact_fs(&self) -> &ArtifactFs {
        &self.core.artifact_fs
    }

    /// Working dir for resolving literals.
    /// Note, unlike buck2 command line UI, we resolve targets and literals
    /// against the cell root instead of user working dir.
    pub(crate) fn working_dir(&self) -> anyhow::Result<ProjectRelativePathBuf> {
        let cell = self.cell_resolver().get(self.cell_name())?;
        Ok(cell.path().as_project_relative_path().to_owned())
    }

    pub(crate) fn parse_query_file_literal(&self, literal: &str) -> anyhow::Result<CellPath> {
        parse_query_file_literal(
            literal,
            self.cell_alias_resolver(),
            self.cell_resolver(),
            // NOTE(nga): we pass cell root as working directory here,
            //   which is inconsistent with the rest of buck2:
            //   The same query `owner(foo.h)` is resolved using
            //   current directory in `buck2 query`, but relative to cell root in BXL.
            self.cell_root_abs(),
            self.project_root(),
        )
    }
}

pub(crate) async fn eval_bxl_for_dynamic_output<'v>(
    base_deferred_key: &'v BaseDeferredKeyBxl,
    self_key: DeferredHolderKey,
    dynamic_lambda: &'v FrozenDynamicLambdaParams,
    dice_ctx: &'v mut DiceComputations<'_>,
    action_key: String,
    materialized_artifacts: HashMap<Artifact, ProjectRelativePathBuf>,
    project_filesystem: ProjectRoot,
    _digest_config: DigestConfig,
    liveness: CancellationObserver,
) -> anyhow::Result<RecordedAnalysisValues> {
    // TODO(wendyy) emit telemetry, support profiler
    let dynamic_key =
        BxlDynamicKey::from_base_deferred_key_dyn_impl_err(base_deferred_key.clone())?;
    let key = dynamic_key.key();
    let dynamic_data = DynamicBxlContextData {
        exec_deps: dynamic_key
            .0
            .execution_resolution
            .exec_deps_configured
            .clone(),
        toolchains: dynamic_key
            .0
            .execution_resolution
            .toolchain_deps_configured
            .clone(),
    };
    // TODO(cjhopman): Why does this get the digest_config from dice???
    let digest_config = dice_ctx.global_data().get_digest_config();
    let dispatcher = dice_ctx.per_transaction_data().get_dispatcher().dupe();
    let eval_ctx = BxlEvalContext {
        data: BxlContextCoreData::new(key, dice_ctx).await?,
        self_key,
        liveness,
        dynamic_lambda,
        dynamic_data,
        digest_config,
        action_key,
        materialized_artifacts,
        project_filesystem,

        print: EventDispatcherPrintHandler(dispatcher.dupe()),
    };

    // Note: because we use `block_in_place`, that will prevent the inner future from being polled
    // and yielded. So, for cancellation observers to work properly within the dice cancellable
    // future context, we need the future that it's attached to the cancellation context can
    // yield and be polled. To ensure that, we have to spawn the future that then enters block_in_place
    let (_, futs) = unsafe {
        // SAFETY: as long as we don't `forget` the return object from `scope_and_collect`, it is safe

        // Additional cancellation notes:
        // the `scope_and_collect` will block on drop, but it will move the blocking to a tokio
        // blocking thread, freeing up the main worker threads. Additionally, the `spawn_cancellable`
        // on the scope will be dropped at the earliest await point. If we are within the blocking
        // section of bxl, the cancellation observer will be notified and cause the blocking calls
        // to terminate.
        scope_and_collect_with_dice(dice_ctx, |dice_ctx, s| {
            s.spawn_cancellable(
                async move {
                    with_starlark_eval_provider(
                        dice_ctx,
                        &mut StarlarkProfilerOpt::disabled(),
                        format!("bxl_dynamic:{}", "foo"),
                        move |provider, dice_ctx| {
                            tokio::task::block_in_place(|| eval_ctx.do_eval(provider, dice_ctx))
                        },
                    )
                    .await
                },
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

struct BxlEvalContext<'v> {
    data: BxlContextCoreData,
    self_key: DeferredHolderKey,
    liveness: CancellationObserver,
    dynamic_lambda: &'v FrozenDynamicLambdaParams,
    dynamic_data: DynamicBxlContextData,
    digest_config: DigestConfig,
    action_key: String,
    materialized_artifacts: HashMap<Artifact, ProjectRelativePathBuf>,
    project_filesystem: ProjectRoot,
    print: EventDispatcherPrintHandler,
}

impl BxlEvalContext<'_> {
    fn do_eval(
        self,
        provider: &mut dyn StarlarkEvaluatorProvider,
        dice: &mut DiceComputations<'_>,
    ) -> anyhow::Result<RecordedAnalysisValues> {
        let env = Module::new();

        let analysis_registry = {
            let (mut eval, _) = provider.make(&env)?;
            eval.set_print_handler(&self.print);
            eval.set_soft_error_handler(&Buck2StarlarkSoftErrorHandler);
            eval.extra = Some(&BxlEvalExtraTag);

            let dynamic_lambda_ctx_data = dynamic_lambda_ctx_data(
                self.dynamic_lambda,
                self.self_key.dupe(),
                &self.action_key,
                &self.materialized_artifacts,
                &self.project_filesystem,
                self.digest_config,
                &env,
            )?;

            let async_ctx = Rc::new(RefCell::new(BxlSafeDiceComputations::new(
                dice,
                self.liveness,
            )));

            let bxl_dynamic_ctx = BxlContext::new_dynamic(
                env.heap(),
                self.data,
                async_ctx,
                self.digest_config,
                dynamic_lambda_ctx_data.registry,
                self.dynamic_data,
            )?;

            let ctx = ValueTyped::<BxlContext>::new_err(env.heap().alloc(bxl_dynamic_ctx))?;

            let args = match dynamic_lambda_ctx_data.lambda.arg() {
                None => DynamicLambdaArgs::OldPositional {
                    ctx: ctx.to_value(),
                    artifacts: dynamic_lambda_ctx_data.artifacts,
                    outputs: dynamic_lambda_ctx_data.outputs,
                },
                Some(_arg) => {
                    return Err(anyhow::anyhow!(
                        "New `dynamic_actions` API is not implemented for BXL"
                    ));
                }
            };

            invoke_dynamic_output_lambda(&mut eval, dynamic_lambda_ctx_data.lambda.lambda(), args)?;

            ctx.take_state_dynamic()?
        };

        let (_frozen_env, recorded_values) = analysis_registry.finalize(&env)?(env)?;
        Ok(recorded_values)
    }
}

pub(crate) fn init_eval_bxl_for_dynamic_output() {
    EVAL_BXL_FOR_DYNAMIC_OUTPUT.init(
        |base_deferred_key,
         self_key,
         dynamic_lambda,
         dice_ctx,
         action_key,
         materialized_artifacts,
         project_filesystem,
         digest_config,
         liveness| {
            Box::pin(eval_bxl_for_dynamic_output(
                base_deferred_key,
                self_key,
                dynamic_lambda,
                dice_ctx,
                action_key,
                materialized_artifacts,
                project_filesystem,
                digest_config,
                liveness,
            ))
        },
    );
}

#[starlark_value(type = "bxl.Context", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for BxlContext<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(bxl_context_methods)
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
fn bxl_context_methods(builder: &mut MethodsBuilder) {
    /// Gets the output stream to the console via stdout. Items written to the output stream
    /// are considered to be the results of a bxl script, which will be displayed to stdout by
    /// buck2 even when the script is cached.
    ///
    /// Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
    /// and `pprint`.
    ///
    /// This function is not available on the `bxl_ctx` when called from `dynamic_output`.
    #[starlark(attribute)]
    fn output<'v>(this: &'v BxlContext) -> anyhow::Result<ValueTyped<'v, OutputStream<'v>>> {
        let output_stream = this
            .data
            .context_type
            .unpack_root()
            .context(BxlContextDynamicError::Unsupported("output".to_owned()))?
            .output_stream;
        Ok(output_stream)
    }

    /// Returns the absolute path to the root of the repository
    ///
    /// This function is not available on the `bxl_ctx` when called from `dynamic_output`.
    fn root<'v>(this: &'v BxlContext<'v>) -> anyhow::Result<String> {
        let _root_type = this
            .data
            .context_type
            .unpack_root()
            .context(BxlContextDynamicError::Unsupported("root".to_owned()))?;
        Ok(this
            .async_ctx
            .borrow()
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
            .data
            .context_type
            .unpack_root()
            .context(BxlContextDynamicError::Unsupported("root".to_owned()))?;
        Ok(this.cell_root_abs().to_owned().to_string())
    }

    /// Gets the target nodes for the `labels`, accepting an optional `target_platform` which is the
    /// target platform configuration used to resolve configurations of any unconfigured target
    /// nodes.
    /// The `target_platform` is either a string that can be parsed as a target label, or a
    /// target label.
    ///
    /// The given `labels` is a [`TargetListExpr`], which is either:
    ///     - a single string that is a `target pattern`.
    ///     - a single target node or label, configured or unconfigured
    ///     - a list of the two options above.
    ///
    /// Note that this function does not accept `Label` (which is a configured provider label), since this
    /// is the label of a subtarget. You can get the underlying configured target label on the `Label`
    /// using `configured_targets()` (ex: `my_label.configured_target()`).
    ///
    /// This returns either a single `target_node` if the given `labels`
    /// is "singular", a dict keyed by target labels of `target_node` if the
    /// given `labels` is list-like
    fn configured_targets<'v>(
        this: &'v BxlContext<'v>,
        #[starlark(require = pos)] labels: ConfiguredTargetListExprArg<'v>,
        #[starlark(default = ValueAsStarlarkTargetLabel::NONE)]
        target_platform: ValueAsStarlarkTargetLabel<'v>,
    ) -> anyhow::Result<
        Either<NoneOr<StarlarkConfiguredTargetNode>, StarlarkTargetSet<ConfiguredTargetNode>>,
    > {
        let target_platform = target_platform.parse_target_platforms(
            this.target_alias_resolver(),
            this.cell_resolver(),
            this.cell_alias_resolver(),
            this.cell_name(),
            &this.global_cfg_options().target_platform,
        )?;

        this.via_dice(|dice, this| {
            dice.via(|ctx| {
                async move {
                    let target_expr =
                        TargetListExpr::<'v, ConfiguredTargetNode>::unpack_allow_unconfigured(
                            labels,
                            &GlobalCfgOptions {
                                target_platform,
                                cli_modifiers: vec![].into(),
                            },
                            this,
                            ctx,
                        )
                        .await?;

                    if let Some(one) = target_expr.get_one(ctx).await? {
                        let result = filter_incompatible(iter::once(one), this)?;
                        if let Some(node) = result.iter().next() {
                            Ok(Either::Left(NoneOr::Other(StarlarkConfiguredTargetNode(
                                node.dupe(),
                            ))))
                        } else {
                            Ok(Either::Left(NoneOr::None))
                        }
                    } else {
                        Ok(Either::Right(StarlarkTargetSet(filter_incompatible(
                            target_expr.get(ctx).await?,
                            this,
                        )?)))
                    }
                }
                .boxed_local()
            })
        })
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
        labels: TargetListExprArg<'v>,
    ) -> anyhow::Result<Either<StarlarkTargetNode, StarlarkTargetSet<TargetNode>>> {
        this.via_dice(|ctx, this| {
            ctx.via(|ctx| {
                async move {
                    let expr = TargetListExpr::<'v, TargetNode>::unpack(labels, this, ctx).await?;
                    if let Some(one) = expr.get_one(ctx).await? {
                        Ok(Either::Left(StarlarkTargetNode(one)))
                    } else {
                        Ok(Either::Right(StarlarkTargetSet(
                            expr.get(ctx).await?.into_owned(),
                        )))
                    }
                }
                .boxed_local()
            })
        })
    }

    /// Gets the unconfigured subtargets for the given `labels`
    ///
    /// The given `labels` is a providers expression, which is either:
    ///     - a single string that is a `target pattern`.
    ///     - a single target node or label, configured or unconfigured
    ///     - a single subtarget label, configured or unconfigured
    ///     - a list of the two options above.
    ///
    /// This returns either a single `providers_label` if the given `labels` argument
    /// is "singular", or dict of the subtarget string representation to the
    /// `providers_label` if the given `labels` argument is list-like.
    ///
    /// Note that this function does not check that this subtarget exists in the repo.
    fn unconfigured_sub_targets<'v>(
        this: &BxlContext<'v>,
        // TODO(nga): parameter should be either positional or named, not both.
        labels: ProviderExprArg<'v>,
    ) -> anyhow::Result<Either<StarlarkProvidersLabel, SmallMap<String, StarlarkProvidersLabel>>>
    {
        let providers =
            this.via_dice(|_dice, this| ProvidersExpr::<ProvidersLabel>::unpack(labels, this))?;

        match providers {
            ProvidersExpr::Literal(provider) => {
                Ok(Either::Left(StarlarkProvidersLabel::new(provider)))
            }
            ProvidersExpr::Iterable(providers) => Ok(Either::Right(
                providers
                    .into_iter()
                    .map(|p| (p.to_string(), StarlarkProvidersLabel::new(p)))
                    .collect(),
            )),
        }
    }

    /// Returns the `target_universe` that can lookup valid configured nodes in the universe.
    ///
    /// The given `labels` is a target expression, which is either:
    ///     - a single string that is a `target pattern`.
    ///     - a single target node or label, configured or unconfigured
    ///     - a single subtarget label, configured or unconfigured
    ///     - a list of the two options above.
    ///
    /// Also takes in an optional `target_platform` param to configure the nodes with, and a `keep_going``
    /// flag to skip any loading or configuration errors. Note that `keep_going` currently can only be used
    /// if the input labels is a single target pattern as a string literal.
    fn target_universe<'v>(
        this: &'v BxlContext<'v>,
        labels: ConfiguredTargetListExprArg<'v>,
        #[starlark(default = ValueAsStarlarkTargetLabel::NONE)]
        target_platform: ValueAsStarlarkTargetLabel<'v>,
        #[starlark(require = named, default = false)] keep_going: bool,
    ) -> anyhow::Result<StarlarkTargetUniverse<'v>> {
        let target_platform = target_platform.parse_target_platforms(
            this.target_alias_resolver(),
            this.cell_resolver(),
            this.cell_alias_resolver(),
            this.cell_name(),
            &this.global_cfg_options().target_platform,
        )?;

        this.via_dice(|ctx, this_no_dice: &BxlContextNoDice<'_>| {
            ctx.via(|ctx| {
                async move {
                    let target_expr = if keep_going {
                        TargetListExpr::<'v, ConfiguredTargetNode>::unpack_keep_going(
                            labels,
                            &GlobalCfgOptions {
                                target_platform,
                                cli_modifiers: vec![].into(),
                            },
                            this_no_dice,
                            ctx,
                        )
                        .await?
                    } else {
                        TargetListExpr::<'v, ConfiguredTargetNode>::unpack_allow_unconfigured(
                            labels,
                            &GlobalCfgOptions {
                                target_platform,
                                cli_modifiers: vec![].into(),
                            },
                            this_no_dice,
                            ctx,
                        )
                        .await?
                    };

                    let maybe_compatible_set = target_expr.get(ctx).await?;

                    let target_set = filter_incompatible(maybe_compatible_set, this_no_dice)?;

                    StarlarkTargetUniverse::new(this, target_set).await
                }
                .boxed_local()
            })
        })
    }

    /// Returns the `uqueryctx` that holds all uquery functions.
    fn uquery<'v>(this: &'v BxlContext<'v>) -> anyhow::Result<StarlarkUQueryCtx<'v>> {
        StarlarkUQueryCtx::new(this)
    }

    /// Returns the `cqueryctx` that holds all the cquery functions.
    /// This function takes an optional parameter `target_platform`, which is the target platform
    /// configuration used to configured any unconfigured target nodes.
    ///
    /// The `target_platform` is a target label, or a string that is a target label.
    fn cquery<'v>(
        this: &'v BxlContext<'v>,
        // TODO(nga): parameter should be either positional or named, not both.
        #[starlark(default = ValueAsStarlarkTargetLabel::NONE)]
        target_platform: ValueAsStarlarkTargetLabel<'v>,
    ) -> anyhow::Result<StarlarkCQueryCtx<'v>> {
        StarlarkCQueryCtx::new(this, target_platform, this.data.global_cfg_options())
    }

    /// Returns the `aqueryctx` that holds all the aquery functions.
    /// This function takes an optional parameter `target_platform`, which is the target platform
    /// configuration used to configured any unconfigured target nodes.
    ///
    /// The `target_platform` is a target label, or a string that is a target label.
    fn aquery<'v>(
        this: &'v BxlContext<'v>,
        #[starlark(default = ValueAsStarlarkTargetLabel::NONE)]
        target_platform: ValueAsStarlarkTargetLabel<'v>,
    ) -> anyhow::Result<StarlarkAQueryCtx<'v>> {
        StarlarkAQueryCtx::new(
            this,
            target_platform,
            &this.data.global_cfg_options().target_platform,
        )
    }

    /// Returns the bxl actions to create and register actions for this
    /// bxl function. This will have the execution platform resolved according to the execution
    /// deps and toolchains you pass into this function.
    /// You'll be able to access the analysis action factory of the correct execution platform,
    /// toolchains, and execution deps of the corresponding configuration via this context.
    ///
    /// Actions created by bxl will not be built by default. Instead, they are marked to be built
    /// by `ctx.output.ensure(artifact)` on the output module of the `bxl_ctx`. Only artifacts
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
    /// Note that the keys of `exec_deps` and `toolchains` must be unconfigured subtarget labels (`providers_label`s),
    /// and not unconfigured target labels. You can use `ctx.unconfigured_sub_targets(...)` or `with_sub_target()` on
    /// `target_label` to create the label.
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
        #[starlark(require = named, default = NoneOr::None)] exec_deps: NoneOr<ProviderExprArg<'v>>,
        #[starlark(require = named, default = NoneOr::None)] toolchains: NoneOr<
            ProviderExprArg<'v>,
        >,
        #[starlark(require = named, default = ValueAsStarlarkTargetLabel::NONE)]
        target_platform: ValueAsStarlarkTargetLabel<'v>,
        #[starlark(require = named, default = NoneOr::None)] exec_compatible_with: NoneOr<
            TargetListExprArg<'v>,
        >,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> anyhow::Result<BxlActions<'v>> {
        this.via_dice(|ctx, this| {
            ctx.via(|ctx| {
                async {
                    let (exec_deps, toolchains) = match &this.context_type {
                        BxlContextType::Root { .. } => {
                            let target_platform = target_platform.parse_target_platforms(
                                this.target_alias_resolver(),
                                this.cell_resolver(),
                                this.cell_alias_resolver(),
                                this.cell_name(),
                                &this.global_cfg_options().target_platform,
                            )?;
                            let exec_deps = match exec_deps {
                                NoneOr::None => Vec::new(),
                                NoneOr::Other(exec_deps) => {
                                    ProvidersExpr::<ProvidersLabel>::unpack(exec_deps, this)?
                                        .labels()
                                        .cloned()
                                        .collect()
                                }
                            };

                            let toolchains = match toolchains {
                                NoneOr::None => Vec::new(),
                                NoneOr::Other(toolchains) => {
                                    ProvidersExpr::<ProvidersLabel>::unpack(toolchains, this)?
                                        .labels()
                                        .cloned()
                                        .collect()
                                }
                            };

                            let exec_compatible_with: Arc<[_]> = match exec_compatible_with {
                                NoneOr::None => Arc::new([]),
                                NoneOr::Other(exec_compatible_with) => {
                                    TargetListExpr::<TargetNode>::unpack(
                                        exec_compatible_with,
                                        this,
                                        ctx,
                                    )
                                    .await?
                                    .get(ctx)
                                    .await?
                                    .iter()
                                    .map(|n| ConfigurationSettingKey(n.label().dupe()))
                                    .collect()
                                }
                            };

                            let execution_resolution = resolve_bxl_execution_platform(
                                ctx,
                                this.cell_name(),
                                exec_deps,
                                toolchains,
                                target_platform.clone(),
                                exec_compatible_with.clone(),
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
                                    BxlContextDynamicError::RequireSameExecutionPlatformAsRoot
                                        .into(),
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
                }
                .boxed_local()
            })
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
    /// This returns either a single `analysis_result` if the given `labels` argument is "singular",
    /// or a dict keyed by sub target labels of `analysis` if the given `labels` argument
    /// is list-like
    fn analysis<'v>(
        this: &BxlContext<'v>,
        // TODO(nga): these parameters should be either position or named, not both.
        labels: ConfiguredProvidersExprArg<'v>,
        #[starlark(default = ValueAsStarlarkTargetLabel::NONE)]
        target_platform: ValueAsStarlarkTargetLabel<'v>,
        #[starlark(require = named, default = true)] skip_incompatible: bool,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> anyhow::Result<
        Either<
            NoneOr<StarlarkAnalysisResult>,
            SmallMap<
                ValueTyped<'v, StarlarkConfiguredProvidersLabel>,
                ValueTyped<'v, StarlarkAnalysisResult>,
            >,
        >,
    > {
        let target_platform = target_platform.parse_target_platforms(
            this.data.target_alias_resolver(),
            this.data.cell_resolver(),
            this.cell_alias_resolver(),
            this.data.cell_name(),
            &this.data.global_cfg_options().target_platform,
        )?;

        let res: anyhow::Result<_> = this.via_dice(|dice, ctx| {
            dice.via(|dice| {
                async {
                    let providers = ProvidersExpr::<ConfiguredProvidersLabel>::unpack(
                        labels,
                        &GlobalCfgOptions {
                            target_platform,
                            cli_modifiers: vec![].into(),
                        },
                        ctx,
                        dice,
                    )
                    .await?;
                    analysis::analysis(dice, ctx, providers, skip_incompatible).await
                }
                .boxed_local()
            })
        });

        Ok(match res? {
            Either::Left(single) => {
                let single = match single {
                    Some(single) => NoneOr::Other(single),
                    None => NoneOr::None,
                };
                Either::Left(single)
            }
            Either::Right(many) => Either::Right(
                many.into_iter()
                    .map(|(t, v)| {
                        Ok((
                            eval.heap()
                                .alloc_typed(StarlarkConfiguredProvidersLabel::new(t))
                                .hashed()
                                .unwrap(),
                            eval.heap().alloc_typed(v),
                        ))
                    })
                    .collect::<anyhow::Result<_>>()?,
            ),
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
    /// This returns a dict keyed by sub target labels mapped to `bxl_build_result`s if the
    /// given `labels` argument is list-like.
    ///
    /// This function is not available on the `bxl_ctx` when called from `dynamic_output`.
    fn build<'v>(
        this: &'v BxlContext<'v>,
        // TODO(nga): parameter should be either positional or named, not both.
        labels: ConfiguredProvidersExprArg<'v>,
        #[starlark(default = ValueAsStarlarkTargetLabel::NONE)]
        target_platform: ValueAsStarlarkTargetLabel<'v>,
        #[starlark(require = named, default = "default")] materializations: &str,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> anyhow::Result<
        SmallMap<
            ValueTyped<'v, StarlarkConfiguredProvidersLabel>,
            ValueTyped<'v, StarlarkBxlBuildResult>,
        >,
    > {
        let materialization_setting = materializations;
        let materializations = &this
            .data
            .context_type
            .unpack_root()
            .context(BxlContextDynamicError::Unsupported("build".to_owned()))?
            .materializations;
        build::build(
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
        )
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
    fn cli_args<'v>(this: &BxlContext<'v>) -> anyhow::Result<ValueOfUnchecked<'v, StructRef<'v>>> {
        let cli_args = this
            .data
            .context_type
            .unpack_root()
            .context(BxlContextDynamicError::Unsupported("cli_args".to_owned()))?
            .cli_args;

        Ok(cli_args)
    }

    /// Returns the `bxl.Filesystem` for performing a basic set of filesystem operations within bxl
    #[starlark(attribute)]
    fn fs<'v>(this: &BxlContext<'v>) -> anyhow::Result<BxlFilesystem<'v>> {
        Ok(BxlFilesystem::new(this))
    }

    /// Checks if a target label exists. Target label must be a string literal, and an exact target.
    fn target_exists<'v>(this: &'v BxlContext<'v>, label: &'v str) -> anyhow::Result<bool> {
        this.via_dice(|ctx, this_no_dice: &BxlContextNoDice<'_>| {
            ctx.via(|ctx| {
                async move {
                    match ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                        this_no_dice.target_alias_resolver(),
                        CellPathRef::new(this_no_dice.cell_name(), CellRelativePath::empty()),
                        label,
                        this_no_dice.cell_resolver(),
                        this_no_dice.cell_alias_resolver(),
                    )? {
                        ParsedPattern::Target(pkg, name, TargetPatternExtra) => {
                            let target_label = TargetLabel::new(pkg, name.as_ref());
                            Ok(ctx.get_target_node(&target_label).await.ok().is_some())
                        }
                        _ => Err(anyhow::anyhow!(NotATargetLabelString)),
                    }
                }
                .boxed_local()
            })
        })
    }

    /// Returns the `audit_ctx` that holds all the audit functions.
    fn audit<'v>(this: &'v BxlContext<'v>) -> anyhow::Result<StarlarkAuditCtx<'v>> {
        let (working_dir, cell_resolver) = this.via_dice(|ctx, this| {
            ctx.via(|ctx| {
                async move {
                    Ok((
                        this.cell_resolver()
                            .get(this.cell_name())?
                            .path()
                            .as_project_relative_path()
                            .to_buf(),
                        ctx.get_cell_resolver().await?,
                    ))
                }
                .boxed_local()
            })
        })?;

        StarlarkAuditCtx::new(this, working_dir, cell_resolver)
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
    ///     promise = actions.anon_target(my_anon_targets_rule, attrs).promise.map(my_map_function)
    ///     providers_result = ctx.resolve(actions, promise) # result is `provider_collection` type, which is a collection of `provider`s
    ///     ctx.output.print(providers_result[0].my_field)
    /// ```
    fn resolve<'v>(
        this: &'v BxlContext<'v>,
        action_factory: ValueTyped<'v, AnalysisActions<'v>>,
        promise: ValueTyped<'v, StarlarkPromise<'v>>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> anyhow::Result<Option<Value<'v>>> {
        this.via_dice(|dice, this| {
            dice.via(|dice| {
                action_factory
                    .run_promises(dice, eval, format!("bxl$promises:{}", this.current_bxl()))
                    .boxed_local()
            })
        })?;
        Ok(promise.get())
    }

    /// Emits a user-defined instant event, taking in a required string id and a metadata dictionary where the
    /// keys are strings, and values are either strings, bools, or ints. The id is user-supplied, and used to
    /// identify the instant events in the event logs more easily.
    ///
    /// You may pass in an ensured artifact as a value in the metadata. The resulting output would be the ensured
    /// artifact's relative or absolute path as a string.
    fn instant_event<'v>(
        this: &'v BxlContext<'v>,
        #[starlark(require = named)] id: &str,
        #[starlark(require = named)] metadata: Value<'v>,
    ) -> anyhow::Result<NoneType> {
        let parser = StarlarkUserEventParser {
            artifact_fs: this.artifact_fs(),
            project_fs: this.project_fs(),
        };
        let event = parser.parse(id, metadata)?;

        this.async_ctx
            .borrow()
            .per_transaction_data()
            .get_dispatcher()
            .instant_event(event);

        Ok(NoneType)
    }
}
