/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::sync::Arc;

use anyhow::Context;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::bxl::result::BxlResult;
use buck2_build_api::bxl::types::BxlFunctionLabel;
use buck2_build_api::deferred::types::DeferredTable;
use buck2_common::events::HasEvents;
use buck2_common::scope::scope_and_collect_with_dice;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::fs::buck_out_path::BuckOutPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_data::BxlExecutionEnd;
use buck2_data::BxlExecutionStart;
use buck2_data::StarlarkFailNoStacktrace;
use buck2_events::dispatch::console_message;
use buck2_events::dispatch::get_dispatcher;
use buck2_events::dispatch::with_dispatcher;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_futures::cancellable_future::CancellationObserver;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::error::BuckStarlarkError;
use buck2_interpreter::factory::StarlarkEvaluatorProvider;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::starlark_profiler::StarlarkProfileDataAndStats;
use buck2_interpreter::starlark_profiler::StarlarkProfileModeOrInstrumentation;
use buck2_interpreter::starlark_profiler::StarlarkProfiler;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use clap::ErrorKind;
use dashmap::DashMap;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use itertools::Itertools;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::structs::AllocStruct;
use starlark::values::structs::StructRef;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark_map::ordered_map::OrderedMap;

use crate::bxl::key::BxlKey;
use crate::bxl::starlark_defs::bxl_function::FrozenBxlFunction;
use crate::bxl::starlark_defs::cli_args::CliArgValue;
use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::context::BxlContextCoreData;
use crate::bxl::starlark_defs::functions::BxlErrorWithoutStacktrace;
use crate::bxl::starlark_defs::tag::BxlEvalExtraTag;

pub(crate) async fn eval(
    ctx: &mut DiceComputations<'_>,
    key: BxlKey,
    profile_mode_or_instrumentation: StarlarkProfileModeOrInstrumentation,
    liveness: CancellationObserver,
) -> anyhow::Result<(
    BxlResult,
    Option<StarlarkProfileDataAndStats>,
    Arc<DashMap<BuildArtifact, ()>>,
)> {
    // Note: because we use `block_in_place`, that will prevent the inner future from being polled
    // and yielded. So, for cancellation observers to work properly within the dice cancellable
    // future context, we need the future that it's attached to the cancellation context can
    // yield and be polled. To ensure that, we have to spawn the future that then enters block_in_place

    let dispatcher = ctx.per_transaction_data().get_dispatcher().dupe();

    let (_, futs) = unsafe {
        // SAFETY: as long as we don't `forget` the return object from `scope_and_collect`, it is safe

        // Additional cancellation notes:
        // the `scope_and_collect` will block on drop, but it will move the blocking to a tokio
        // blocking thread, freeing up the main worker threads. Additionally, the `spawn_cancellable`
        // on the scope will be dropped at the earliest await point. If we are within the blocking
        // section of bxl, the cancellation observer will be notified and cause the blocking calls
        // to terminate.
        scope_and_collect_with_dice(ctx, |ctx, s| {
            s.spawn_cancellable(
                eval_bxl_inner(
                    ctx,
                    dispatcher,
                    key,
                    profile_mode_or_instrumentation,
                    liveness,
                ),
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

struct BxlInnerEvaluator {
    data: BxlContextCoreData,
    module: LoadedModule,
    liveness: CancellationObserver,
    digest_config: DigestConfig,
    dispatcher: EventDispatcher,
}

impl BxlInnerEvaluator {
    fn do_eval<'a>(
        self,
        provider: &mut dyn StarlarkEvaluatorProvider,
        dice: &'a mut DiceComputations,
    ) -> anyhow::Result<(BxlResult, Arc<DashMap<BuildArtifact, ()>>)> {
        let BxlInnerEvaluator {
            data,
            module,
            liveness,
            digest_config,
            dispatcher,
        } = self;
        let bxl_dice = BxlSafeDiceComputations::new(dice, liveness);

        let env = Module::new();
        let key = data.key();

        let output_stream = mk_stream_cache("output", key);
        let file_path = data
            .artifact_fs()
            .buck_out_path_resolver()
            .resolve_gen(&output_stream);

        let file = RefCell::new(Box::new(
            data.project_fs()
                .create_file(&file_path, false)
                .context("Failed to create output cache for BXL")?,
        ));

        let error_stream = mk_stream_cache("error", key);
        let error_file_path = data
            .artifact_fs()
            .buck_out_path_resolver()
            .resolve_gen(&error_stream);

        let error_file = RefCell::new(Box::new(
            data.project_fs()
                .create_file(&error_file_path, false)
                .context("Failed to create error cache for BXL")?,
        ));

        let (actions, ensured_artifacts, materializations) = {
            let resolved_args = ValueOfUnchecked::<StructRef>::unpack_value_err(
                env.heap().alloc(AllocStruct(
                    key.cli_args()
                        .iter()
                        .map(|(k, v)| (k, v.as_starlark(env.heap()))),
                )),
            )?;

            let print = EventDispatcherPrintHandler(dispatcher.clone());

            let (mut eval, _) = provider.make(&env)?;
            let bxl_function_name = key.label().name.clone();
            let frozen_callable = get_bxl_callable(key.label(), &module)?;
            eval.set_print_handler(&print);
            eval.extra = Some(&BxlEvalExtraTag);

            let force_print_stacktrace = key.force_print_stacktrace();
            let bxl_ctx = BxlContext::new(
                eval.heap(),
                data,
                resolved_args,
                bxl_dice,
                file,
                error_file,
                digest_config,
            )?;

            let bxl_ctx = ValueTyped::<BxlContext>::new_err(env.heap().alloc(bxl_ctx))?;

            let result = tokio::task::block_in_place(|| {
                with_dispatcher(dispatcher.clone(), || {
                    dispatcher.clone().span(
                        BxlExecutionStart {
                            name: bxl_function_name,
                        },
                        || {
                            (
                                eval_bxl(
                                    &mut eval,
                                    frozen_callable,
                                    bxl_ctx.to_value(),
                                    provider,
                                    force_print_stacktrace,
                                ),
                                BxlExecutionEnd {},
                            )
                        },
                    )
                })
            })?;
            if !result.is_none() {
                return Err(anyhow::anyhow!(NotAValidReturnType(result.get_type())));
            }

            BxlContext::take_state(bxl_ctx)?
        };

        let (actions_finalizer, ensured_artifacts, materializations) = {
            // help rust understand that actions is consumed here.
            let actions = actions;
            match actions {
                Some(registry) => (
                    Some(registry.finalize(&env)?),
                    ensured_artifacts,
                    materializations,
                ),
                None => (None, ensured_artifacts, materializations),
            }
        };

        let (frozen_module, bxl_result) = match actions_finalizer {
            Some(actions_finalizer) => {
                // this bxl registered actions, so extract the deferreds from it
                let (frozen_module, deferred) = actions_finalizer(env)?;

                let deferred_table = DeferredTable::new(deferred.take_result()?);

                (
                    frozen_module,
                    BxlResult::new(
                        output_stream,
                        error_stream,
                        ensured_artifacts,
                        deferred_table,
                    ),
                )
            }
            None => {
                let frozen_module = env.freeze()?;

                // this bxl did not try to build anything, so we don't have any deferreds
                (
                    frozen_module,
                    BxlResult::new(
                        output_stream,
                        error_stream,
                        ensured_artifacts,
                        DeferredTable::new(Vec::new()),
                    ),
                )
            }
        };

        provider
            .visit_frozen_module(Some(&frozen_module))
            .context("Profiler heap visitation failed")?;

        Ok((bxl_result, materializations))
    }
}

async fn eval_bxl_inner(
    ctx: &mut DiceComputations<'_>,
    dispatcher: EventDispatcher,
    key: BxlKey,
    profile_mode_or_instrumentation: StarlarkProfileModeOrInstrumentation,
    liveness: CancellationObserver,
) -> anyhow::Result<(
    BxlResult,
    Option<StarlarkProfileDataAndStats>,
    Arc<DashMap<BuildArtifact, ()>>,
)> {
    let bxl_module = ctx
        .get_loaded_module(StarlarkModulePath::BxlFile(&key.label().bxl_path))
        .await?;

    let digest_config = ctx.global_data().get_digest_config();
    let core_data = BxlContextCoreData::new(key.dupe(), ctx).await?;

    // The bxl function may trigger async operations like builds, analysis, parsing etc, but those
    // will be blocking calls so that starlark can remain synchronous.
    // So indicate to tokio that this may block in place to avoid starvation. Ideally we use
    // spawn_blocking but that requires a static lifetime. There is no `join`s of multiple
    // futures that requires work to be done on the current thread, so using block_in_place
    // should have no noticeable different compared to spawn_blocking

    let mut profiler_opt = profile_mode_or_instrumentation
        .profile_mode()
        .map(|profile_mode| StarlarkProfiler::new(profile_mode.dupe(), true));

    let mut profiler = match &mut profiler_opt {
        None => StarlarkProfilerOrInstrumentation::disabled(),
        Some(profiler) => StarlarkProfilerOrInstrumentation::for_profiler(profiler),
    };

    let starlark_eval_description = format!("bxl:{}", core_data.key());

    let eval_ctx = BxlInnerEvaluator {
        data: core_data,
        module: bxl_module,
        liveness,
        digest_config,
        dispatcher,
    };

    let (bxl_result, materializations) = with_starlark_eval_provider(
        ctx,
        &mut profiler,
        starlark_eval_description,
        move |provider, ctx| eval_ctx.do_eval(provider, ctx),
    )
    .await?;

    let profile_data = profiler_opt.map(|p| p.finish()).transpose()?;
    Ok((bxl_result, profile_data, materializations))
}

// We use a file as our output/error stream cache. The file is associated with the `BxlDynamicKey` (created from `BxlKey`),
// which is super important, as it HAS to be the SAME as the DiceKey so that DICE is keeping the output file
// cache up to date. `BxlDynamicKey` requires an execution platform. We set the execution platform to be unspecified here
// because BXL functions do not have execution platform resolutions. exec_deps, toolchains, target_platform, and exec_compatible_with
// are empty here for the same reason.
pub(crate) fn mk_stream_cache(stream_type: &str, key: &BxlKey) -> BuckOutPath {
    BuckOutPath::new(
        BaseDeferredKey::BxlLabel(key.dupe().into_base_deferred_key_dyn_impl(
            ExecutionPlatformResolution::unspecified(),
            Vec::new(),
            Vec::new(),
        )),
        ForwardRelativePathBuf::unchecked_new(format!(
            "__bxl_internal__/{}stream_cache",
            stream_type
        )),
    )
}

fn eval_bxl<'a>(
    eval: &mut Evaluator<'a, '_, '_>,
    frozen_callable: OwnedFrozenValueTyped<FrozenBxlFunction>,
    ctx: Value<'a>,
    provider: &mut dyn StarlarkEvaluatorProvider,
    force_print_stacktrace: bool,
) -> anyhow::Result<Value<'a>> {
    let bxl_impl = frozen_callable.implementation();
    let result = eval.eval_function(bxl_impl.to_value(), &[ctx], &[]);

    provider
        .evaluation_complete(eval)
        .context("Profiler finalization failed")?;

    let e = match result {
        Ok(v) => return Ok(v),
        Err(e) => e,
    };

    let should_skip_backtrace = !force_print_stacktrace
        && match e.kind() {
            starlark::ErrorKind::Other(e) => {
                e.downcast_ref::<BxlErrorWithoutStacktrace>().is_some()
            }
            _ => false,
        };

    let mut e = BuckStarlarkError::new(e);
    if should_skip_backtrace {
        let dispatcher = get_dispatcher();
        dispatcher.instant_event(StarlarkFailNoStacktrace {
            trace: format!("{}", e),
        });
        dispatcher
            .console_message("Re-run the script with `-v5` to show the full stacktrace".to_owned());
        e.set_print_stacktrace(false);
    }

    Err(e.into())
}

#[derive(Debug, buck2_error::Error)]
#[error("Expected {0} to be a bxl function, was a {1}")]
struct NotABxlFunction(String, &'static str);

pub(crate) fn get_bxl_callable<'a>(
    spec: &BxlFunctionLabel,
    bxl_module: &'a LoadedModule,
) -> anyhow::Result<OwnedFrozenValueTyped<FrozenBxlFunction>> {
    let callable = bxl_module.env().get_any_visibility(&spec.name)?.0;

    callable.downcast_anyhow::<FrozenBxlFunction>()
}

pub(crate) struct CliResolutionCtx<'a> {
    pub(crate) target_alias_resolver: BuckConfigTargetAliasResolver,
    pub(crate) cell_resolver: CellResolver,
    pub(crate) cell_alias_resolver: CellAliasResolver,
    pub(crate) relative_dir: PackageLabel,
    pub(crate) dice: &'a DiceTransaction,
}

pub(crate) enum BxlResolvedCliArgs {
    Resolved(OrderedMap<String, CliArgValue>),
    Help,
}

pub(crate) async fn resolve_cli_args<'a>(
    spec: &BxlFunctionLabel,
    cli_ctx: &CliResolutionCtx<'a>,
    bxl_args: &Vec<String>,
    frozen_callable: &'a FrozenBxlFunction,
) -> anyhow::Result<BxlResolvedCliArgs> {
    match frozen_callable
        .to_clap(clap::Command::new(&spec.name).no_binary_name(true))
        .try_get_matches_from(bxl_args)
    {
        Ok(args) => Ok(BxlResolvedCliArgs::Resolved(
            frozen_callable.parse_clap(args, cli_ctx).await?,
        )),
        Err(e) => match e.kind() {
            ErrorKind::DisplayHelp => {
                let mut help_out = Vec::new();

                frozen_callable
                    .to_clap(clap::Command::new(&spec.name).no_binary_name(true))
                    .write_long_help(&mut help_out)
                    .unwrap();
                let help_msg = String::from_utf8(help_out)?;

                console_message(help_msg);

                Ok(BxlResolvedCliArgs::Help)
            }
            _ => Err(e.into()),
        },
    }
}

#[derive(Debug, buck2_error::Error)]
#[error("Expected `NoneType` to be returned from bxl. Got return value `{0}`")]
struct NotAValidReturnType(&'static str);
