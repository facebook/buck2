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
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::bxl::result::BxlResult;
use buck2_build_api::bxl::types::BxlFunctionLabel;
use buck2_build_api::deferred::types::DeferredTable;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::events::HasEvents;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_core::cells::CellResolver;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::fs::buck_out_path::BuckOutPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_data::BxlExecutionEnd;
use buck2_data::BxlExecutionStart;
use buck2_events::dispatch::console_message;
use buck2_events::dispatch::with_dispatcher;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::factory::StarlarkEvaluatorProvider;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::starlark_profiler::StarlarkProfileDataAndStats;
use buck2_interpreter::starlark_profiler::StarlarkProfileModeOrInstrumentation;
use buck2_interpreter::starlark_profiler::StarlarkProfiler;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_util::collections::ordered_map::OrderedMap;
use clap::ErrorKind;
use dashmap::DashMap;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use itertools::Itertools;
use more_futures::cancellable_future::CancellationObserver;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::structs::AllocStruct;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::Value;
use starlark::values::ValueTyped;
use thiserror::Error;

use crate::bxl::key::BxlKey;
use crate::bxl::starlark_defs::bxl_function::FrozenBxlFunction;
use crate::bxl::starlark_defs::cli_args::CliArgValue;
use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::context::BxlContext;

pub(crate) async fn eval(
    ctx: &DiceComputations,
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
        async_scoped::TokioScope::scope_and_collect(|s| {
            s.spawn_cancellable(
                with_dispatcher_async(dispatcher.dupe(), async move {
                    let bxl_module = ctx
                        .get_loaded_module(StarlarkModulePath::BxlFile(&key.label().bxl_path))
                        .await?;

                    let cell_resolver = ctx.get_cell_resolver().await?;

                    let bxl_cell = cell_resolver
                        .get(key.label().bxl_path.cell())
                        .with_context(|| {
                            format!("Cell does not exist: `{}`", key.label().bxl_path.cell())
                        })?
                        .dupe();

                    let target_alias_resolver = ctx
                        .target_alias_resolver_for_cell(key.label().bxl_path.cell())
                        .await?;

                    let project_fs = ctx.global_data().get_io_provider().project_root().dupe();
                    let artifact_fs = ctx.get_artifact_fs().await?;

                    let digest_config = ctx.global_data().get_digest_config();

                    // The bxl function may trigger async operations like builds, analysis, parsing etc, but those
                    // will be blocking calls so that starlark can remain synchronous.
                    // So indicate to tokio that this may block in place to avoid starvation. Ideally we use
                    // spawn_blocking but that requires a static lifetime. There is no `join`s of multiple
                    // futures that requires work to be done on the current thread, so using block_in_place
                    // should have no noticeable different compared to spawn_blocking

                    let output_stream = mk_stream_cache("output", &key);
                    let file_path = artifact_fs
                        .buck_out_path_resolver()
                        .resolve_gen(&output_stream);

                    let file = RefCell::new(Box::new(project_fs.create_file(&file_path, false)?));

                    let error_stream = mk_stream_cache("error", &key);
                    let error_file_path = artifact_fs
                        .buck_out_path_resolver()
                        .resolve_gen(&error_stream);

                    let error_file =
                        RefCell::new(Box::new(project_fs.create_file(&error_file_path, false)?));

                    let print = EventDispatcherPrintHandler(dispatcher.clone());

                    let mut profiler_opt = profile_mode_or_instrumentation
                        .profile_mode()
                        .map(|profile_mode| StarlarkProfiler::new(profile_mode.dupe(), true));

                    let mut profiler = match &mut profiler_opt {
                        None => StarlarkProfilerOrInstrumentation::disabled(),
                        Some(profiler) => StarlarkProfilerOrInstrumentation::for_profiler(profiler),
                    };

                    let global_target_platform = key.global_target_platform().clone();

                    let (bxl_result, materializations) = with_starlark_eval_provider(
                        ctx,
                        &mut profiler,
                        format!("bxl:{}", key),
                        move |provider, ctx| {
                            let env = Module::new();

                            let resolved_args = env.heap().alloc(AllocStruct(
                                key.cli_args()
                                    .iter()
                                    .map(|(k, v)| (k, v.as_starlark(env.heap()))),
                            ));

                            let mut eval = provider.make(&env)?;
                            let bxl_function_name = key.label().name.clone();
                            let frozen_callable = get_bxl_callable(key.label(), &bxl_module)?;
                            eval.set_print_handler(&print);

                            let bxl_ctx = BxlContext::new(
                                eval.heap(),
                                key,
                                resolved_args,
                                target_alias_resolver,
                                project_fs,
                                artifact_fs,
                                cell_resolver,
                                bxl_cell.name(),
                                BxlSafeDiceComputations::new(ctx, liveness),
                                file,
                                error_file,
                                digest_config,
                                global_target_platform,
                            )?;

                            let bxl_ctx =
                                ValueTyped::<BxlContext>::new(env.heap().alloc(bxl_ctx)).unwrap();

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
                                                ),
                                                BxlExecutionEnd {},
                                            )
                                        },
                                    )
                                })
                            })?;
                            if !result.is_none() {
                                return Err(anyhow::anyhow!(NotAValidReturnType(
                                    result.get_type()
                                )));
                            }

                            let (actions, ensured_artifacts, materializations) =
                                BxlContext::take_state(bxl_ctx)?;
                            std::mem::drop(eval);

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

                                    let deferred_table =
                                        DeferredTable::new(deferred.take_result()?);

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
                        },
                    )
                    .await?;

                    let profile_data = profiler_opt.map(|p| p.finish()).transpose()?;
                    Ok((bxl_result, profile_data, materializations))
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
    eval: &mut Evaluator<'a, '_>,
    frozen_callable: OwnedFrozenValueTyped<FrozenBxlFunction>,
    ctx: Value<'a>,
    provider: &mut dyn StarlarkEvaluatorProvider,
) -> anyhow::Result<Value<'a>> {
    let bxl_impl = frozen_callable.implementation();
    let result = eval.eval_function(bxl_impl.to_value(), &[ctx], &[]);

    provider
        .evaluation_complete(eval)
        .context("Profiler finalization failed")?;
    result
}

#[derive(Debug, Error)]
#[error("Expected {0} to be a bxl function, was a {1}")]
struct NotABxlFunction(String, &'static str);

pub(crate) fn get_bxl_callable<'a>(
    spec: &BxlFunctionLabel,
    bxl_module: &'a LoadedModule,
) -> anyhow::Result<OwnedFrozenValueTyped<FrozenBxlFunction>> {
    let callable = bxl_module.env().get_any_visibility(&spec.name)?.0;

    callable
        .downcast::<FrozenBxlFunction>()
        .map_err(|e| NotABxlFunction(spec.name.clone(), e.value().get_type()).into())
}

pub(crate) struct CliResolutionCtx<'a> {
    pub(crate) target_alias_resolver: BuckConfigTargetAliasResolver,
    pub(crate) cell_resolver: CellResolver,
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

#[derive(Debug, Error)]
#[error("Expected `NoneType` to be returned from bxl. Got return value `{0}`")]
struct NotAValidReturnType(&'static str);
