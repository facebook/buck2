/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::future::Future;
use std::rc::Rc;
use std::sync::Arc;

use buck2_build_api::bxl::result::BxlResult;
use buck2_build_api::bxl::types::BxlFunctionLabel;
use buck2_common::events::HasEvents;
use buck2_common::scope::scope_and_collect_with_dice;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::fs::buck_out_path::BuildArtifactPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_data::BxlExecutionEnd;
use buck2_data::BxlExecutionStart;
use buck2_data::StarlarkFailNoStacktrace;
use buck2_error::buck2_error;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::starlark_error::from_starlark_with_options;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::console_message;
use buck2_events::dispatch::get_dispatcher;
use buck2_events::dispatch::with_dispatcher;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_futures::cancellation::CancellationObserver;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::factory::StarlarkEvaluatorProvider;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::from_freeze::from_freeze_error;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::soft_error::Buck2StarlarkSoftErrorHandler;
use buck2_interpreter::starlark_profiler::config::GetStarlarkProfilerInstrumentation;
use buck2_interpreter::starlark_profiler::data::StarlarkProfileDataAndStats;
use clap::error::ErrorKind;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use itertools::Itertools;
use once_cell::sync::Lazy;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::structs::AllocStruct;
use starlark::values::structs::StructRef;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::UnpackValue;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark_map::ordered_map::OrderedMap;
use tokio::sync::Semaphore;

use crate::bxl::key::BxlKey;
use crate::bxl::starlark_defs::bxl_function::FrozenBxlFunction;
use crate::bxl::starlark_defs::cli_args::CliArgValue;
use crate::bxl::starlark_defs::context::actions::BxlExecutionResolution;
use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::context::BxlContextCoreData;
use crate::bxl::starlark_defs::eval_extra::BxlEvalExtra;
use crate::bxl::starlark_defs::functions::BxlErrorWithoutStacktrace;

pub(crate) static LIMITED_EXECUTOR: Lazy<Arc<LimitedExecutor>> = Lazy::new(|| {
    Arc::new(LimitedExecutor::new(500)) // Default working thread of tokio is 512 threads. We set it to 500 for here to leave some room for other things.
});

/// A limited executor that can be used to limit the number of concurrent bxl execution threads.
pub(crate) struct LimitedExecutor {
    semaphore: Arc<Semaphore>,
}

impl LimitedExecutor {
    fn new(limit: usize) -> Self {
        Self {
            semaphore: Arc::new(Semaphore::new(limit)),
        }
    }

    pub(crate) async fn execute<F, T>(&self, task: F) -> T
    where
        F: Future<Output = T>,
    {
        let _permit = self.semaphore.acquire().await.unwrap();
        task.await
    }
}

pub(crate) async fn eval(
    ctx: &mut DiceComputations<'_>,
    key: BxlKey,
    liveness: CancellationObserver,
) -> buck2_error::Result<(BxlResult, Option<StarlarkProfileDataAndStats>)> {
    // Note: because we use `block_in_place`, that will prevent the inner future from being polled
    // and yielded. So, for cancellation observers to work properly within the dice cancellable
    // future context, we need the future that it's attached to the cancellation context can
    // yield and be polled. To ensure that, we have to spawn the future that then enters block_in_place

    let dispatcher = ctx.per_transaction_data().get_dispatcher().dupe();

    let limited_executor = LIMITED_EXECUTOR.clone();

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
                limited_executor.execute(eval_bxl_inner(ctx, dispatcher, key, liveness)),
                || Err(buck2_error!(buck2_error::ErrorTag::Tier0, "cancelled")),
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
    ) -> buck2_error::Result<(FrozenModule, BxlResult)> {
        let BxlInnerEvaluator {
            data,
            module,
            liveness,
            digest_config,
            dispatcher,
        } = self;
        let bxl_dice = BxlSafeDiceComputations::new(dice, liveness);
        let bxl_dice = Rc::new(RefCell::new(bxl_dice));
        let data = Rc::new(data);

        let env = Module::new();
        let key = data.key().dupe();

        let output_stream = mk_stream_cache("output", &key);
        let file_path = data
            .artifact_fs()
            .buck_out_path_resolver()
            .resolve_gen(&output_stream);

        let file = Rc::new(RefCell::new(
            data.project_fs()
                .create_file(&file_path, false)
                .buck_error_context("Failed to create output cache for BXL")?,
        ));

        let error_stream = mk_stream_cache("error", &key);
        let error_file_path = data
            .artifact_fs()
            .buck_out_path_resolver()
            .resolve_gen(&error_stream);

        let error_file = Rc::new(RefCell::new(
            data.project_fs()
                .create_file(&error_file_path, false)
                .buck_error_context("Failed to create error cache for BXL")?,
        ));

        let (actions, ensured_artifacts) = {
            let resolved_args = ValueOfUnchecked::<StructRef>::unpack_value_err(
                env.heap().alloc(AllocStruct(
                    key.cli_args()
                        .iter()
                        .map(|(k, v)| (k, v.as_starlark(env.heap()))),
                )),
            )?;

            let print = EventDispatcherPrintHandler(dispatcher.clone());
            let extra = BxlEvalExtra::new(bxl_dice.dupe(), data.dupe(), error_file.dupe());

            let (mut eval, _) = provider.make(&env)?;
            let bxl_function_name = key.label().name.clone();
            let frozen_callable = get_bxl_callable(key.label(), &module)?;
            eval.set_print_handler(&print);
            eval.set_soft_error_handler(&Buck2StarlarkSoftErrorHandler);

            eval.extra = Some(&extra);

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

            tokio::task::block_in_place(|| {
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
                                    bxl_ctx,
                                    provider,
                                    force_print_stacktrace,
                                ),
                                BxlExecutionEnd {},
                            )
                        },
                    )
                })
            })?;

            BxlContext::take_state(bxl_ctx)?
        };

        let actions_finalizer = actions.finalize(&env)?;

        // TODO(cjhopman): Why is there so much divergence in code here for whether we created actions or
        // not? It seems to just make this unnecessarily complex.

        let frozen_module = env.freeze().map_err(from_freeze_error)?;
        let recorded_values = actions_finalizer(&frozen_module)?;

        let bxl_result = BxlResult::new(
            output_stream,
            error_stream,
            ensured_artifacts,
            recorded_values,
        );

        Ok((frozen_module, bxl_result))
    }
}

async fn eval_bxl_inner(
    ctx: &mut DiceComputations<'_>,
    dispatcher: EventDispatcher,
    key: BxlKey,
    liveness: CancellationObserver,
) -> buck2_error::Result<(BxlResult, Option<StarlarkProfileDataAndStats>)> {
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

    let eval_ctx = BxlInnerEvaluator {
        data: core_data,
        module: bxl_module,
        liveness,
        digest_config,
        dispatcher,
    };

    let eval_kind = key.as_starlark_eval_kind();
    let mut profiler = ctx.get_starlark_profiler(&eval_kind).await?;
    let (frozen_module, bxl_result) = with_starlark_eval_provider(
        ctx,
        &mut profiler.as_mut(),
        &eval_kind,
        move |provider, ctx| eval_ctx.do_eval(provider, ctx),
    )
    .await?;

    let profile_data = profiler.finish(Some(&frozen_module))?;
    Ok((bxl_result, profile_data))
}

// We use a file as our output/error stream cache. The file is associated with the `BxlDynamicKey` (created from `BxlKey`),
// which is super important, as it HAS to be the SAME as the DiceKey so that DICE is keeping the output file
// cache up to date. `BxlDynamicKey` requires an execution platform. We set the execution platform to be unspecified here
// because BXL functions do not have execution platform resolutions. exec_deps, toolchains, target_platform, and exec_compatible_with
// are empty here for the same reason.
pub(crate) fn mk_stream_cache(stream_type: &str, key: &BxlKey) -> BuildArtifactPath {
    BuildArtifactPath::new(
        key.dupe()
            .into_base_deferred_key(BxlExecutionResolution::unspecified()),
        ForwardRelativePathBuf::unchecked_new(format!(
            "__bxl_internal__/{}stream_cache",
            stream_type
        )),
    )
}

fn eval_bxl<'v>(
    eval: &mut Evaluator<'v, '_, '_>,
    frozen_callable: OwnedFrozenValueTyped<FrozenBxlFunction>,
    ctx: ValueTyped<'v, BxlContext<'v>>,
    provider: &mut dyn StarlarkEvaluatorProvider,
    force_print_stacktrace: bool,
) -> buck2_error::Result<()> {
    let bxl_impl = frozen_callable.implementation();
    let result = eval.eval_function(bxl_impl.to_value(), &[ctx.to_value()], &[]);

    provider
        .evaluation_complete(eval)
        .buck_error_context("Profiler finalization failed")?;

    let e = match result {
        Ok(v) => {
            if !v.is_none() {
                return Err(NotAValidReturnType(v.get_type()).into());
            }

            return Ok(());
        }
        Err(e) => e,
    };

    let should_skip_backtrace = !force_print_stacktrace
        && match e.kind() {
            starlark::ErrorKind::Native(e) => {
                e.downcast_ref::<BxlErrorWithoutStacktrace>().is_some()
            }
            _ => false,
        };

    let e = from_starlark_with_options(
        e,
        buck2_error::starlark_error::NativeErrorHandling::Unknown,
        should_skip_backtrace,
    );
    if should_skip_backtrace {
        let dispatcher = get_dispatcher();
        dispatcher.instant_event(StarlarkFailNoStacktrace {
            trace: format!("{}", e),
        });
        dispatcher
            .console_message("Re-run the script with `-v5` to show the full stacktrace".to_owned());
    }

    Err(e.into())
}

pub(crate) fn get_bxl_callable(
    spec: &BxlFunctionLabel,
    bxl_module: &LoadedModule,
) -> buck2_error::Result<OwnedFrozenValueTyped<FrozenBxlFunction>> {
    let callable = bxl_module
        .env()
        .get_any_visibility(&spec.name)
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?
        .0;

    Ok(callable.downcast_starlark::<FrozenBxlFunction>()?)
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
) -> buck2_error::Result<BxlResolvedCliArgs> {
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
#[buck2(tag = Input)]
struct NotAValidReturnType(&'static str);
