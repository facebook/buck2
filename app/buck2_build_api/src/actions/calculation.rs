/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::future::Future;
use std::iter::zip;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_signals::env::NodeDuration;
use buck2_build_signals::env::WaitingData;
use buck2_common::events::HasEvents;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_data::ActionErrorDiagnostics;
use buck2_data::ActionSubErrors;
use buck2_data::ToProtoMessage;
use buck2_data::get_action_digest;
use buck2_error::BuckErrorContext;
use buck2_event_observer::action_util::get_execution_time_ms;
use buck2_events::dispatch::async_record_root_spans;
use buck2_events::dispatch::get_dispatcher;
use buck2_events::dispatch::span_async;
use buck2_events::span::SpanId;
use buck2_execute::execute::result::CommandExecutionReport;
use buck2_execute::execute::result::CommandExecutionStatus;
use buck2_execute::output_size::OutputSize;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::soft_error::Buck2StarlarkSoftErrorHandler;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_util::time_span::TimeSpan;
use derive_more::Display;
use dice::DiceComputations;
use dice::DiceTrackedInvalidationPath;
use dice::Key;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::FutureExt;
use futures::future::BoxFuture;
use futures::future::{self};
use indexmap::IndexMap;
use ref_cast::RefCast;
use smallvec::SmallVec;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use tracing::debug;

use crate::actions::RegisteredAction;
use crate::actions::artifact::get_artifact_fs::GetArtifactFs;
use crate::actions::error::ActionError;
use crate::actions::error_handler::ActionErrorHandlerError;
use crate::actions::error_handler::ActionSubErrorResult;
use crate::actions::error_handler::StarlarkActionErrorContext;
use crate::actions::execute::action_executor::ActionOutputs;
use crate::actions::execute::action_executor::BuckActionExecutor;
use crate::actions::execute::action_executor::HasActionExecutor;
use crate::actions::execute::error::ExecuteError;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::artifact_groups::calculation::ensure_artifact_group_staged;
use crate::build::detailed_aggregated_metrics::dice::HasDetailedAggregatedMetrics;
use crate::build::detailed_aggregated_metrics::types::ActionExecutionMetrics;
use crate::deferred::calculation::ActionLookup;
use crate::deferred::calculation::lookup_deferred_holder;
use crate::keep_going::KeepGoing;
use crate::starlark::values::UnpackValue;
use crate::starlark::values::type_repr::StarlarkTypeRepr;

pub struct ActionCalculation;

async fn build_action_impl(
    ctx: &mut DiceComputations<'_>,
    cancellation: &CancellationContext,
    key: &ActionKey,
) -> buck2_error::Result<ActionOutputs> {
    // Compute is only called if we have cache miss
    debug!("compute {}", key);

    let action = ActionCalculation::get_action(ctx, key).await?;

    if action.key() != key {
        // The action key we start with is on the DICE graph, and thus cached
        // and properly deduplicated. But if the underlying has a different key,
        // e.g. due to dynamic_output, then we might have two different action keys
        // pointing at the same underlying action. We need to make sure that
        // underlying action only gets called once, so call build_action once
        // again with the new key to get DICE deduplication.
        let res = ActionCalculation::build_action(ctx, action.key()).await;
        return res;
    }

    build_action_no_redirect(ctx, cancellation, action).await
}

async fn build_action_no_redirect(
    ctx: &mut DiceComputations<'_>,
    cancellation: &CancellationContext,
    action: Arc<RegisteredAction>,
) -> buck2_error::Result<ActionOutputs> {
    let inputs = action.inputs()?;
    let waiting_data = WaitingData::new();
    let ensured_inputs = if inputs.is_empty() {
        IndexMap::new()
    } else {
        let ready_inputs: Vec<_> = tokio::task::unconstrained(KeepGoing::try_compute_join_all(
            ctx,
            inputs.iter(),
            |ctx, v| {
                async move {
                    let resolved = v.resolved_artifact(ctx).await?;
                    buck2_error::Ok(
                        ensure_artifact_group_staged(ctx, resolved.clone())
                            .await?
                            .to_group_values(&resolved)?,
                    )
                }
                .boxed()
            },
        ))
        .await?;

        let mut results = IndexMap::with_capacity(inputs.len());
        for (artifact, ready) in zip(inputs.iter(), ready_inputs) {
            results.insert(artifact.clone(), ready);
        }
        results
    };

    let start_event = buck2_data::ActionExecutionStart {
        key: Some(action.key().as_proto()),
        kind: action.kind().into(),
        name: Some(buck2_data::ActionName {
            category: action.category().as_str().to_owned(),
            identifier: action.identifier().unwrap_or("").to_owned(),
        }),
    };

    let executor = ctx
        .get_action_executor(action.execution_config())
        .await
        .buck_error_context(format!("for action `{action}`"))?;

    let now = TimeSpan::start_now();
    let action = &action;

    let target = match action.key().owner() {
        BaseDeferredKey::TargetLabel(target_label) => Some(target_label.dupe()),
        _ => None,
    };

    let target_rule_type_name = match target {
        Some(label) => Some(get_target_rule_type_name(ctx, &label).await?),
        None => None,
    };

    let fut = build_action_inner(
        ctx,
        cancellation,
        &executor,
        waiting_data,
        ensured_inputs,
        action,
        target_rule_type_name,
    );

    // boxed() the future so that we don't need to allocate space for it while waiting on input dependencies.
    let (action_execution_data, spans) =
        async_record_root_spans(span_async(start_event, fut.boxed())).await;

    let execution_metrics = ActionExecutionMetrics {
        key: action.key().dupe(),
        execution_time_ms: action_execution_data
            .extra_data
            .execution_time_ms
            .unwrap_or_default(),
        execution_kind: action_execution_data.extra_data.execution_kind,
        output_size_bytes: action_execution_data.extra_data.output_size,
        memory_peak: action_execution_data.memory_peak,
    };
    ctx.store_evaluation_data(BuildKeyActivationData {
        action_with_extra_data: ActionWithExtraData {
            action: action.dupe(),
            extra_data: action_execution_data.extra_data,
        },
        duration: NodeDuration {
            user: action_execution_data.wall_time.unwrap_or_default(),
            total: now.end_now(),
            queue: action_execution_data.queue_duration,
        },
        spans,
        waiting_data: action_execution_data.waiting_data,
    })?;

    ctx.action_executed(execution_metrics)?;

    action_execution_data.action_result
}

async fn build_action_inner(
    ctx: &mut DiceComputations<'_>,
    cancellation: &CancellationContext,
    executor: &BuckActionExecutor,
    waiting_data: WaitingData,
    ensured_inputs: IndexMap<ArtifactGroup, ArtifactGroupValues>,
    action: &Arc<RegisteredAction>,
    target_rule_type_name: Option<String>,
) -> (ActionExecutionData, Box<buck2_data::ActionExecutionEnd>) {
    let is_eligible_for_dedupe = is_action_eligible_for_dedupe(action, &ensured_inputs);
    let is_expected_eligible_for_dedupe = match action.is_expected_eligible_for_dedupe() {
        Some(v) => {
            if v {
                buck2_data::ExpectedEligibleForDedupe::ExpectedEligible
            } else {
                buck2_data::ExpectedEligibleForDedupe::ExpectedIneligible
            }
        }
        None => buck2_data::ExpectedEligibleForDedupe::UnknownEligibility,
    };
    let (execute_result, command_reports) = executor
        .execute(waiting_data, ensured_inputs, action, cancellation)
        .await;

    let allow_omit_details = execute_result.is_ok();

    let commands = future::join_all(
        command_reports
            .iter()
            .map(|r| command_execution_report_to_proto(r, allow_omit_details)),
    )
    .await;

    let action_digest = get_action_digest(&commands);

    let queue_duration = command_reports.last().and_then(|r| r.timing.queue_duration);
    let memory_peak = command_reports
        .last()
        .and_then(|r| r.timing.execution_stats.and_then(|s| s.memory_peak));

    let action_key = action.key().as_proto();

    let action_name = buck2_data::ActionName {
        category: action.category().as_str().to_owned(),
        identifier: action.identifier().unwrap_or("").to_owned(),
    };

    let action_result;
    let execution_kind;
    let wall_time;
    let error;
    let output_size;

    let mut prefers_local = None;
    let mut requires_local = None;
    let mut allows_cache_upload = None;
    let mut did_cache_upload = None;
    let mut allows_dep_file_cache_upload = None;
    let mut did_dep_file_cache_upload = None;
    let mut dep_file_key = None;
    let mut eligible_for_full_hybrid = None;

    let mut buck2_revision = None;
    let mut buck2_build_time = None;
    let mut hostname = None;
    let mut input_files_bytes = None;
    let mut scheduling_mode = None;
    let mut incremental_kind = None;
    let mut waiting_data = None;
    let error_diagnostics = match execute_result {
        Ok((outputs, meta)) => {
            output_size = outputs.calc_output_count_and_bytes().bytes;
            action_result = Ok(outputs);
            execution_kind = Some(meta.execution_kind.as_enum());
            wall_time = Some(meta.timing.wall_time);
            error = None;
            input_files_bytes = meta.input_files_bytes;
            waiting_data = Some(meta.waiting_data);

            if let Some(command) = meta.execution_kind.command() {
                prefers_local = Some(command.prefers_local);
                requires_local = Some(command.requires_local);
                allows_cache_upload = Some(command.allows_cache_upload);
                did_cache_upload = Some(command.did_cache_upload);
                allows_dep_file_cache_upload = Some(command.allows_dep_file_cache_upload);
                did_dep_file_cache_upload = Some(command.did_dep_file_cache_upload);
                dep_file_key = *command.dep_file_key;
                eligible_for_full_hybrid = Some(command.eligible_for_full_hybrid);
                scheduling_mode = command.scheduling_mode;
                incremental_kind = Some(command.incremental_kind);
            }

            None
        }
        Err(e) => {
            // TODO (torozco): Remove (see protobuf file)?
            execution_kind = command_reports
                .last()
                .and_then(|r| r.status.execution_kind())
                .map(|e| e.as_enum());
            wall_time = command_reports
                .last()
                .map(|r| r.timing.time_span.duration());
            output_size = 0;
            // We define the below fields only in the instance of an action error
            // so as to reduce Scribe traffic and log it in buck2_action_errors
            buck2_revision = buck2_build_info::revision().map(|s| s.to_owned());
            buck2_build_time = buck2_build_info::time_iso8601().map(|s| s.to_owned());
            hostname = buck2_events::metadata::hostname();

            let last_command = commands.last().cloned();

            let outputs = match &e {
                ExecuteError::CommandExecutionError { action_outputs, .. } => Some(action_outputs),
                _ => None,
            };

            let error_diagnostics = try_run_error_handler(
                action.dupe(),
                last_command.as_ref(),
                ctx.get_artifact_fs().await,
                outputs,
            );

            let e = ActionError::new(
                e,
                action_name.clone(),
                action_key.clone(),
                last_command.clone(),
                error_diagnostics.clone(),
            );

            error = Some(e.as_proto_field());

            ctx.per_transaction_data()
                .get_dispatcher()
                .instant_event(e.as_proto_event());

            action_result = Err(buck2_error::Error::from(e)
                // Make sure to mark the error as emitted so that it is not printed out to console
                // again in this command. We still need to keep it around for the build report (and
                // in the future) other commands
                .mark_emitted({
                    let owner = action.owner().dupe();
                    Arc::new(move |f| write!(f, "Failed to build '{owner}'"))
                })
                .into());

            error_diagnostics
        }
    };

    let outputs = action_result
        .as_ref()
        .map(|outputs| {
            outputs
                .iter()
                .filter_map(|(_artifact, value)| {
                    Some(buck2_data::ActionOutput {
                        tiny_digest: value.digest()?.tiny_digest().to_string(),
                    })
                })
                .collect()
        })
        .unwrap_or_default();

    let invalidation_info = if executor.invalidation_tracking_enabled() {
        fn to_proto(
            invalidation_path: &DiceTrackedInvalidationPath,
        ) -> Option<buck2_data::command_invalidation_info::InvalidationSource> {
            match invalidation_path {
                dice::DiceTrackedInvalidationPath::Clean
                | dice::DiceTrackedInvalidationPath::Unknown => None,
                dice::DiceTrackedInvalidationPath::Invalidated(_) => {
                    Some(buck2_data::command_invalidation_info::InvalidationSource {})
                }
            }
        }
        let invalidation_paths = ctx.get_invalidation_paths();
        Some(buck2_data::CommandInvalidationInfo {
            changed_any: to_proto(&invalidation_paths.normal_priority_path),
            changed_file: to_proto(&invalidation_paths.high_priority_path),
        })
    } else {
        None
    };

    let execution_kind = execution_kind.unwrap_or(buck2_data::ActionExecutionKind::NotSet);

    (
        ActionExecutionData {
            action_result,
            wall_time,
            queue_duration,
            memory_peak,
            extra_data: ActionExtraData {
                execution_kind,
                target_rule_type_name: target_rule_type_name.clone(),
                action_digest,
                invalidation_info,
                execution_time_ms: get_execution_time_ms(&commands),
                output_size,
            },
            waiting_data: waiting_data.unwrap_or_default(),
        },
        Box::new(buck2_data::ActionExecutionEnd {
            key: Some(action_key),
            kind: action.kind().into(),
            name: Some(action_name),
            failed: error.is_some(),
            error,
            always_print_stderr: action.always_print_stderr(),
            wall_time: wall_time.and_then(|d| d.try_into().ok()),
            execution_kind: execution_kind as i32,
            output_size,
            commands,
            outputs,
            prefers_local: prefers_local.unwrap_or_default(),
            requires_local: requires_local.unwrap_or_default(),
            allows_cache_upload: allows_cache_upload.unwrap_or_default(),
            did_cache_upload: did_cache_upload.unwrap_or_default(),
            allows_dep_file_cache_upload: allows_dep_file_cache_upload.unwrap_or_default(),
            did_dep_file_cache_upload: did_dep_file_cache_upload.unwrap_or_default(),
            dep_file_key: dep_file_key.map(|d| d.to_string()),
            eligible_for_full_hybrid,
            buck2_revision,
            buck2_build_time,
            hostname,
            error_diagnostics,
            input_files_bytes,
            invalidation_info,
            target_rule_type_name,
            scheduling_mode: scheduling_mode.map(|h| h as i32),
            incremental_kind: incremental_kind.map(|k| k as i32),
            eligible_for_dedupe: is_eligible_for_dedupe as i32,
            expected_eligible_for_dedupe: is_expected_eligible_for_dedupe as i32,
        }),
    )
}

fn is_action_eligible_for_dedupe(
    action: &Arc<RegisteredAction>,
    inputs: &IndexMap<ArtifactGroup, ArtifactGroupValues>,
) -> buck2_data::EligibleForDedupe {
    if !action.all_outputs_are_content_based() {
        return buck2_data::EligibleForDedupe::IneligibleOutput;
    }

    let target_platform =
        if let BaseDeferredKey::TargetLabel(configured_label) = action.key().owner() {
            Some(configured_label.cfg())
        } else {
            None
        };

    for (ag, _agv) in inputs.iter() {
        if !ag.is_eligible_for_dedupe(target_platform) {
            return buck2_data::EligibleForDedupe::IneligibleInput;
        }
    }

    buck2_data::EligibleForDedupe::Eligible
}

// Attempt to run the error handler if one was specified. Returns either the error diagnostics, or
// an actual error if the handler failed to run successfully.
fn try_run_error_handler(
    action: Arc<RegisteredAction>,
    last_command: Option<&buck2_data::CommandExecution>,
    artifact_fs: buck2_error::Result<ArtifactFs>,
    outputs: Option<&ActionOutputs>,
) -> Option<ActionErrorDiagnostics> {
    use buck2_data::action_error_diagnostics::Data;

    fn create_error(
        e: buck2_error::Error,
    ) -> (
        Option<ActionErrorDiagnostics>,
        buck2_data::ActionErrorHandlerExecutionEnd,
    ) {
        (
            Some(ActionErrorDiagnostics {
                data: Some(Data::HandlerInvocationError(format!("{e:#}"))),
            }),
            buck2_data::ActionErrorHandlerExecutionEnd {},
        )
    }

    match action.action.error_handler() {
        Some(error_handler) => {
            let dispatcher = get_dispatcher();

            dispatcher
                .clone()
                .span(buck2_data::ActionErrorHandlerExecutionStart {}, || {
                    Module::with_temp_heap(|env| {
                        let heap = env.heap();
                        let print = EventDispatcherPrintHandler(get_dispatcher());
                        let mut eval = Evaluator::new(&env);
                        eval.set_print_handler(&print);
                        eval.set_soft_error_handler(&Buck2StarlarkSoftErrorHandler);

                        let artifact_fs = match artifact_fs {
                            Ok(fs) => fs,
                            Err(e) => return create_error(e),
                        };

                        let outputs_artifacts = match action.action.failed_action_output_artifacts(
                            &artifact_fs,
                            heap,
                            outputs,
                        ) {
                            Ok(v) => v,
                            Err(e) => return create_error(e),
                        };

                        let error_handler_ctx =
                            StarlarkActionErrorContext::new_from_command_execution(
                                last_command,
                                outputs_artifacts,
                            );

                        let error_handler_result = eval.eval_function(
                            error_handler.value(),
                            &[heap.alloc(error_handler_ctx)],
                            &[],
                        );

                        let data = match error_handler_result {
                            Ok(result) => match ActionSubErrorResult::unpack_value_err(result) {
                                Ok(result) => Data::SubErrors(ActionSubErrors {
                                    sub_errors: result
                                        .items
                                        .into_iter()
                                        .map(|s| s.to_proto())
                                        .collect(),
                                }),
                                Err(_) => Data::HandlerInvocationError(format!(
                                    "{}",
                                    ActionErrorHandlerError::TypeError(
                                        ActionSubErrorResult::starlark_type_repr(),
                                        result.get_type().to_owned()
                                    )
                                )),
                            },
                            Err(e) => {
                                let e = buck2_error::Error::from(e).context("Error handler failed");
                                Data::HandlerInvocationError(format!("{e:#}"))
                            }
                        };
                        (
                            Some(ActionErrorDiagnostics { data: Some(data) }),
                            buck2_data::ActionErrorHandlerExecutionEnd {},
                        )
                    })
                })
        }
        None => None,
    }
}

pub struct BuildKeyActivationData {
    pub action_with_extra_data: ActionWithExtraData,
    pub duration: NodeDuration,
    pub waiting_data: WaitingData,
    pub spans: SmallVec<[SpanId; 1]>,
}

#[derive(Clone)]
pub struct ActionWithExtraData {
    pub action: Arc<RegisteredAction>,
    pub extra_data: ActionExtraData,
}

#[derive(Clone)]
pub struct ActionExtraData {
    pub execution_kind: buck2_data::ActionExecutionKind,
    pub execution_time_ms: Option<u64>,
    pub output_size: u64,
    pub target_rule_type_name: Option<String>,
    pub action_digest: Option<String>,
    pub invalidation_info: Option<buck2_data::CommandInvalidationInfo>,
}

struct ActionExecutionData {
    action_result: buck2_error::Result<ActionOutputs>,
    wall_time: Option<std::time::Duration>,
    queue_duration: Option<std::time::Duration>,
    memory_peak: Option<u64>,
    extra_data: ActionExtraData,
    waiting_data: WaitingData,
}

/// The cost of these calls are particularly critical. To control the cost (particularly size) of these calls
/// we drop the `async_trait` common in other `*Calculation` types and avoid `async fn` (for
/// build_action/build_artifact at least).
impl ActionCalculation {
    pub async fn get_action(
        ctx: &mut DiceComputations<'_>,
        action_key: &ActionKey,
    ) -> buck2_error::Result<Arc<RegisteredAction>> {
        // In the typical case, this lookup is only going to require a single deferred holder lookup. There's three cases:
        // 1. a normal action defined in analysis: lookup the holder for that analysis, get the action
        // 2. an action bound to a dynamic_output and then bound to an action there: the initial holder_key will actually
        //    point to the dynamic_output (not the analysis that first created the action key) and then the action will be found there
        // 3. an action bound to a dynamic_output, and then in that dynamic_output bound to another dynamic_output: only in this case
        //    will the initial lookup not find the key and we'll recurse.
        //
        // We could introduce a dice key to cache the recursive resolution, but that would only be valuable if we had long nested chains
        // of dynamic_output that were re-binding artifacts. In practice we've not yet encountered that.
        let deferred_holder = lookup_deferred_holder(ctx, action_key.holder_key()).await?;
        match deferred_holder.lookup_action(action_key)? {
            ActionLookup::Action(action) => Ok(action),
            ActionLookup::Deferred(action_key) => {
                fn get_action_recurse<'a>(
                    ctx: &'a mut DiceComputations<'_>,
                    action_key: &'a ActionKey,
                ) -> BoxFuture<'a, buck2_error::Result<Arc<RegisteredAction>>> {
                    async move { ActionCalculation::get_action(ctx, action_key).await }.boxed()
                }
                get_action_recurse(ctx, &action_key).await
            }
        }
    }

    pub fn build_action<'a>(
        ctx: &'a mut DiceComputations<'_>,
        action_key: &ActionKey,
    ) -> impl Future<Output = buck2_error::Result<ActionOutputs>> + use<'a> {
        // build_action is called for every action key. We don't use `async fn` to ensure that it has minimal cost.
        // We don't currently consume this in buck_e2e but it's good to log for debugging purposes.
        debug!("build_action {}", action_key);
        ctx.compute(BuildKey::ref_cast(action_key))
            .map(|v| v?.map_err(buck2_error::Error::from))
    }

    pub fn build_artifact<'a>(
        ctx: &'a mut DiceComputations<'_>,
        artifact: &BuildArtifact,
    ) -> impl Future<Output = buck2_error::Result<ActionOutputs>> + use<'a> {
        Self::build_action(ctx, artifact.key())
    }
}

#[derive(Clone, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, RefCast)]
#[repr(transparent)]
pub struct BuildKey(pub ActionKey);

#[async_trait]
impl Key for BuildKey {
    type Value = buck2_error::Result<ActionOutputs>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        cancellation: &CancellationContext,
    ) -> Self::Value {
        build_action_impl(ctx, cancellation, &self.0)
            .await
            .map_err(buck2_error::Error::from)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }

    fn validity(x: &Self::Value) -> bool {
        // we don't cache any kind of errors. Ideally, we could try to distinguish different
        // error types and try to cache non-transient error types, but practically there
        // are too many unknowns that may cause more harm than good if we cached errors.
        // So, don't cache it for now, until someday we decide to really need to.
        x.is_ok()
    }
}

async fn command_execution_report_to_proto(
    report: &CommandExecutionReport,
    allow_omit_details: bool,
) -> buck2_data::CommandExecution {
    let details = command_details(report, allow_omit_details).await;

    let status = match &report.status {
        CommandExecutionStatus::Success { .. } => buck2_data::command_execution::Success {}.into(),
        CommandExecutionStatus::Cancelled { .. } => {
            buck2_data::command_execution::Cancelled {}.into()
        }
        CommandExecutionStatus::Failure { .. } => buck2_data::command_execution::Failure {}.into(),
        CommandExecutionStatus::WorkerFailure { .. } => {
            buck2_data::command_execution::WorkerFailure {}.into()
        }
        CommandExecutionStatus::TimedOut { duration, .. } => {
            buck2_data::command_execution::Timeout {
                duration: (*duration).try_into().ok(),
            }
            .into()
        }
        CommandExecutionStatus::Error { stage, error, .. } => {
            buck2_data::command_execution::Error {
                stage: (*stage).to_owned(),
                error: format!("{error:#}"),
            }
            .into()
        }
    };

    buck2_data::CommandExecution {
        details: Some(details),
        status: Some(status),
        inline_environment_metadata: Some(report.inline_environment_metadata),
    }
}

pub async fn command_details(
    command: &CommandExecutionReport,
    allow_omit_details: bool,
) -> buck2_data::CommandExecutionDetails {
    // If the top-level command failed then we don't want to omit any details. If it succeeded and
    // so did this command (it could succeed while not having a success here if we have rejected
    // executions), then we'll strip non-relevant stuff.
    let omit_details =
        allow_omit_details && matches!(command.status, CommandExecutionStatus::Success { .. });

    let signed_exit_code = command.exit_code;

    let stdout;
    let stderr;

    if omit_details {
        stdout = Default::default();
        stderr = command.std_streams.to_lossy_stderr().await;
    } else {
        let pair = command.std_streams.to_lossy().await;
        stdout = pair.stdout;
        stderr = pair.stderr;
    };

    let command_kind = command
        .status
        .execution_kind()
        .map(|k| k.to_proto(omit_details));

    buck2_data::CommandExecutionDetails {
        cmd_stdout: stdout,
        cmd_stderr: stderr,
        command_kind,
        signed_exit_code,
        metadata: Some(command.timing.to_proto()),
        additional_message: command.additional_message.clone(),
    }
}

pub async fn get_target_rule_type_name(
    ctx: &mut DiceComputations<'_>,
    label: &ConfiguredTargetLabel,
) -> buck2_error::Result<String> {
    Ok(ctx
        .get_configured_target_node(label)
        .await?
        .require_compatible()?
        .underlying_rule_type()
        .name()
        .to_owned())
}
