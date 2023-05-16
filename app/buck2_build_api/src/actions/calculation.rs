/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter::zip;
use std::sync::Arc;
use std::time::Instant;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_common::result::ToUnsharedResultExt;
use buck2_data::ToProtoMessage;
use buck2_events::dispatch::async_record_root_spans;
use buck2_events::dispatch::span_async;
use buck2_events::span::SpanId;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::result::CommandExecutionReport;
use buck2_execute::execute::result::CommandExecutionStatus;
use buck2_execute::output_size::OutputSize;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::future;
use futures::stream::FuturesOrdered;
use futures::Future;
use futures::FutureExt;
use indexmap::IndexMap;
use more_futures::cancellation::CancellationContext;
use ref_cast::RefCast;
use smallvec::SmallVec;
use tracing::debug;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::build_listener::NodeDuration;
use crate::actions::execute::action_executor::ActionOutputs;
use crate::actions::execute::action_executor::HasActionExecutor;
use crate::actions::key::ActionKey;
use crate::actions::RegisteredAction;
use crate::artifact_groups::calculation::ensure_artifact_group_staged;
use crate::deferred::calculation::DeferredCalculation;
use crate::keep_going;

pub struct ActionCalculation;

async fn build_action_impl(
    ctx: &DiceComputations,
    cancellation: &CancellationContext,
    key: &ActionKey,
) -> anyhow::Result<ActionOutputs> {
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
    ctx: &DiceComputations,
    cancellation: &CancellationContext,
    action: Arc<RegisteredAction>,
) -> anyhow::Result<ActionOutputs> {
    let materialized_inputs = {
        let inputs = action.inputs()?;
        let ensure_futs: FuturesOrdered<_> = inputs
            .iter()
            .map(|v| ensure_artifact_group_staged(ctx, v))
            .collect();

        let ready_inputs: Vec<_> =
            tokio::task::unconstrained(keep_going::try_join_all(ctx, ensure_futs)).await?;

        let mut results = IndexMap::with_capacity(inputs.len());
        for (artifact, ready) in zip(inputs.iter(), ready_inputs.into_iter()) {
            results.insert(artifact.clone(), ready.to_group_values(artifact)?);
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
        .context(format!("for action `{}`", action))?;

    let now = Instant::now();
    let action = &action;

    let fut = async move {
        let (execute_result, command_reports) = executor
            .execute(materialized_inputs, &action, cancellation)
            .await;

        let allow_omit_details = execute_result.is_ok();

        let commands = future::join_all(
            command_reports
                .iter()
                .map(|r| command_execution_report_to_proto(r, allow_omit_details)),
        )
        .await;

        let action_result;
        let execution_kind;
        let wall_time;
        let error;
        let output_size;

        let mut prefers_local = None;
        let mut requires_local = None;
        let mut allows_cache_upload = None;
        let mut did_cache_upload = None;
        let mut eligible_for_full_hybrid = None;

        let mut buck2_revision = None;
        let mut buck2_build_time = None;
        let mut hostname = None;

        match execute_result {
            Ok((outputs, meta)) => {
                output_size = outputs.calc_output_count_and_bytes().bytes;
                action_result = Ok(outputs);
                execution_kind = Some(meta.execution_kind.as_enum());
                wall_time = Some(meta.timing.wall_time);
                error = None;

                if let Some(command) = meta.execution_kind.command() {
                    prefers_local = Some(command.prefers_local);
                    requires_local = Some(command.requires_local);
                    allows_cache_upload = Some(command.allows_cache_upload);
                    did_cache_upload = Some(command.did_cache_upload);
                    eligible_for_full_hybrid = Some(command.eligible_for_full_hybrid);
                }
            }
            Err(e) => {
                // Because we already are sending the error message in the
                // ActionExecutionEnd event, we slim the error down in the result.
                // We can then unconditionally print the error message for compute(),
                // including ones near the beginning of this method, and also not
                // duplicate any error messages.
                action_result = Err(anyhow::anyhow!("Failed to build '{}'", action.owner()));
                // TODO (torozco): Remove (see protobuf file)?
                execution_kind = command_reports
                    .last()
                    .and_then(|r| r.status.execution_kind())
                    .map(|e| e.as_enum());
                wall_time = None;
                error = Some(e.as_proto());
                output_size = 0;
                // We define the below fields only in the instance of an action error
                // so as to reduce Scribe traffic and log it in buck2_action_errors
                buck2_revision = buck2_build_info::revision().map(|s| s.to_owned());
                buck2_build_time = buck2_build_info::time_iso8601().map(|s| s.to_owned());
                hostname = buck2_events::metadata::hostname();
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

        (
            (action_result, wall_time),
            Box::new(buck2_data::ActionExecutionEnd {
                key: Some(action.key().as_proto()),
                kind: action.kind().into(),
                name: Some(buck2_data::ActionName {
                    category: action.category().as_str().to_owned(),
                    identifier: action.identifier().unwrap_or("").to_owned(),
                }),
                failed: error.is_some(),
                error,
                always_print_stderr: action.always_print_stderr(),
                wall_time: wall_time.and_then(|d| d.try_into().ok()),
                execution_kind: execution_kind.unwrap_or(buck2_data::ActionExecutionKind::NotSet)
                    as i32,
                output_size,
                commands,
                outputs,
                prefers_local: prefers_local.unwrap_or_default(),
                requires_local: requires_local.unwrap_or_default(),
                allows_cache_upload: allows_cache_upload.unwrap_or_default(),
                did_cache_upload: did_cache_upload.unwrap_or_default(),
                eligible_for_full_hybrid,
                buck2_revision,
                buck2_build_time,
                hostname,
            }),
        )
    };

    // boxed() the future so that we don't need to allocate space for it while waiting on input dependencies.
    let ((res, wall_time), spans) =
        async_record_root_spans(span_async(start_event, fut.boxed())).await;

    // TODO: This wall time is rather wrong. We should report a wall time on failures too.
    ctx.store_evaluation_data(BuildKeyActivationData {
        action: action.dupe(),
        duration: NodeDuration {
            user: wall_time.unwrap_or_default(),
            total: now.elapsed(),
        },
        spans,
    })?;

    res
}

pub struct BuildKeyActivationData {
    pub action: Arc<RegisteredAction>,
    pub duration: NodeDuration,
    pub spans: SmallVec<[SpanId; 1]>,
}

/// The cost of these calls are particularly critical. To control the cost (particularly size) of these calls
/// we drop the `async_trait` common in other `*Calculation` types and avoid `async fn` (for
/// build_action/build_artifact at least).
impl ActionCalculation {
    pub async fn get_action(
        ctx: &DiceComputations,
        action_key: &ActionKey,
    ) -> anyhow::Result<Arc<RegisteredAction>> {
        // TODO add async/deferred stuff
        ctx.compute_deferred_data(action_key.deferred_data())
            .await
            .map(|a| (*a).dupe())
            .with_context(|| format!("for action key `{}`", action_key))
    }

    pub fn build_action<'a>(
        ctx: &'a DiceComputations,
        action_key: &'a ActionKey,
    ) -> impl Future<Output = anyhow::Result<ActionOutputs>> + 'a {
        // build_action is called for every action key. We don't use `async fn` to ensure that it has minimal cost.
        // We don't currently consume this in buck_e2e but it's good to log for debugging purposes.
        debug!("build_action {}", action_key);
        ctx.compute(BuildKey::ref_cast(action_key))
            .map(|v| v?.unshared_error())
    }

    pub fn build_artifact<'a>(
        ctx: &'a DiceComputations,
        artifact: &'a BuildArtifact,
    ) -> impl Future<Output = anyhow::Result<ActionOutputs>> + 'a {
        Self::build_action(ctx, artifact.key())
    }
}

#[derive(Clone, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, RefCast)]
#[repr(transparent)]
pub(crate) struct BuildKey(pub(crate) ActionKey);

#[async_trait]
impl Key for BuildKey {
    type Value = SharedResult<ActionOutputs>;

    async fn compute(
        &self,
        ctx: &DiceComputations,
        cancellation: &CancellationContext,
    ) -> Self::Value {
        build_action_impl(ctx, cancellation, &self.0)
            .await
            .shared_error()
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
        CommandExecutionStatus::Cancelled => buck2_data::command_execution::Cancelled {}.into(),
        CommandExecutionStatus::Failure { .. } => buck2_data::command_execution::Failure {}.into(),
        CommandExecutionStatus::TimedOut { duration, .. } => {
            buck2_data::command_execution::Timeout {
                duration: (*duration).try_into().ok(),
            }
            .into()
        }
        CommandExecutionStatus::Error { stage, error } => buck2_data::command_execution::Error {
            stage: (*stage).to_owned(),
            error: format!("{:#}", error),
        }
        .into(),
    };

    buck2_data::CommandExecution {
        details: Some(details),
        status: Some(status),
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

    let command_data = command.status.execution_kind().map(|kind| match kind {
        CommandExecutionKind::Local {
            command,
            env,
            digest,
        } => {
            if omit_details {
                buck2_data::OmittedLocalCommand {
                    action_digest: digest.to_string(),
                }
                .into()
            } else {
                buck2_data::LocalCommand {
                    action_digest: digest.to_string(),
                    argv: command.to_owned(),
                    env: env
                        .iter()
                        .map(|(key, value)| buck2_data::local_command::EnvironmentEntry {
                            key: key.clone(),
                            value: value.clone(),
                        })
                        .collect(),
                }
                .into()
            }
        }
        CommandExecutionKind::Remote { digest } => buck2_data::RemoteCommand {
            action_digest: digest.to_string(),
            cache_hit: false,
            queue_time: command.timing.re_queue_time.and_then(|d| d.try_into().ok()),
        }
        .into(),
        CommandExecutionKind::ActionCache { digest } => buck2_data::RemoteCommand {
            action_digest: digest.to_string(),
            cache_hit: true,
            queue_time: command.timing.re_queue_time.and_then(|d| d.try_into().ok()),
        }
        .into(),
    });

    buck2_data::CommandExecutionDetails {
        exit_code: signed_exit_code.and_then(|e| e.try_into().ok()), // To be deprecated in favor of signed_exit_code
        stdout,
        stderr,
        command: command_data,
        signed_exit_code,
        execution_stats: command.timing.execution_stats,
    }
}
