/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::future::Future;
use std::sync::Arc;
use std::sync::Mutex;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_build_signals::env::BuildSignalsContext;
use buck2_build_signals::env::DeferredBuildSignals;
use buck2_build_signals::env::EarlyCommandEntry;
use buck2_build_signals::env::HasCriticalPathBackend;
use buck2_certs::validate::CertState;
use buck2_cli_proto::client_context::PreemptibleWhen;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::working_dir::AbsWorkingDir;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::ConfiguredProvidersPatternExtra;
use buck2_data::CommandCriticalEnd;
use buck2_data::CommandCriticalStart;
use buck2_data::DiceCriticalSectionEnd;
use buck2_data::DiceCriticalSectionStart;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::materialize::materializer::Materializer;
use buck2_futures::cancellation::CancellationContext;
use buck2_wrapper_common::invocation_id::TraceId;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;

use crate::concurrency::ConcurrencyHandler;
use crate::concurrency::DiceUpdater;
use crate::stderr_output_guard::StderrOutputGuard;

const TIME_SPENT_SYNCHRONIZING_AND_WAITING: &str = "synchronizing-and-waiting";

#[derive(Allocative, Debug)]
pub struct PreviousCommandDataInternal {
    pub external_and_local_configs: Vec<buck2_data::BuckconfigComponent>,
    pub sanitized_argv: Vec<String>,
    pub trace_id: TraceId,
}

#[derive(Allocative, Debug, Default)]
pub struct PreviousCommandData {
    pub data: Option<PreviousCommandDataInternal>,
}

impl PreviousCommandData {
    pub fn process_current_command(
        &mut self,
        event_dispatcher: EventDispatcher,
        current_external_and_local_configs: Vec<buck2_data::BuckconfigComponent>,
        current_sanitized_argv: Vec<String>,
        current_trace: TraceId,
    ) {
        if let Some(PreviousCommandDataInternal {
            external_and_local_configs: external_configs,
            sanitized_argv,
            trace_id,
        }) = self.data.as_ref()
        {
            if *current_external_and_local_configs != *external_configs {
                event_dispatcher.instant_event(buck2_data::PreviousCommandWithMismatchedConfig {
                    sanitized_argv: sanitized_argv.clone(),
                    trace_id: trace_id.to_string(),
                });
            }
        }

        self.data = Some(PreviousCommandDataInternal {
            external_and_local_configs: current_external_and_local_configs,
            sanitized_argv: current_sanitized_argv,
            trace_id: current_trace,
        });
    }
}

#[derive(Allocative, Debug, Default)]
pub struct LockedPreviousCommandData {
    pub data: Mutex<PreviousCommandData>,
}
impl LockedPreviousCommandData {
    pub fn new() -> Arc<Self> {
        Arc::new(LockedPreviousCommandData {
            data: Mutex::new(PreviousCommandData { data: None }),
        })
    }
}

#[async_trait]
pub trait ServerCommandContextTrait: Send + Sync {
    fn working_dir(&self) -> &ProjectRelativePath;

    fn working_dir_abs(&self) -> &AbsWorkingDir;

    fn command_name(&self) -> &str;

    fn isolation_prefix(&self) -> &FileName;

    fn project_root(&self) -> &ProjectRoot;

    fn cert_state(&self) -> CertState;

    fn materializer(&self) -> Arc<dyn Materializer>;

    /// exposes the dice for scoped access, but isn't intended to be callable by anyone
    async fn dice_accessor<'a>(
        &'a self,
        private: PrivateStruct,
    ) -> buck2_error::Result<DiceAccessor<'a>>;

    fn events(&self) -> &EventDispatcher;

    fn previous_command_data(&self) -> Arc<LockedPreviousCommandData>;

    fn stderr(&self) -> buck2_error::Result<StderrOutputGuard<'_>>;

    async fn request_metadata(&self) -> buck2_error::Result<HashMap<String, String>>;

    async fn config_metadata(
        &self,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<HashMap<String, String>>;

    fn log_target_pattern(
        &self,
        providers_patterns: &[ParsedPattern<ConfiguredProvidersPatternExtra>],
    );

    fn cancellation_context(&self) -> &CancellationContext;
}

pub struct PrivateStruct(());

pub struct DiceAccessor<'a> {
    pub dice_handler: Arc<ConcurrencyHandler>,
    pub setup: Box<dyn DiceUpdater + 'a>,
    pub is_nested_invocation: bool,
    pub sanitized_argv: Vec<String>,
    pub exit_when_different_state: bool,
    pub preemptible: PreemptibleWhen,
    pub build_signals: Box<dyn DeferredBuildSignals>,
}

#[async_trait]
pub trait ServerCommandDiceContext {
    async fn with_dice_ctx<'v, F, Fut, R>(&'v self, exec: F) -> buck2_error::Result<R>
    where
        F: FnOnce(&'v dyn ServerCommandContextTrait, DiceTransaction) -> Fut + Send,
        Fut: Future<Output = buck2_error::Result<R>> + Send,
        R: Send;

    async fn with_dice_ctx_maybe_exclusive<'v, F, Fut, R>(
        &'v self,
        exec: F,
        exclusive_cmd: Option<String>,
        command_start: Option<Instant>,
    ) -> buck2_error::Result<R>
    where
        F: FnOnce(&'v dyn ServerCommandContextTrait, DiceTransaction) -> Fut + Send,
        Fut: Future<Output = buck2_error::Result<R>> + Send,
        R: Send;
}

#[async_trait]
impl ServerCommandDiceContext for dyn ServerCommandContextTrait + '_ {
    /// Allows running a section of code that uses the shared DiceTransaction
    async fn with_dice_ctx<'v, F, Fut, R>(&'v self, exec: F) -> buck2_error::Result<R>
    where
        F: FnOnce(&'v dyn ServerCommandContextTrait, DiceTransaction) -> Fut + Send,
        Fut: Future<Output = buck2_error::Result<R>> + Send,
        R: Send,
    {
        self.with_dice_ctx_maybe_exclusive(exec, None, None).await
    }

    async fn with_dice_ctx_maybe_exclusive<'v, F, Fut, R>(
        &'v self,
        exec: F,
        exclusive_cmd: Option<String>,
        command_start: Option<Instant>,
    ) -> buck2_error::Result<R>
    where
        F: FnOnce(&'v dyn ServerCommandContextTrait, DiceTransaction) -> Fut + Send,
        Fut: Future<Output = buck2_error::Result<R>> + Send,
        R: Send,
    {
        let DiceAccessor {
            dice_handler,
            setup,
            is_nested_invocation,
            sanitized_argv,
            exit_when_different_state,
            preemptible,
            build_signals,
        } = self.dice_accessor(PrivateStruct(())).await?;

        let events = self.events().dupe();
        events
            .span_async(DiceCriticalSectionStart {}, async move {
                (
                    dice_handler
                        .enter(
                            self.events().dupe(),
                            &*setup,
                            |mut dice| async move {
                                let events = self.events().dupe();

                                let request_metadata = self.request_metadata().await?;
                                let config_metadata = self.config_metadata(&mut dice).await?;
                                events
                                    .span_async(
                                        CommandCriticalStart {
                                            metadata: config_metadata.clone(),
                                            dice_version: dice.equality_token().to_string(),
                                        },
                                        async move {
                                            let early_command_entries =
                                                command_start.map_or(vec![], |t| {
                                                    // The period of time between CommandStart and CommandCriticalStart is
                                                    // the time spent synchronizing changes and waiting for concurrent commands to
                                                    // finish.
                                                    vec![EarlyCommandEntry {
                                                        kind: TIME_SPENT_SYNCHRONIZING_AND_WAITING
                                                            .to_owned(),
                                                        duration: t.elapsed(),
                                                    }]
                                                });
                                            let res = buck2_build_signals::env::scope(
                                                build_signals,
                                                self.events().dupe(),
                                                dice.per_transaction_data()
                                                    .get_critical_path_backend(),
                                                BuildSignalsContext {
                                                    command_name: self.command_name().to_owned(),
                                                    metadata: request_metadata
                                                        .into_iter()
                                                        .chain(
                                                            config_metadata.iter().map(|(k, v)| {
                                                                (k.clone(), v.clone())
                                                            }),
                                                        )
                                                        .collect(),
                                                    isolation_prefix: self
                                                        .isolation_prefix()
                                                        .to_owned(),
                                                    early_command_entries,
                                                },
                                                || exec(self, dice),
                                            )
                                            .await;

                                            (
                                                res,
                                                CommandCriticalEnd {
                                                    metadata: config_metadata,
                                                },
                                            )
                                        },
                                    )
                                    .await
                            },
                            is_nested_invocation,
                            sanitized_argv,
                            exclusive_cmd,
                            exit_when_different_state,
                            self.cancellation_context(),
                            preemptible,
                            self.previous_command_data().into(),
                            self.project_root(),
                        )
                        .await,
                    DiceCriticalSectionEnd {},
                )
            })
            .await?
    }
}
