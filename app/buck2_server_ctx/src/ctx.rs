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

use async_trait::async_trait;
use buck2_build_signals::BuildSignalsContext;
use buck2_build_signals::DeferredBuildSignals;
use buck2_build_signals::HasCriticalPathBackend;
use buck2_common::result::SharedResult;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::working_dir::WorkingDir;
use buck2_core::pattern::pattern_type::ConfiguredProvidersPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_data::CommandCriticalEnd;
use buck2_data::CommandCriticalStart;
use buck2_data::DiceCriticalSectionEnd;
use buck2_data::DiceCriticalSectionStart;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::materialize::materializer::Materializer;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use more_futures::cancellation::ExplicitCancellationContext;

use crate::concurrency::ConcurrencyHandler;
use crate::concurrency::DiceDataProvider;
use crate::concurrency::DiceUpdater;
use crate::stderr_output_guard::StderrOutputGuard;

#[async_trait]
pub trait ServerCommandContextTrait: Send + Sync {
    fn working_dir(&self) -> &ProjectRelativePath;

    fn working_dir_abs(&self) -> &WorkingDir;

    fn command_name(&self) -> &str;

    fn isolation_prefix(&self) -> &FileName;

    fn project_root(&self) -> &ProjectRoot;

    fn materializer(&self) -> Arc<dyn Materializer>;

    /// exposes the dice for scoped access, but isn't intended to be callable by anyone
    async fn dice_accessor(&self, private: PrivateStruct) -> SharedResult<DiceAccessor>;

    fn events(&self) -> &EventDispatcher;

    fn stderr(&self) -> anyhow::Result<StderrOutputGuard<'_>>;

    async fn request_metadata(&self) -> anyhow::Result<HashMap<String, String>>;

    async fn config_metadata(
        &self,
        ctx: &DiceComputations,
    ) -> anyhow::Result<HashMap<String, String>>;

    fn log_target_pattern(
        &self,
        providers_patterns: &[ParsedPattern<ConfiguredProvidersPatternExtra>],
    );

    fn cancellation_context(&self) -> &ExplicitCancellationContext;
}

pub struct PrivateStruct(());

pub struct DiceAccessor {
    pub dice_handler: ConcurrencyHandler,
    pub data: Box<dyn DiceDataProvider>,
    pub setup: Box<dyn DiceUpdater>,
    pub is_nested_invocation: bool,
    pub sanitized_argv: Vec<String>,
    pub exit_when_different_state: bool,
    pub build_signals: Box<dyn DeferredBuildSignals>,
}

#[async_trait]
pub trait ServerCommandDiceContext {
    async fn with_dice_ctx<'v, F, Fut, R>(&'v self, exec: F) -> anyhow::Result<R>
    where
        F: FnOnce(&'v dyn ServerCommandContextTrait, DiceTransaction) -> Fut + Send,
        Fut: Future<Output = anyhow::Result<R>> + Send,
        R: Send;

    async fn with_dice_ctx_maybe_exclusive<'v, F, Fut, R>(
        &'v self,
        exec: F,
        exclusive_cmd: Option<String>,
    ) -> anyhow::Result<R>
    where
        F: FnOnce(&'v dyn ServerCommandContextTrait, DiceTransaction) -> Fut + Send,
        Fut: Future<Output = anyhow::Result<R>> + Send,
        R: Send;
}

#[async_trait]
impl ServerCommandDiceContext for dyn ServerCommandContextTrait + '_ {
    /// Allows running a section of code that uses the shared DiceTransaction
    async fn with_dice_ctx<'v, F, Fut, R>(&'v self, exec: F) -> anyhow::Result<R>
    where
        F: FnOnce(&'v dyn ServerCommandContextTrait, DiceTransaction) -> Fut + Send,
        Fut: Future<Output = anyhow::Result<R>> + Send,
        R: Send,
    {
        self.with_dice_ctx_maybe_exclusive(exec, None).await
    }

    async fn with_dice_ctx_maybe_exclusive<'v, F, Fut, R>(
        &'v self,
        exec: F,
        exclusive_cmd: Option<String>,
    ) -> anyhow::Result<R>
    where
        F: FnOnce(&'v dyn ServerCommandContextTrait, DiceTransaction) -> Fut + Send,
        Fut: Future<Output = anyhow::Result<R>> + Send,
        R: Send,
    {
        let DiceAccessor {
            dice_handler,
            data,
            setup,
            is_nested_invocation,
            sanitized_argv,
            exit_when_different_state,
            build_signals,
        } = self.dice_accessor(PrivateStruct(())).await?;

        let events = self.events().dupe();
        events
            .span_async(DiceCriticalSectionStart {}, async move {
                (
                    dice_handler
                        .enter(
                            self.events().dupe(),
                            &*data,
                            &*setup,
                            |dice| async move {
                                let events = self.events().dupe();

                                let request_metadata = self.request_metadata().await?;
                                let config_metadata = self.config_metadata(&dice).await?;

                                events
                                    .span_async(
                                        CommandCriticalStart {
                                            metadata: config_metadata.clone(),
                                            dice_version: dice.equality_token().to_string(),
                                        },
                                        async move {
                                            let res = buck2_build_signals::scope(
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
                        )
                        .await,
                    DiceCriticalSectionEnd {},
                )
            })
            .await?
    }
}
