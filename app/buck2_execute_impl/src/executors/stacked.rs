/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use async_trait::async_trait;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::prepared::PreparedCommand;
use buck2_execute::execute::prepared::PreparedCommandExecutor;
use buck2_execute::execute::prepared::PreparedCommandOptionalExecutor;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_futures::cancellation::CancellationContext;

pub struct StackedExecutor<O, F> {
    pub optional1: O,
    pub optional2: O,
    pub fallback: F,
}

#[async_trait]
impl<O, F> PreparedCommandExecutor for StackedExecutor<O, F>
where
    O: PreparedCommandOptionalExecutor,
    F: PreparedCommandExecutor,
{
    async fn exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
        cancellations: &CancellationContext,
    ) -> CommandExecutionResult {
        let manager = self
            .optional1
            .maybe_execute(command, manager, cancellations)
            .await?; // This actually returns if we get a response.
        let manager = self
            .optional2
            .maybe_execute(command, manager, cancellations)
            .await?; // This actually returns if we get a response.

        self.fallback
            .exec_cmd(command, manager, cancellations)
            .await
    }

    fn is_local_execution_possible(&self, executor_preference: ExecutorPreference) -> bool {
        // Apparently it's never possible on PreparedCommandOptionalExecutor, so we only ask the
        // fallback.
        self.fallback
            .is_local_execution_possible(executor_preference)
    }
}
