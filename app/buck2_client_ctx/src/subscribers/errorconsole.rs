/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;

use crate::subscribers::subscriber_unpack::UnpackingEventSubscriber;

/// This console is what is used for `--console none` and only prints errors.
///
/// It is also used as a part of simpleconsole's implementation.
pub struct ErrorConsole;

#[async_trait]
impl UnpackingEventSubscriber for ErrorConsole {
    async fn handle_command_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        if let buck2_cli_proto::CommandResult {
            result: Some(buck2_cli_proto::command_result::Result::Error(e)),
        } = result
        {
            crate::eprintln!("Command failed: ")?;
            for e in &e.errors {
                crate::eprintln!("{}", e.message)?;
            }
        }
        Ok(())
    }

    async fn handle_command_start(
        &mut self,
        _command: &buck2_data::CommandStart,
        _event: &buck2_events::BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_command_end(
        &mut self,
        _command: &buck2_data::CommandEnd,
        _event: &buck2_events::BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_action_execution_end(
        &mut self,
        _action: &buck2_data::ActionExecutionEnd,
        _event: &buck2_events::BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_file_watcher_end(
        &mut self,
        _watchman: &buck2_data::FileWatcherEnd,
        _event: &buck2_events::BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_console_message(
        &mut self,
        _message: &buck2_data::ConsoleMessage,
        _event: &buck2_events::BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_structured_error(
        &mut self,
        _err: &buck2_data::StructuredError,
        _event: &buck2_events::BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_test_discovery(
        &mut self,
        _test_info: &buck2_data::TestDiscovery,
        _event: &buck2_events::BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_test_result(
        &mut self,
        _result: &buck2_data::TestResult,
        _event: &buck2_events::BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_console_preferences(
        &mut self,
        _prefs: &buck2_data::ConsolePreferences,
        _event: &buck2_events::BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_error(&mut self, _error: &anyhow::Error) -> anyhow::Result<()> {
        Ok(())
    }

    async fn tick(&mut self, _tick: &super::subscriber::Tick) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_action_error(
        &mut self,
        _error: &buck2_data::ActionError,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_console_warning(
        &mut self,
        _message: &buck2_data::ConsoleWarning,
        _event: &buck2_events::BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}
