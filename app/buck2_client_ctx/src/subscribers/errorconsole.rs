/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;

use crate::exit_result::ExitResult;
use crate::subscribers::subscriber::EventSubscriber;

/// This console is what is used for `--console none` and only prints errors.
///
/// It is also used as a part of simpleconsole's implementation.
pub struct ErrorConsole;

#[async_trait]
impl EventSubscriber for ErrorConsole {
    async fn handle_command_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> buck2_error::Result<()> {
        if let buck2_cli_proto::CommandResult {
            result: Some(buck2_cli_proto::command_result::Result::Error(error)),
        } = result
        {
            print_error(&error.message)?;
        }

        Ok(())
    }

    async fn handle_exit_result(&mut self, result: &mut ExitResult) {
        if let Some(error) = result.get_error() {
            result.handle_console_write(print_error(&format!("{:?}", error)));
        }
    }
}

fn print_error(error: &String) -> buck2_error::Result<()> {
    crate::eprintln!("Command failed: ")?;
    crate::eprintln!("{}", error)?;
    Ok(())
}
