/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;

use crate::subscribers::subscriber::EventSubscriber;

/// The only purpose of this subscriber is to print out stderr/stdout
/// during buckd connection bootstrapping.
pub struct StdoutStderrForwarder;

#[async_trait]
impl EventSubscriber for StdoutStderrForwarder {
    async fn handle_tailer_stderr(&mut self, stderr: &str) -> anyhow::Result<()> {
        crate::eprintln!("{}", stderr)?;
        Ok(())
    }
    async fn handle_tailer_stdout(&mut self, raw_output: &str) -> anyhow::Result<()> {
        crate::eprint!("{}", raw_output)?;
        Ok(())
    }
}
