/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_test_api::data::ExternalRunnerSpec;
use buck2_test_api::protocol::TestExecutor;
use futures::channel::mpsc::UnboundedSender;

pub type SpecSender = UnboundedSender<ExternalRunnerSpec>;

pub struct Buck2TestExecutor {
    pub sender: SpecSender,
}

impl Buck2TestExecutor {
    pub fn new(sender: SpecSender) -> Self {
        Self { sender }
    }
}

#[async_trait::async_trait]
impl TestExecutor for Buck2TestExecutor {
    async fn external_runner_spec(&self, spec: ExternalRunnerSpec) -> anyhow::Result<()> {
        self.sender
            .clone()
            .start_send(spec)
            .expect("Sending to not fail if all core invariants are held.");
        Ok(())
    }

    async fn end_of_test_requests(&self) -> anyhow::Result<()> {
        // This ensures that all senders are dropped so the receiver can terminate
        self.sender.close_channel();
        Ok(())
    }
}
