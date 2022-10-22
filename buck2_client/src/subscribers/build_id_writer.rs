/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;

use async_trait::async_trait;
use buck2_events::BuckEvent;

use crate::subscribers::subscriber::EventSubscriber;

pub struct BuildIdWriter {
    path: PathBuf,
}

impl BuildIdWriter {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }
}

#[async_trait]
impl EventSubscriber for BuildIdWriter {
    async fn handle_command_start(
        &mut self,
        _command: &buck2_data::CommandStart,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(tokio::fs::write(&self.path, event.trace_id().to_string()).await?)
    }
}
