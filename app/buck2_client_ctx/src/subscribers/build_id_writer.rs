/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_events::BuckEvent;

use crate::subscribers::subscriber::EventSubscriber;

pub struct BuildIdWriter {
    path: AbsPathBuf,
}

impl BuildIdWriter {
    pub fn new(path: AbsPathBuf) -> Self {
        Self { path }
    }
}

#[async_trait]
impl EventSubscriber for BuildIdWriter {
    async fn handle_events(&mut self, events: &[Arc<BuckEvent>]) -> anyhow::Result<()> {
        for event in events {
            if event.command_start()?.is_some() {
                tokio::fs::write(&self.path, event.trace_id()?.to_string()).await?;
            }
        }
        Ok(())
    }
}
