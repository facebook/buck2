/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_common::file_ops::IgnoreSet;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_core::cells::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::AbsPath;
use dice::DiceTransaction;

mod watchman;

#[async_trait]
pub trait FileWatcher: Send + Sync + 'static {
    async fn sync(&self, dice: DiceTransaction) -> anyhow::Result<DiceTransaction>;
}

impl dyn FileWatcher {
    /// Create a new FileWatcher. Note that this is not async, since it's called during daemon
    /// startup and shouldn't be doing any work that could warrant suspending.
    pub fn new(
        project_root: &AbsPath,
        root_config: &LegacyBuckConfig,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> anyhow::Result<Arc<dyn FileWatcher>> {
        match root_config.get("buck2", "file_watcher") {
            Some("watchman") | None => Ok(Arc::new(watchman::WatchmanFileWatcher::new(
                project_root,
                root_config,
                cells,
                ignore_specs,
            )?)),
            Some(other) => Err(anyhow::anyhow!("Invalid buck2.file_watcher: {}", other)),
        }
    }
}
