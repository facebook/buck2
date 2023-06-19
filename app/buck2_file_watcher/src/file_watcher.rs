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

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_common::ignores::ignore_set::IgnoreSet;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::is_open_source;
use dice::DiceTransactionUpdater;

use crate::mergebase::Mergebase;
use crate::notify::NotifyFileWatcher;
use crate::watchman::interface::WatchmanFileWatcher;

#[async_trait]
pub trait FileWatcher: Allocative + Send + Sync + 'static {
    async fn sync(
        &self,
        dice: DiceTransactionUpdater,
    ) -> anyhow::Result<(DiceTransactionUpdater, Mergebase)>;
}

impl dyn FileWatcher {
    /// Create a new FileWatcher. Note that this is not async, since it's called during daemon
    /// startup and shouldn't be doing any work that could warrant suspending.
    pub fn new(
        project_root: &ProjectRoot,
        root_config: &LegacyBuckConfig,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> anyhow::Result<Arc<dyn FileWatcher>> {
        let default = if is_open_source() {
            "notify"
        } else {
            "watchman"
        };

        match root_config.get("buck2", "file_watcher").unwrap_or(default) {
            "watchman" => Ok(Arc::new(
                WatchmanFileWatcher::new(project_root.root(), root_config, cells, ignore_specs)
                    .context("Creating watchman file watcher")?,
            )),
            "notify" => Ok(Arc::new(
                NotifyFileWatcher::new(project_root, cells, ignore_specs)
                    .context("Creating notify file watcher")?,
            )),
            other => Err(anyhow::anyhow!("Invalid buck2.file_watcher: {}", other)),
        }
    }
}
