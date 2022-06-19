/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{collections::HashMap, sync::Arc};

use async_trait::async_trait;
use buck2_common::{file_ops::IgnoreSet, legacy_configs::LegacyBuckConfig};
use buck2_core::cells::{CellName, CellResolver};
use dice::Dice;
use events::dispatch::EventDispatcher;

use crate::paths::Paths;

mod watchman;

#[async_trait]
pub(crate) trait FileWatcher: Send + Sync + 'static {
    async fn sync(&self, dispatcher: &EventDispatcher) -> anyhow::Result<()>;
}

impl dyn FileWatcher {
    pub(crate) async fn new(
        paths: &Paths,
        root_config: &LegacyBuckConfig,
        dice: Arc<Dice>,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> anyhow::Result<Box<dyn FileWatcher>> {
        match root_config.get("buck2", "file_watcher") {
            Some("watchman") | None => Ok(box watchman::WatchmanFileWatcher::new(
                paths,
                root_config,
                dice,
                cells,
                ignore_specs,
            )
            .await?),
            Some(other) => Err(anyhow::anyhow!("Invalid buck2.file_watcher: {}", other)),
        }
    }
}
