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
use async_trait::async_trait;
use buck2_common::ignores::ignore_set::IgnoreSet;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::is_open_source;
#[cfg(fbcode_build)]
use buck2_core::soft_error;
use buck2_error::buck2_error;
use buck2_error::BuckErrorContext;
use dice::DiceTransactionUpdater;

#[cfg(fbcode_build)]
use crate::edenfs::interface::EdenFsFileWatcher;
#[cfg(fbcode_build)]
use crate::edenfs::interface::EdenFsWatcherError;
use crate::fs_hash_crawler::FsHashCrawler;
use crate::mergebase::Mergebase;
use crate::notify::NotifyFileWatcher;
use crate::watchman::interface::WatchmanFileWatcher;

#[async_trait]
pub trait FileWatcher: Allocative + Send + Sync + 'static {
    async fn sync(
        &self,
        dice: DiceTransactionUpdater,
    ) -> buck2_error::Result<(DiceTransactionUpdater, Mergebase)>;
}

impl dyn FileWatcher {
    /// Create a new FileWatcher. Note that this is not async, since it's called during daemon
    /// startup and shouldn't be doing any work that could warrant suspending.
    pub fn new(
        fb: fbinit::FacebookInit,
        project_root: &ProjectRoot,
        root_config: &LegacyBuckConfig,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> buck2_error::Result<Arc<dyn FileWatcher>> {
        let default = if is_open_source() {
            "notify"
        } else {
            // TODO: On EdenFS mount use "edenfs", on non-EdenFS use "watchman"
            "watchman"
        };

        let _allow_unused = fb;

        match root_config
            .get(BuckconfigKeyRef {
                section: "buck2",
                property: "file_watcher",
            })
            .unwrap_or(default)
        {
            "watchman" => Ok(Arc::new(
                WatchmanFileWatcher::new(project_root.root(), root_config, cells, ignore_specs)
                    .buck_error_context("Creating watchman file watcher")?,
            )),
            "notify" => Ok(Arc::new(
                NotifyFileWatcher::new(project_root, cells, ignore_specs)
                    .buck_error_context("Creating notify file watcher")?,
            )),
            "fs_hash_crawler" => Ok(Arc::new(
                FsHashCrawler::new(project_root, cells, ignore_specs)
                    .buck_error_context("Creating fs_crawler file watcher")?,
            )),
            #[cfg(fbcode_build)]
            "edenfs" => {
                match EdenFsFileWatcher::new(
                    fb,
                    project_root,
                    root_config,
                    cells.clone(),
                    ignore_specs.clone(),
                ) {
                    Ok(edenfs) => Ok(Arc::new(edenfs)),
                    Err(EdenFsWatcherError::NoEden) => {
                        soft_error!(
                            "edenfs_watcher_creation_failure",
                            EdenFsWatcherError::NoEden.into()
                        )?;
                        // fallback to watchman if failed to create edenfs watcher
                        Ok(Arc::new(
                            WatchmanFileWatcher::new(
                                project_root.root(),
                                root_config,
                                cells,
                                ignore_specs,
                            )
                            .buck_error_context("Creating watchman file watcher")?,
                        ))
                    }
                    Err(e) => Err(e.into()),
                }
            }

            other => Err(buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "Invalid buck2.file_watcher: {}",
                other
            )),
        }
    }
}
