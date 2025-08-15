/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::ignores::ignore_set::IgnoreSet;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::cells::CellResolver;
use buck2_core::cells::name::CellName;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::is_open_source;
#[cfg(fbcode_build)]
use buck2_core::soft_error;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
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

        let watcher_conf = root_config
            .get(BuckconfigKeyRef {
                section: "buck2",
                property: "file_watcher",
            })
            .unwrap_or(default);

        let watcher_conf = if let "edenfs" = watcher_conf {
            #[cfg(fbcode_build)]
            match EdenFsFileWatcher::new(
                fb,
                project_root,
                root_config,
                cells.clone(),
                ignore_specs.clone(),
            ) {
                Ok(edenfs) => return Ok(Arc::new(edenfs)),
                Err(EdenFsWatcherError::NoEden) => {
                    soft_error!(
                        "edenfs_watcher_creation_failure",
                        EdenFsWatcherError::NoEden.into()
                    )?;
                    // fallback to watchman if failed to create edenfs watcher
                    "watchman"
                }
                Err(e) => return Err(e.into()),
            }
            #[cfg(not(fbcode_build))]
            default
        } else {
            watcher_conf
        };

        match watcher_conf {
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
            other => Err(buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "Invalid buck2.file_watcher: {}",
                other
            )),
        }
    }
}
