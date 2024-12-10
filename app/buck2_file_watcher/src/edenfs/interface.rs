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
use buck2_core::buck2_env;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project::ProjectRoot;
use buck2_eden::connection::EdenConnectionManager;
use buck2_events::dispatch::span_async;
use dice::DiceTransactionUpdater;
use edenfs::ChangesSinceV2Params;
use edenfs::JournalPosition;
use fbinit::FacebookInit;
use tokio::sync::RwLock;
use tokio::sync::Semaphore;

use crate::file_watcher::FileWatcher;
use crate::mergebase::Mergebase;
use crate::stats::FileWatcherStats;

#[derive(Allocative)]
pub(crate) struct EdenFsFileWatcher {
    manager: EdenConnectionManager,
    mount_point: Vec<u8>,
    #[allocative(skip)]
    position: RwLock<JournalPosition>,
    cells: CellResolver,
    ignore_specs: HashMap<CellName, IgnoreSet>,
}

impl EdenFsFileWatcher {
    pub(crate) fn new(
        fb: FacebookInit,
        root: &ProjectRoot,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> buck2_error::Result<Self> {
        let eden_semaphore =
            buck2_env!("BUCK2_EDEN_SEMAPHORE", type=usize, default=2048, applicability=internal)?;

        let manager = EdenConnectionManager::new(fb, root, Semaphore::new(eden_semaphore))?
            .expect("Failed to connect to EdenFS");
        let mount_point = manager.get_mount_point();

        Ok(Self {
            manager,
            mount_point,
            position: RwLock::new(JournalPosition::default()),
            cells,
            ignore_specs,
        })
    }

    async fn update(
        &self,
        dice: DiceTransactionUpdater,
    ) -> buck2_error::Result<(buck2_data::FileWatcherStats, DiceTransactionUpdater)> {
        let position = self.position.read().await.clone();
        let changes_since_v2_params = ChangesSinceV2Params {
            mountPoint: self.mount_point.clone(),
            fromPosition: position,
            includeVCSRoots: Some(false),
            includedRoots: None,
            excludedRoots: None,
            ..Default::default()
        };
        let changes = self
            .manager
            .with_eden(|eden| eden.changesSinceV2(&changes_since_v2_params))
            .await?;
        let mut position = self.position.write().await;
        *position = changes.toPosition;
        let stats = FileWatcherStats::new(Default::default(), 0);
        Ok((stats.finish(), dice))
    }
}

#[async_trait]
impl FileWatcher for EdenFsFileWatcher {
    async fn sync(
        &self,
        dice: DiceTransactionUpdater,
    ) -> buck2_error::Result<(DiceTransactionUpdater, Mergebase)> {
        span_async(
            buck2_data::FileWatcherStart {
                provider: buck2_data::FileWatcherProvider::EdenFs as i32,
            },
            async {
                let (stats, res) = match self.update(dice).await {
                    Ok((stats, dice)) => {
                        let mergebase = Mergebase(Arc::new(stats.branched_from_revision.clone()));
                        ((Some(stats)), Ok((dice, mergebase)))
                    }
                    Err(e) => (None, Err(e)),
                };
                (res, buck2_data::FileWatcherEnd { stats })
            },
        )
        .await
    }
}
