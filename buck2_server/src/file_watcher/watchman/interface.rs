/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::path::Path;

use allocative::Allocative;
use anyhow::Context as _;
use async_trait::async_trait;
use buck2_common::dice::file_ops::FileChangeHandler;
use buck2_common::dice::file_ops::FileChangeTracker;
use buck2_common::file_ops::IgnoreSet;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_core::cells::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_events::dispatch::span_async;
use dice::DiceTransaction;
use tracing::info;
use tracing::warn;
use watchman_client::expr::Expr;
use watchman_client::prelude::Connector;
use watchman_client::prelude::FileType;

use crate::file_watcher::watchman::core::SyncableQuery;
use crate::file_watcher::watchman::core::SyncableQueryProcessor;
use crate::file_watcher::watchman::core::WatchmanEvent;
use crate::file_watcher::watchman::core::WatchmanEventType;
use crate::file_watcher::watchman::core::WatchmanKind;
use crate::file_watcher::FileWatcher;

const MAX_WATCHMAN_MESSAGES: usize = 3;
const MAX_FILE_CHANGE_RECORDS: usize = 850;

struct WatchmanQueryProcessor {
    cells: CellResolver,
    ignore_specs: HashMap<CellName, IgnoreSet>,
}

/// Used in process_one_change
#[derive(Debug)]
enum ChangeEvent<'a> {
    Watchman(&'a WatchmanEvent),
    SyntheticDirectoryChange,
}

impl WatchmanQueryProcessor {
    async fn process_events_impl(
        &self,
        ctx: DiceTransaction,
        events: Vec<WatchmanEvent>,
        mergebase: &Option<String>,
    ) -> anyhow::Result<(buck2_data::FileWatcherStats, DiceTransaction)> {
        let mut stats = buck2_data::FileWatcherStats {
            branched_from_revision: mergebase.clone(),
            ..Default::default()
        };

        let mut handler = FileChangeTracker::new();
        let mut iter = events.into_iter();

        let mut file_changes = if iter.len() > MAX_FILE_CHANGE_RECORDS {
            buck2_data::FileChanges {
                data: Some(buck2_data::file_changes::Data::NoRecordReason(format!(
                    "Too many files changed ({}, max {})",
                    iter.len(),
                    MAX_FILE_CHANGE_RECORDS
                ))),
            }
        } else {
            buck2_data::FileChanges {
                data: Some(buck2_data::file_changes::Data::Records(
                    buck2_data::FileWatcherEvents { events: vec![] },
                )),
            }
        };

        {
            let mut messages = 0;

            let mut file_changes = match file_changes.data {
                Some(buck2_data::file_changes::Data::Records(buck2_data::FileWatcherEvents {
                    ref mut events,
                })) => Some(events),
                _ => None,
            };

            while let Some(ev) = iter.next() {
                // If the path is invalid, then walk up all the way until you find a valid dir to
                // invalidate listings. We don't need to invalidate the file itself, as we can't
                // read invalid files.

                let (path, event) = match ProjectRelativePath::new(&ev.path) {
                    Ok(path) => (path, ChangeEvent::Watchman(&ev)),
                    Err(_) => {
                        // If we error out here then we might miss other changes. This seems like
                        // it shouldn't happen, since the empty path should always be a valid path.
                        let path = find_first_valid_parent(&ev.path)
                            .with_context(|| {
                                format!("Invalid path had no valid parent: `{}`", ev.path.display())
                            })
                            .unwrap();

                        (path, ChangeEvent::SyntheticDirectoryChange)
                    }
                };

                self.process_one_change(
                    path,
                    event,
                    &mut handler,
                    &mut stats,
                    file_changes.as_deref_mut(),
                    &mut messages,
                    iter.len(),
                )?;
            }
        }

        stats.file_changes_since_last_build = Some(file_changes);
        handler.write_to_dice(&ctx)?;

        Ok((stats, ctx))
    }

    fn process_one_change(
        &self,
        path: &ProjectRelativePath,
        ev: ChangeEvent<'_>,
        handler: &mut FileChangeTracker,
        stats: &mut buck2_data::FileWatcherStats,
        file_changes: Option<&mut Vec<buck2_data::FileWatcherEvent>>,
        messages: &mut usize,
        events_remaining: usize,
    ) -> anyhow::Result<()> {
        let cell_path = self.cells.get_cell_path(path)?;

        let ignore = self
            .ignore_specs
            .get(cell_path.cell())
            .expect("unexpected cell name mismatch")
            .is_match(cell_path.path());

        info!("Watchman: {:?} (ignore = {})", ev, ignore);
        stats.events_total += 1;

        if !ignore {
            stats.events_processed += 1;
            let mut event_for_log = buck2_data::FileWatcherEvent {
                path: cell_path.to_string(),
                ..Default::default()
            };

            if *messages < MAX_WATCHMAN_MESSAGES {
                *messages += 1;
                if *messages == MAX_WATCHMAN_MESSAGES && events_remaining > 0 {
                    eprintln!("{} additional file changes", events_remaining + 1);
                } else {
                    eprintln!("File changed: {}", cell_path);
                }
            }

            let log_kind;
            let log_event;

            match ev {
                ChangeEvent::Watchman(ev) => {
                    match (&ev.kind, &ev.event) {
                        (WatchmanKind::File, typ) => {
                            log_kind = buck2_data::FileWatcherKind::File;
                            match typ {
                                WatchmanEventType::Modify => {
                                    handler.file_changed(cell_path);
                                    log_event = buck2_data::FileWatcherEventType::Modify;
                                }
                                WatchmanEventType::Create => {
                                    handler.file_added(cell_path);
                                    log_event = buck2_data::FileWatcherEventType::Create;
                                }
                                WatchmanEventType::Delete => {
                                    handler.file_removed(cell_path);
                                    log_event = buck2_data::FileWatcherEventType::Delete;
                                }
                            }
                        }
                        (WatchmanKind::Directory, typ) => {
                            log_kind = buck2_data::FileWatcherKind::Directory;
                            match typ {
                                WatchmanEventType::Modify => {
                                    // We can safely ignore this, as it corresponds to files being added or removed,
                                    // but there are always file add/remove notifications sent too.
                                    // See https://fb.workplace.com/groups/watchman.users/permalink/2858842194433249
                                    return Ok(());
                                }
                                WatchmanEventType::Create => {
                                    handler.dir_added(cell_path);
                                    log_event = buck2_data::FileWatcherEventType::Create;
                                }
                                WatchmanEventType::Delete => {
                                    handler.dir_removed(cell_path);
                                    log_event = buck2_data::FileWatcherEventType::Delete;
                                }
                            }
                        }
                        (WatchmanKind::Symlink, typ) => {
                            log_kind = buck2_data::FileWatcherKind::Symlink;
                            match typ {
                                WatchmanEventType::Modify => {
                                    handler.file_changed(cell_path);
                                    log_event = buck2_data::FileWatcherEventType::Modify;
                                }
                                WatchmanEventType::Create => {
                                    warn!(
                                        "New symlink detected (source symlinks are not supported): {}",
                                        cell_path
                                    );
                                    handler.file_added(cell_path);
                                    log_event = buck2_data::FileWatcherEventType::Create;
                                }
                                WatchmanEventType::Delete => {
                                    handler.file_removed(cell_path);
                                    log_event = buck2_data::FileWatcherEventType::Delete;
                                }
                            }
                        }
                    }
                }
                ChangeEvent::SyntheticDirectoryChange => {
                    log_kind = buck2_data::FileWatcherKind::Directory;
                    log_event = buck2_data::FileWatcherEventType::Modify;
                    handler.dir_changed(cell_path);
                }
            };

            if let Some(file_changes) = file_changes {
                event_for_log.event = log_event as i32;
                event_for_log.kind = log_kind as i32;
                file_changes.push(event_for_log);
            }
        }

        Ok(())
    }
}

fn find_first_valid_parent(mut path: &Path) -> Option<&ProjectRelativePath> {
    loop {
        path = path.parent()?;

        match ProjectRelativePath::new(path) {
            Ok(path) => return Some(path),
            Err(_) => {}
        }
    }
}

#[async_trait]
impl SyncableQueryProcessor for WatchmanQueryProcessor {
    type Output = buck2_data::FileWatcherStats;
    type Payload = DiceTransaction;

    async fn process_events(
        &self,
        dice: DiceTransaction,
        events: Vec<WatchmanEvent>,
        mergebase: &Option<String>,
    ) -> anyhow::Result<(Self::Output, DiceTransaction)> {
        self.process_events_impl(dice, events, mergebase).await
    }

    async fn on_fresh_instance(
        &self,
        ctx: DiceTransaction,
        mergebase: &Option<String>,
    ) -> anyhow::Result<(Self::Output, DiceTransaction)> {
        eprintln!("watchman fresh instance event, clearing cache");

        buck2_build_api::actions::impls::run::dep_files::flush_dep_files();

        // TODO(cjhopman): could probably get away with just invalidating all fs things, but that's not supported.
        // Dropping the entire DICE map can be somewhat computationally expensive as there
        // are a lot of destructors to run. On the other hand, we don't have to wait for
        // it. So, we just send it off to its own thread.
        let (ctx, map) = ctx.unstable_take();
        std::thread::spawn(|| drop(map));

        Ok((
            buck2_data::FileWatcherStats {
                fresh_instance: true,
                branched_from_revision: mergebase.clone(),
                file_changes_since_last_build: Some(buck2_data::FileChanges {
                    data: Some(buck2_data::file_changes::Data::NoRecordReason(
                        "Fresh instance".to_owned(),
                    )),
                }),
                ..Default::default()
            },
            ctx,
        ))
    }
}

#[derive(Allocative)]
pub(crate) struct WatchmanFileWatcher {
    #[allocative(skip)]
    query: SyncableQuery<buck2_data::FileWatcherStats, DiceTransaction>,
}

/// The watchman query is constructed once on daemon startup. It is an unfiltered watchman query
/// over the entire project (though, like all watchman queries, still filtered by the
/// .watchmanconfig itself). Before any new computation request is started, it will be synced to
/// ensure that any recent changes are flushed and visible to the computation.
impl WatchmanFileWatcher {
    pub(crate) fn new(
        project_root: &AbsNormPath,
        root_config: &LegacyBuckConfig,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> anyhow::Result<Self> {
        let watchman_merge_base = root_config
            .get("project", "watchman_merge_base")
            .map(|s| s.to_owned());

        let query = SyncableQuery::new(
            Connector::new(),
            project_root,
            Expr::Any(vec![
                Expr::FileType(FileType::Regular),
                Expr::FileType(FileType::Directory),
                Expr::FileType(FileType::Symlink),
            ]),
            box WatchmanQueryProcessor {
                cells,
                ignore_specs,
            },
            watchman_merge_base,
        )?;

        Ok(Self { query })
    }
}

#[async_trait]
impl FileWatcher for WatchmanFileWatcher {
    async fn sync(&self, dice: DiceTransaction) -> anyhow::Result<DiceTransaction> {
        span_async(buck2_data::FileWatcherStart {}, async {
            let (stats, res) = match self.query.sync(dice).await {
                Ok((stats, dice)) => ((Some(stats)), Ok(dice)),
                Err(e) => (None, Err(e)),
            };
            (res, buck2_data::FileWatcherEnd { stats })
        })
        .await
    }
}
