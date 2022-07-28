/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use async_trait::async_trait;
use buck2_build_api::context::HasBuildContextData;
use buck2_build_api::context::SetBuildContextData;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::file_ops::IgnoreSet;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_core::cells::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_interpreter::dice::interpreter_setup::setup_interpreter;
use buck2_interpreter::dice::starlark_profiler::GetStarlarkProfilerInstrumentation;
use buck2_interpreter::dice::starlark_types::GetDisableStarlarkTypes;
use buck2_interpreter::dice::HasInterpreterContext;
use dice::DiceTransaction;
use events::dispatch::EventDispatcher;
use tracing::info;
use tracing::warn;
use watchman_client::expr::Expr;
use watchman_client::prelude::Connector;
use watchman_client::prelude::FileType;

use crate::daemon::server::file_watcher::FileWatcher;
use crate::paths::Paths;
use crate::watchman::SyncableQuery;
use crate::watchman::SyncableQueryProcessor;
use crate::watchman::WatchmanEvent;
use crate::watchman::WatchmanEventType;
use crate::watchman::WatchmanKind;

const MAX_WATCHMAN_MESSAGES: u64 = 3;

pub(crate) struct WatchmanQueryProcessor {
    pub cells: CellResolver,
    pub ignore_specs: HashMap<CellName, IgnoreSet>,
}

impl WatchmanQueryProcessor {
    pub(crate) async fn process_events_impl(
        &self,
        ctx: DiceTransaction,
        events: Vec<WatchmanEvent>,
    ) -> anyhow::Result<(buck2_data::WatchmanStats, DiceTransaction)> {
        let mut stats = buck2_data::WatchmanStats::default();

        let handler = ctx.get_file_change_handler();
        let mut iter = events.into_iter();

        let mut messages = 0;

        while let Some(ev) = iter.next() {
            let cell_path = self
                .cells
                .get_cell_path(&ProjectRelativePath::new(&ev.path)?)?;
            let ignore = self
                .ignore_specs
                .get(cell_path.cell())
                .expect("unexpected cell name mismatch")
                .is_match(cell_path.path());

            info!("Watchman: {:?} (ignore = {})", ev, ignore);

            stats.events_total += 1;

            if !ignore {
                stats.events_processed += 1;

                if messages < MAX_WATCHMAN_MESSAGES {
                    messages += 1;
                    if messages == MAX_WATCHMAN_MESSAGES && iter.len() > 0 {
                        eprintln!("{} additional file changes", iter.len() + 1);
                    } else {
                        eprintln!("File changed: {}", cell_path);
                    }
                }
                match (ev.kind, ev.event) {
                    (WatchmanKind::File, WatchmanEventType::Modify) => {
                        handler.file_changed(cell_path);
                    }
                    (WatchmanKind::File, WatchmanEventType::Create) => {
                        handler.file_added(cell_path);
                    }
                    (WatchmanKind::File, WatchmanEventType::Delete) => {
                        handler.file_removed(cell_path);
                    }
                    (WatchmanKind::Directory, WatchmanEventType::Modify) => {
                        // We can safely ignore this, as it corresponds to files being added or removed,
                        // but there are always file add/remove notifications sent too.
                        // See https://fb.workplace.com/groups/watchman.users/permalink/2858842194433249
                    }
                    (WatchmanKind::Directory, WatchmanEventType::Create) => {
                        handler.dir_added(cell_path);
                    }
                    (WatchmanKind::Directory, WatchmanEventType::Delete) => {
                        handler.dir_removed(cell_path);
                    }
                    (WatchmanKind::Symlink, typ) => {
                        // We don't really support symlinks in the source, but better than a panic.
                        // Just pretend they are a file, so invalidate directory listing and contents.
                        warn!(
                            "Symlink change detected (source symlinks are not supported): {}",
                            cell_path
                        );
                        match typ {
                            WatchmanEventType::Modify => handler.file_changed(cell_path),
                            WatchmanEventType::Create => handler.file_added(cell_path),
                            WatchmanEventType::Delete => handler.file_removed(cell_path),
                        }
                    }
                }
            }
        }
        Ok((stats, ctx))
    }
}

#[async_trait]
impl SyncableQueryProcessor for WatchmanQueryProcessor {
    type Output = buck2_data::WatchmanStats;
    type Payload = DiceTransaction;

    async fn process_events(
        &self,
        dice: DiceTransaction,
        events: Vec<WatchmanEvent>,
    ) -> anyhow::Result<(Self::Output, DiceTransaction)> {
        self.process_events_impl(dice, events).await
    }

    async fn on_fresh_instance(
        &self,
        ctx: DiceTransaction,
    ) -> anyhow::Result<(Self::Output, DiceTransaction)> {
        eprintln!("watchman fresh instance event, clearing cache");

        let buck_out_path = ctx.get_buck_out_path().await?;
        let cells = ctx.get_cell_resolver().await?;
        let configuror = ctx.get_interpreter_configuror().await?;
        let legacy_configs = ctx.get_legacy_configs().await?;
        let starlark_profiler_instrumentation_override =
            ctx.get_starlark_profiler_instrumentation_override().await?;
        let disable_starlark_types = ctx.get_disable_starlark_types().await?;

        buck2_build_api::actions::run::dep_files::flush_dep_files();

        // TODO(cjhopman): could probably get away with just invalidating all fs things, but that's not supported.
        // Dropping the entire DICE map can be somewhat computationally expensive as there
        // are a lot of destructors to run. On the other hand, we don't have to wait for
        // it. So, we just send it off to its own thread.
        let (ctx, map) = ctx.unstable_take();
        std::thread::spawn(|| drop(map));

        ctx.set_buck_out_path(Some((*buck_out_path).to_buf()));
        setup_interpreter(
            &ctx,
            cells,
            configuror,
            legacy_configs,
            starlark_profiler_instrumentation_override,
            disable_starlark_types,
        );
        Ok((
            buck2_data::WatchmanStats {
                fresh_instance: true,
                ..Default::default()
            },
            ctx,
        ))
    }
}

pub(crate) struct WatchmanFileWatcher {
    query: SyncableQuery<buck2_data::WatchmanStats, DiceTransaction>,
}

/// The watchman query is constructed once on daemon startup. It is an unfiltered watchman query
/// over the entire project (though, like all watchman queries, still filtered by the
/// .watchmanconfig itself). Before any new computation request is started, it will be synced to
/// ensure that any recent changes are flushed and visible to the computation.
impl WatchmanFileWatcher {
    pub(crate) fn new(
        paths: &Paths,
        root_config: &LegacyBuckConfig,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> anyhow::Result<Self> {
        let watchman_merge_base = root_config
            .get("project", "watchman_merge_base")
            .map(|s| s.to_owned());

        let query = SyncableQuery::new(
            Connector::new(),
            paths.project_root(),
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
    async fn sync(
        &self,
        dice: DiceTransaction,
        dispatcher: &EventDispatcher,
    ) -> anyhow::Result<DiceTransaction> {
        dispatcher
            .span_async(buck2_data::WatchmanStart {}, async {
                let (stats, res) = match self.query.sync(dice).await {
                    Ok((stats, dice)) => ((Some(stats)), Ok(dice)),
                    Err(e) => (None, Err(e)),
                };
                (res, buck2_data::WatchmanEnd { stats })
            })
            .await
    }
}
