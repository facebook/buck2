/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Daemon-only panic hooks.
//!
//! This module sets up a panic hook to send the panic message to open CLIs.

use std::env::temp_dir;
use std::panic;
use std::panic::PanicInfo;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use std::time::SystemTime;

use buck2_events::trace::TraceId;
use cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use once_cell::sync::OnceCell;

pub trait DaemonStatePanicDiceDump: Send + Sync + 'static {
    fn dice_dump(&self, path: &Path, format: DiceDumpFormat) -> anyhow::Result<()>;
}

fn get_panic_dump_dir() -> PathBuf {
    temp_dir().join("buck2-dumps")
}

async fn remove_old_panic_dumps() -> anyhow::Result<()> {
    const MAX_PANIC_AGE: Duration = Duration::from_secs(60 * 60 * 24); // 1 day
    let dump_dir = get_panic_dump_dir();
    let now = SystemTime::now();
    if let Ok(dir_result) = std::fs::read_dir(dump_dir) {
        let dumps = dir_result.filter_map(Result::ok).collect::<Vec<_>>();
        for record in dumps {
            let metadata = record.metadata()?;
            if now.duration_since(metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH))?
                > MAX_PANIC_AGE
            {
                match metadata.is_dir() {
                    true => std::fs::remove_dir_all(record.path()),
                    false => std::fs::remove_file(record.path()),
                }
                .ok();
            }
        }
    }
    Ok(())
}

/// Initializes the panic hook.
pub fn initialize(daemon_state: Arc<dyn DaemonStatePanicDiceDump>) {
    let hook = panic::take_hook();
    panic::set_hook(box move |info| {
        daemon_panic_hook(&daemon_state, info);
        hook(info);
    });
    tokio::spawn(remove_old_panic_dumps());
}

/// If DICE panics while holding a write lock, that lock will be poisoned and
/// the DICE dump will panic when it tries to acquire that lock.
/// This cell prevents a circular set of panics if this happens.
static ALREADY_DUMPED_DICE: OnceCell<()> = OnceCell::new();

fn daemon_panic_hook(daemon_state: &Arc<dyn DaemonStatePanicDiceDump>, info: &PanicInfo) {
    if ALREADY_DUMPED_DICE.set(()).is_ok() {
        let panic_id = TraceId::new();
        maybe_dice_dump(daemon_state, info, &panic_id);
    }
}

fn maybe_dice_dump(
    daemon_state: &Arc<dyn DaemonStatePanicDiceDump>,
    info: &PanicInfo,
    panic_id: &TraceId,
) {
    let is_dice_panic = info.location().map_or(false, |loc| {
        loc.file().split(&['/', '\\']).any(|x| x == "dice")
    });
    if is_dice_panic {
        let dice_dump_folder = get_panic_dump_dir().join(format!("dice-dump-{}", panic_id));
        eprintln!(
            "Buck2 panicked and DICE may be responsible. Please be patient as we try to dump DICE graph to `{:?}`",
            dice_dump_folder
        );
        if let Err(e) = daemon_state.dice_dump(&dice_dump_folder, DiceDumpFormat::Serde) {
            eprintln!("Failed to dump DICE graph: {:#}", e);
        } else {
            let maybe_report_msg = if cfg!(fbcode_build) {
                format!(
                    "Please upload the report via `jf upload {}` and then report to https://fb.workplace.com/groups/buck2users. ",
                    dice_dump_folder.display()
                )
            } else {
                "".to_owned()
            };
            eprintln!(
                "DICE graph dumped to `{:?}`. {}DICE dumps can take up a lot of disk space, you should delete the dump after reporting.",
                dice_dump_folder, maybe_report_msg
            );
        }
    }
}
