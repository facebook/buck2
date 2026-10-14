/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Build-log ingestion: a buck2 event log decoded natively through
//! `buck2_event_log`, in order and in bounded parallel batches, into the
//! per-action records and command profile the rest of retrace reads.

#[cfg(test)]
mod action_projection;
mod compact;
mod decode_pool;
mod heap_size;
mod normalized;
mod profile;
pub mod scan;
pub mod testlog;

use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

#[cfg(test)]
use buck2_event_log::read::EventLogPathBuf;

pub(super) fn check_cancel(cancel: Option<&AtomicBool>) -> anyhow::Result<()> {
    anyhow::ensure!(
        !cancel.is_some_and(|c| c.load(Ordering::Relaxed)),
        "ingestion cancelled"
    );
    Ok(())
}

#[derive(Clone, Copy, Debug)]
pub(super) enum HeapMode {
    #[cfg(test)]
    Reference,
    Fast,
    #[cfg(test)]
    Verify,
}

impl HeapMode {
    #[cfg(test)]
    fn bytes(
        self,
        item: &buck2_error::Result<buck2_event_log::stream_value::StreamValue>,
    ) -> usize {
        let reference = || match item {
            Ok(value) => allocative::size_of_unique_allocated_data(value),
            Err(error) => allocative::size_of_reachable(error)
                .saturating_sub(std::mem::size_of::<buck2_error::Error>()),
        };
        match self {
            Self::Reference => reference(),
            Self::Fast => heap_size::item_heap_bytes(item),
            Self::Verify => {
                let fast = heap_size::item_heap_bytes(item);
                assert_eq!(fast, reference(), "fast decoded heap accounting differs");
                fast
            }
        }
    }
}

#[cfg(test)]
fn infer(p: &std::path::Path) -> anyhow::Result<EventLogPathBuf> {
    let abs = std::fs::canonicalize(p).map_err(|e| anyhow::anyhow!("{}: {e}", p.display()))?;
    let abs =
        buck2_fs::paths::abs_path::AbsPathBuf::new(abs).map_err(|e| anyhow::anyhow!("{e:#}"))?;
    EventLogPathBuf::infer(abs).map_err(|e| anyhow::anyhow!("{e:#}"))
}
