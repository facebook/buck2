/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

use dupe::Dupe;

#[derive(Copy, Clone, Dupe, Debug)]
pub enum IoCounterKey {
    Copy,
    Symlink,
    Hardlink,
    MkDir,
    ReadDir,
    ReadDirEden,
    RmDir,
    RmDirAll,
    Stat,
    StatEden,
    Chmod,
    ReadLink,
    Remove,
    Rename,
    Read,
    Write,
    Canonicalize,
    EdenSettle,
}

static IN_PROGRESS: [AtomicU32; IoCounterKey::COUNT] = [
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
];

static FINISHED: [AtomicU32; IoCounterKey::COUNT] = [
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
    AtomicU32::new(0),
];

impl IoCounterKey {
    pub const ALL: &'static [IoCounterKey] = &[
        IoCounterKey::Copy,
        IoCounterKey::Symlink,
        IoCounterKey::Hardlink,
        IoCounterKey::MkDir,
        IoCounterKey::ReadDir,
        IoCounterKey::ReadDirEden,
        IoCounterKey::RmDir,
        IoCounterKey::RmDirAll,
        IoCounterKey::Stat,
        IoCounterKey::StatEden,
        IoCounterKey::Chmod,
        IoCounterKey::ReadLink,
        IoCounterKey::Remove,
        IoCounterKey::Rename,
        IoCounterKey::Read,
        IoCounterKey::Write,
        IoCounterKey::Canonicalize,
        IoCounterKey::EdenSettle,
    ];

    const COUNT: usize = IoCounterKey::ALL.len();

    pub fn get(&self) -> u32 {
        IN_PROGRESS[*self as usize].load(Ordering::Relaxed)
    }

    pub fn get_finished(&self) -> u32 {
        FINISHED[*self as usize].load(Ordering::Relaxed)
    }

    pub fn guard(&self) -> IoCounterGuard {
        IN_PROGRESS[*self as usize].fetch_add(1, Ordering::Relaxed);
        IoCounterGuard(*self)
    }
}

#[must_use]
pub struct IoCounterGuard(IoCounterKey);

impl Drop for IoCounterGuard {
    fn drop(&mut self) {
        IN_PROGRESS[self.0 as usize].fetch_sub(1, Ordering::Relaxed);
        // Note: Relaxed ordering is sufficient since these counters are only
        // used for metrics/debugging.
        FINISHED[self.0 as usize].fetch_add(1, Ordering::Relaxed);
    }
}

#[cfg(test)]
mod tests {
    use crate::io_counters::IoCounterKey;

    #[test]
    fn test_keys() {
        for k in IoCounterKey::ALL {
            // Check `IN_PROGRESS` is correct size.
            k.get();
        }
    }
}
