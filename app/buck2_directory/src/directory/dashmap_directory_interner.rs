/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::sync::Weak;

use allocative::Allocative;
use buck2_core::directory_digest::DirectoryDigest;
use buck2_core::directory_digest::InternableDirectoryDigest;
use buck2_util::hash::BuckHasherBuilder;
use dashmap::mapref::entry::Entry;
use dashmap::DashMap;
use dupe::Clone_;
use dupe::Dupe;
use dupe::Dupe_;

use crate::directory::shared_directory::SharedDirectory;
use crate::directory::shared_directory::SharedDirectoryData;
use crate::directory::shared_directory::SharedDirectoryInner;

#[derive(Dupe_, Clone_, Allocative)]
pub struct DashMapDirectoryInterner<L, H>
where
    H: DirectoryDigest,
{
    inner: Arc<DashMap<H, Weak<SharedDirectoryInner<L, H>>, BuckHasherBuilder>>,
}

impl<L, H> DashMapDirectoryInterner<L, H>
where
    H: InternableDirectoryDigest,
{
    pub fn new() -> Self {
        Self {
            inner: Arc::new(DashMap::with_hasher(BuckHasherBuilder)),
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Get an existing entry from the interner.
    pub fn get(&self, fingerprint: &H) -> Option<SharedDirectory<L, H>> {
        self.inner
            .get(fingerprint)
            .and_then(|inner| inner.upgrade())
            .map(|inner| SharedDirectory { inner })
    }

    /// Insert a new entry into the interner. This may insert this data, or return an existing
    /// entry.
    pub fn intern(&self, data: SharedDirectoryData<L, H>) -> SharedDirectory<L, H> {
        let new_inner = match self.inner.entry(data.fingerprint.dupe()) {
            Entry::Occupied(mut o) => {
                if let Some(inner) = o.get().upgrade() {
                    return SharedDirectory { inner };
                }

                // Constructing this here is a bit duplicative but it avoids creating a new
                // SharedDirectoryInner above just to drop it, and avoids an early return possibly
                // calling dropped() below.
                let new_inner = Arc::new(SharedDirectoryInner {
                    data,
                    interner: self.dupe(),
                });

                o.insert(Arc::downgrade(&new_inner));

                new_inner
            }
            Entry::Vacant(v) => {
                let new_inner = Arc::new(SharedDirectoryInner {
                    data,
                    interner: self.dupe(),
                });

                v.insert(Arc::downgrade(&new_inner));

                new_inner
            }
        };

        SharedDirectory { inner: new_inner }
    }
}

impl<L, H> DashMapDirectoryInterner<L, H>
where
    // Note: We "should" require `H: InternableDirectoryDigest` here; however, we can't do that
    // because `Drop` impls having to be always-applicable would force us to require `H:
    // InternableDirectoryDigest` on `ImmutableDirectory`. This should still be ok though, because
    // you can't create a `SharedDirectory` for which that trait bound is not met.
    H: DirectoryDigest,
{
    /// Notify the interner that an entry has been removed.
    pub fn dropped(&self, data: &SharedDirectoryData<L, H>) {
        // Note: we still check the count here, since you could hypothetically have a race where
        // one SharedDirectory data gets released, but by the time the drop code for the
        // SharedDirectoryInner executes (which calls this), another instance has been created.
        match self.inner.entry(data.fingerprint.dupe()) {
            Entry::Occupied(o) if Weak::strong_count(o.get()) == 0 => {
                o.remove();
            }
            _ => {}
        }
    }
}
