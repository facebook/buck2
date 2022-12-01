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
use dashmap::mapref::entry::Entry;
use dashmap::DashMap;
use gazebo::prelude::*;

use super::DirectoryHasher;
use super::HasDirectoryDigest;
use super::SharedDirectory;
use super::SharedDirectoryData;
use super::SharedDirectoryInner;

#[derive(Dupe_, Clone_, Allocative)]
pub struct DashMapDirectoryInterner<L, H>
where
    H: HasDirectoryDigest,
{
    inner: Arc<DashMap<<H as HasDirectoryDigest>::Digest, Weak<SharedDirectoryInner<L, H>>>>,
}

impl<L, H> DashMapDirectoryInterner<L, H>
where
    H: HasDirectoryDigest,
{
    pub fn new() -> Self {
        Self {
            inner: Arc::new(DashMap::new()),
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Get an existing entry from the interner.
    pub fn get(
        &self,
        fingerprint: &<H as HasDirectoryDigest>::Digest,
    ) -> Option<SharedDirectory<L, H>> {
        self.inner
            .get(fingerprint)
            .and_then(|inner| inner.upgrade())
            .map(|inner| SharedDirectory { inner })
    }

    /// Insert a new entry into the interner. This may insert this data, or return anexisitng
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

    /// Notify the interner that an entry has been removed.
    pub fn dropped(&self, data: &SharedDirectoryData<L, H>) {
        // Note: we still check the count here, since you could hypothetically have a race where
        // one SharedDirectory data gets released, but by the time the drop code for the
        // SharedDirectoryInner executes (which calls this), another instance has been created.
        match self.inner.entry(data.fingerprint.dupe()) {
            Entry::Occupied(mut o) if Weak::strong_count(o.get()) == 0 => {
                o.remove();
            }
            _ => {}
        }
    }
}
