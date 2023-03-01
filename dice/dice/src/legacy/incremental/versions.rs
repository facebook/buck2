/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::UnsafeCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::ops::Deref;
use std::sync::Arc;
use std::sync::Weak;

use allocative::Allocative;
use derive_more::Display;
use dupe::Dupe;
use once_cell::sync::OnceCell;
use parking_lot::lock_api::RawMutex as RawMutexApi;
use parking_lot::Mutex;
use parking_lot::RawMutex;
use parking_lot::RwLock;

use crate::versions::VersionNumber;

/// The minor version associated with the major `VersionNumber`. The minor version an identifier to
/// all currently active computations of a particular `VersionNumber`.
/// So, for all computations currently active at a particular `VersionNumber`, they all share the
/// same `MinorVersion`. Furthermore, each time a computation occurs at a `VersionNumber`, if there
/// are no currently active computations at that `VersionNumber`, the `MinorVersion` is increased.
#[derive(Copy, Eq, Debug, Display, Dupe)]
// split this due to formatters not agreeing
#[derive(PartialEq, Hash, Clone, Ord, PartialOrd, Allocative)]
#[display(fmt = "m{}", "_0")]
pub(crate) struct MinorVersion(usize);

impl MinorVersion {
    const ZERO: MinorVersion = MinorVersion(0);

    pub(crate) fn next(&self) -> MinorVersion {
        MinorVersion(self.0 + 1)
    }
}

#[cfg(test)]
impl MinorVersion {
    pub(crate) fn testing_new(num: usize) -> Self {
        MinorVersion(num)
    }
}

// A bit of a weird type to put the MinorVersion in Arc, but we do it to
// have weak guards.
#[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_arc_on_dupe))]
#[derive(Allocative)]
pub(crate) struct MinorVersionGuard(Arc<MinorVersion>);
#[derive(Allocative)]
pub(crate) struct MinorVersionWeak(Weak<MinorVersion>);

impl MinorVersionGuard {
    #[cfg(test)]
    pub(crate) fn testing_new(m_v: usize) -> Self {
        Self(Arc::new(MinorVersion(m_v)))
    }

    pub(crate) fn downgrade(&self) -> MinorVersionWeak {
        MinorVersionWeak(Arc::downgrade(&self.0))
    }
}

impl Deref for MinorVersionGuard {
    type Target = MinorVersion;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl MinorVersionWeak {
    pub(crate) fn new() -> MinorVersionWeak {
        MinorVersionWeak(Weak::new())
    }

    pub(crate) fn upgrade(&self) -> Option<MinorVersionGuard> {
        self.0.upgrade().map(MinorVersionGuard)
    }
}

/// The version to write to if updating the computations. This version is only
/// queried from the 'VersionTracker' when the underlying version is requested,
/// and only committed when this value is dropped.
#[derive(Allocative)]
pub(crate) struct VersionForWrites {
    v: OnceCell<VersionWriteGuard>,
    version_tracker: Arc<VersionTracker>,
}

impl VersionForWrites {
    fn new(version_tracker: Arc<VersionTracker>) -> Self {
        Self {
            v: OnceCell::new(),
            version_tracker,
        }
    }

    /// actually gets the version number to use for updates.
    pub(crate) fn get(&self) -> VersionNumber {
        let v = self.v.get_or_init(|| self.version_tracker.next());
        v.v
    }

    /// records no writes, so undo the write version increase
    pub(crate) fn rollback(mut self) {
        if let Some(guard) = self.v.take() {
            self.version_tracker.prev(guard);
        }
    }

    #[cfg(test)]
    pub(crate) fn testing_new(v: VersionNumber) -> Self {
        let lock = Arc::new(RawMutex::INIT);
        lock.lock();

        Self {
            v: OnceCell::from(VersionWriteGuard { lock, v }),
            version_tracker: VersionTracker::new(Box::new(|_| {})),
        }
    }
}

#[derive(Allocative)]
struct VersionWriteGuard {
    lock: Arc<RawMutex>,
    v: VersionNumber,
}

impl Drop for VersionWriteGuard {
    fn drop(&mut self) {
        unsafe { self.lock.unlock() };
    }
}

unsafe impl Send for VersionWriteGuard {}
unsafe impl Sync for VersionWriteGuard {}

impl Drop for VersionForWrites {
    fn drop(&mut self) {
        self.v
            .get()
            .iter()
            .for_each(|v| self.version_tracker.update(&v.v))
    }
}

/// Tracks the currently in-flight versions for updates and reads to ensure
/// values are up to date.
#[derive(Allocative)]
pub(crate) struct VersionTracker {
    /// Ran when versions update. If the version number is present, that was a version that was
    /// just deleted.
    #[allocative(skip)]
    on_update: Box<dyn Fn(VersionTrackerUpdateNotification<'_>)>,
    current: RwLock<VersionToMinor>,
    /// Tracks the currently active versions and how many contexts are holding each of them.
    active_versions: Mutex<HashMap<VersionNumber, usize>>,
    /// use a RawMutex here so that we can lock and unlock using our custom `VersionWriteGuard`
    /// that is Send and Sync, so that the write version can be sent across multiple threads for
    /// Dice updates, while guaranteeing that the lock is held so that updates to dice are mutually
    /// exclusive.
    write_lock: Arc<RawMutex>,
    /// locked by write_lock above
    #[allocative(skip)]
    write_version: UnsafeCell<VersionNumber>,
}

enum VersionTrackerUpdate {
    Added(VersionNumber),
    Deleted(VersionNumber),
}

pub(crate) struct VersionTrackerUpdateNotification<'a> {
    update: VersionTrackerUpdate,
    active_versions: &'a HashMap<VersionNumber, usize>,
}

impl fmt::Debug for VersionTrackerUpdateNotification<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "VersionTrackerUpdateNotification {{ ")?;
        match self.update {
            VersionTrackerUpdate::Added(v) => write!(f, "added = {}, ", v)?,
            VersionTrackerUpdate::Deleted(v) => write!(f, "deleted = {}, ", v)?,
        };
        write!(f, "active = {:?}, ", self.active_versions)?;
        write!(f, "}}")?;
        Ok(())
    }
}

impl VersionTrackerUpdateNotification<'_> {
    /// If any version was just deleted, which version that is.
    pub(crate) fn deleted_version(&self) -> Option<VersionNumber> {
        match self.update {
            VersionTrackerUpdate::Added(..) => None,
            VersionTrackerUpdate::Deleted(v) => Some(v),
        }
    }

    /// The number of active versions
    pub(crate) fn active_version_count(&self) -> usize {
        self.active_versions.len()
    }
}

unsafe impl Send for VersionTracker {}
unsafe impl Sync for VersionTracker {}

/// Provides the current version for reading, tracking the number of active references
#[derive(Allocative)]
pub(crate) struct VersionGuard {
    tracker: Arc<VersionTracker>,
    pub(crate) version: VersionNumber,
    pub(crate) minor_version_guard: MinorVersionGuard,
}

impl VersionGuard {
    #[cfg(test)]
    pub(crate) fn testing_new(
        tracker: Arc<VersionTracker>,
        version: VersionNumber,
        minor_version_guard: MinorVersionGuard,
    ) -> Self {
        *tracker.active_versions.lock().entry(version).or_default() += 1;

        Self {
            tracker,
            version,
            minor_version_guard,
        }
    }
}

impl Drop for VersionGuard {
    fn drop(&mut self) {
        let mut active_versions = self.tracker.active_versions.lock();
        let entry = active_versions.entry(self.version);

        let cleanup = match entry {
            Entry::Occupied(mut entry) => {
                *entry.get_mut() -= 1;
                if *entry.get() == 0 {
                    entry.remove();
                    true
                } else {
                    false
                }
            }
            Entry::Vacant(_) => {
                unreachable!("this version is active so it should exist in the map");
            }
        };

        if cleanup {
            (self.tracker.on_update)(VersionTrackerUpdateNotification {
                update: VersionTrackerUpdate::Deleted(self.version),
                active_versions: &active_versions,
            });
        }
    }
}

#[derive(Allocative)]
struct VersionToMinor {
    version: VersionNumber,
    /// index of the vec is the version, which index into the corresponding minor version tracker
    minor_version_tracker: Vec<MinorVersionTracker>,
}

impl VersionTracker {
    pub(crate) fn new(on_update: Box<dyn Fn(VersionTrackerUpdateNotification<'_>)>) -> Arc<Self> {
        Arc::new(VersionTracker {
            on_update,
            current: RwLock::new(VersionToMinor {
                version: VersionNumber::ZERO,
                minor_version_tracker: vec![MinorVersionTracker::new()],
            }),
            active_versions: Mutex::new(HashMap::new()),
            write_lock: Arc::new(RawMutex::INIT),
            write_version: UnsafeCell::new(VersionNumber::ZERO),
        })
    }

    /// request an increase in the global version number. This returns a
    /// 'VersionNumber' that holds the next available version number. Note
    /// that the new version isn't committed to be the new global current
    /// version until the 'VersionNumber' is dropped.
    fn next(&self) -> VersionWriteGuard {
        self.write_lock.lock();
        let v = unsafe { &mut *self.write_version.get() };
        v.inc();

        VersionWriteGuard {
            lock: self.write_lock.dupe(),
            v: *v,
        }
    }

    /// request a decrease in the global version number. This will make the next available
    /// version number one lower.
    fn prev(&self, _guard: VersionWriteGuard) {
        // lock is held by the guard
        let v = unsafe { &mut *self.write_version.get() };
        debug_assert!(&_guard.v == v);
        v.dec();
    }

    /// hands out the current "latest" committed version and its corresponding
    /// minor version. The "latest" version is the most recent version number that was given
    /// via `next`, not the most recently committed version.
    ///
    /// the minor version is updated such that it is incremented per major version, only when
    /// there are no active owners of the minor version.
    pub(crate) fn current(self: &Arc<Self>) -> VersionGuard {
        let cur = self.current.read();
        let v = cur.version;
        let m = cur.minor_version_tracker[v.0].acquire();

        let mut active_versions = self.active_versions.lock();
        *active_versions.entry(v).or_default() += 1;

        (self.on_update)(VersionTrackerUpdateNotification {
            update: VersionTrackerUpdate::Added(v),
            active_versions: &active_versions,
        });

        VersionGuard {
            tracker: self.dupe(),
            version: v,
            minor_version_guard: m,
        }
    }

    /// Requests the 'WriteVersion' that is intended to be used for updates to
    /// the incremental computations
    pub(crate) fn write(self: &Arc<Self>) -> VersionForWrites {
        VersionForWrites::new(self.dupe())
    }

    /// updates the current version to the latest of the currently stored
    /// version and the given
    fn update(&self, v: &VersionNumber) {
        let mut cur = self.current.write();

        if cur.version < *v {
            cur.minor_version_tracker
                .resize_with(v.0 + 1, MinorVersionTracker::new);
            cur.version = *v;
        }
    }
}

#[derive(Allocative)]
struct MinorVersionTracker {
    m_v: RwLock<(MinorVersionWeak, MinorVersion)>,
}

impl MinorVersionTracker {
    fn new() -> Self {
        Self {
            m_v: RwLock::new((MinorVersionWeak::new(), MinorVersion::ZERO)),
        }
    }

    fn acquire(&self) -> MinorVersionGuard {
        if let Some(m_v) = self.m_v.read().0.upgrade() {
            return m_v;
        }
        let mut v = self.m_v.write();
        if let Some(m_v) = v.0.upgrade() {
            m_v
        } else {
            let new_mv = MinorVersionGuard(Arc::new(v.1));
            let new_v = v.1.next();
            *v = (new_mv.downgrade(), new_v);
            new_mv
        }
    }
}

#[cfg(test)]
mod tests {
    use std::mem;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;
    use std::sync::Barrier;
    use std::sync::Mutex;
    use std::thread;
    use std::time::Duration;

    use dupe::Dupe;

    use crate::legacy::incremental::versions::MinorVersion;
    use crate::legacy::incremental::versions::MinorVersionTracker;
    use crate::legacy::incremental::versions::VersionNumber;
    use crate::legacy::incremental::versions::VersionTracker;

    #[test]
    fn simple_version_increases() {
        let vt = VersionTracker::new(Box::new(|_| {}));
        let vg = vt.current();
        assert_eq!(
            (vg.version, *vg.minor_version_guard),
            (VersionNumber::new(0), MinorVersion::testing_new(0))
        );

        vt.update(&vt.next().v);

        let vg = vt.current();
        assert_eq!(
            (vg.version, *vg.minor_version_guard),
            (VersionNumber::new(1), MinorVersion::testing_new(0))
        );
    }

    #[test]
    fn active_versions_are_tracked() {
        let cleaned = Arc::new(Mutex::new(None));

        let vt = VersionTracker::new({
            let c = cleaned.dupe();
            Box::new(move |v| {
                *c.lock().unwrap() = v.deleted_version();
            })
        });
        let vg1 = vt.current();
        assert_eq!(vg1.version, VersionNumber::new(0));
        assert_eq!(*vt.active_versions.lock().get(&vg1.version).unwrap(), 1,);

        let vg2 = vt.current();
        assert_eq!(vg2.version, VersionNumber::new(0));
        assert_eq!(*vt.active_versions.lock().get(&vg2.version).unwrap(), 2,);

        drop(vg2);

        assert!(cleaned.lock().unwrap().is_none());
        assert_eq!(*vt.active_versions.lock().get(&vg1.version).unwrap(), 1,);

        {
            let w = vt.write();
            w.get();
        }

        let vg3 = vt.current();
        assert_eq!(vg3.version, VersionNumber::new(1));
        assert_eq!(*vt.active_versions.lock().get(&vg3.version).unwrap(), 1,);

        assert_eq!(*vt.active_versions.lock().get(&vg1.version).unwrap(), 1,);

        drop(vg3);

        assert!(
            vt.active_versions
                .lock()
                .get(&VersionNumber::new(1))
                .is_none()
        );
        assert_eq!(*cleaned.lock().unwrap(), Some(VersionNumber::new(1)));

        assert_eq!(*vt.active_versions.lock().get(&vg1.version).unwrap(), 1,);

        drop(vg1);

        assert_eq!(*cleaned.lock().unwrap(), Some(VersionNumber::new(0)));
        assert!(
            vt.active_versions
                .lock()
                .get(&VersionNumber::new(0))
                .is_none()
        );
    }

    #[test]
    fn write_version_commits_on_drop() {
        let vt = VersionTracker::new(Box::new(|_| {}));
        {
            let vg = vt.current();
            assert_eq!(
                (vg.version, *vg.minor_version_guard),
                (VersionNumber::new(0), MinorVersion::testing_new(0))
            );
        }

        {
            let v1 = vt.write();
            assert_eq!(v1.get(), VersionNumber::new(1));
            let vg = vt.current();
            assert_eq!(
                (vg.version, *vg.minor_version_guard),
                (VersionNumber::new(0), MinorVersion::testing_new(1))
            );

            std::mem::drop(vg);

            let vg = vt.current();
            assert_eq!(
                (vg.version, *vg.minor_version_guard),
                (VersionNumber::new(0), MinorVersion::testing_new(2))
            );

            std::mem::drop(v1);

            let v2 = vt.write();
            assert_eq!(v2.get(), VersionNumber::new(2),);
            let vg = vt.current();
            assert_eq!(
                (vg.version, *vg.minor_version_guard),
                (VersionNumber::new(1), MinorVersion::testing_new(0))
            );
            std::mem::drop(v2);

            let vg = vt.current();
            assert_eq!(
                (vg.version, *vg.minor_version_guard),
                (VersionNumber::new(2), MinorVersion::testing_new(0))
            );
        }
        {
            let vg = vt.current();
            assert_eq!(
                (vg.version, *vg.minor_version_guard),
                (VersionNumber::new(2), MinorVersion::testing_new(1))
            );
        }
    }

    #[test]
    fn write_version_is_lazy() {
        let vt = VersionTracker::new(Box::new(|_| {}));

        let write1 = vt.write();
        let write2 = vt.write();

        assert_eq!(write1.v.get().is_some(), false);
        assert_eq!(write2.v.get().is_some(), false);

        // getting write2 first gives it the lower number
        assert_eq!(write2.get(), VersionNumber::new(1));
        mem::drop(write2);

        assert_eq!(write1.get(), VersionNumber::new(2));
    }

    #[test]
    fn write_version_rollbacks() {
        let vt = VersionTracker::new(Box::new(|_| {}));

        let write1 = vt.write();
        let write2 = vt.write();
        let write3 = vt.write();
        let write4 = vt.write();

        assert!(write1.v.get().is_none());
        assert!(write2.v.get().is_none());
        assert!(write3.v.get().is_none());
        assert!(write4.v.get().is_none());

        assert_eq!(write2.get(), VersionNumber::new(1));
        write2.rollback();

        assert_eq!(write1.get(), VersionNumber::new(1));
        drop(write1);

        // never attempted to get a version can still rollback properly
        write3.rollback();
        assert_eq!(write4.get(), VersionNumber::new(2));
    }

    #[test]
    fn minor_version_updates_only_when_no_refs() {
        let vt = MinorVersionTracker::new();

        {
            let m1 = vt.acquire();
            assert_eq!(*m1, MinorVersion::testing_new(0));

            let m2 = vt.acquire();
            assert_eq!(*m2, MinorVersion::testing_new(0));

            mem::drop(m1);
            let m3 = vt.acquire();
            assert_eq!(*m3, MinorVersion::testing_new(0));
        }

        let m = vt.acquire();
        assert_eq!(*m, MinorVersion::testing_new(1));
    }

    #[test]
    fn version_write_is_exclusive() {
        let tracker = VersionTracker::new(Box::new(|_| {}));
        let write_v = tracker.write();
        assert_eq!(write_v.get(), VersionNumber::new(1));

        let barrier = Arc::new(Barrier::new(2));

        let is_ran = Arc::new(AtomicBool::new(false));

        let handle = thread::spawn({
            let tracker = tracker.dupe();
            let is_ran = is_ran.dupe();
            let barrier = barrier.dupe();
            move || {
                barrier.wait();

                let write_v = tracker.write();
                assert_eq!(write_v.get(), VersionNumber::new(2));

                is_ran.store(true, Ordering::SeqCst);
            }
        });

        barrier.wait();
        // sadly there's still a race that the spawned thread might not execute the call to `write()`
        // so we just sleep here briefly and hope.
        thread::sleep(Duration::from_secs(1));

        assert_eq!(is_ran.load(Ordering::SeqCst), false);

        mem::drop(write_v);

        handle.join().unwrap();
    }
}
