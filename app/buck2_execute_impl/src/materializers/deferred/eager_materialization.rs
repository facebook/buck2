/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::sync::Arc;
use std::sync::Weak;

use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::materialize::materializer::EagerMaterializationGuard;
use buck2_hash::BuckMutMap;
use buck2_hash::BuckMutSet;
use dupe::Dupe;

use crate::materializers::deferred::MaterializerCommand;
use crate::materializers::deferred::MaterializerSender;

/// Wraps the leases so they can be returned as `Box<dyn EagerMaterializationGuard>`.
/// Dropping this releases all path registrations.
#[allow(dead_code)]
pub(crate) struct EagerPathLeases<T: 'static>(pub(crate) Vec<Arc<EagerPathLease<T>>>);

impl<T: 'static> EagerMaterializationGuard for EagerPathLeases<T> {}

/// Keeps a path registered for eager materialization for as long as this lease is alive.
/// When all Arc references are dropped, sends a release command back to the materializer.
#[must_use]
pub(crate) struct EagerPathLease<T: 'static> {
    path: Arc<ProjectRelativePathBuf>,
    command_sender: Arc<MaterializerSender<T>>,
}

impl<T: 'static> Drop for EagerPathLease<T> {
    fn drop(&mut self) {
        let _ignored = self
            .command_sender
            .send(MaterializerCommand::ReleaseEagerPath(self.path.dupe()));
    }
}

impl<T: 'static> fmt::Debug for EagerPathLease<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EagerPathLease")
            .field("path", &self.path)
            .finish()
    }
}

pub(super) struct EagerMaterializations<T: 'static> {
    active: BuckMutMap<ProjectRelativePathBuf, Weak<EagerPathLease<T>>>,
    bridged_declares: BuckMutMap<ProjectRelativePathBuf, BuckMutSet<ProjectRelativePathBuf>>,
}

impl<T: 'static> EagerMaterializations<T> {
    pub(super) fn new() -> Self {
        Self {
            active: BuckMutMap::default(),
            bridged_declares: BuckMutMap::default(),
        }
    }

    pub(super) fn register(
        &mut self,
        paths: Vec<ProjectRelativePathBuf>,
        command_sender: &Arc<MaterializerSender<T>>,
    ) -> Vec<Arc<EagerPathLease<T>>> {
        paths
            .into_iter()
            .map(|path| {
                if let Some(existing) = self.active.get(&path).and_then(Weak::upgrade) {
                    return existing;
                }

                let path = Arc::new(path);
                let lease = Arc::new(EagerPathLease {
                    path: path.dupe(),
                    command_sender: command_sender.dupe(),
                });
                self.active.insert((*path).clone(), Arc::downgrade(&lease));
                lease
            })
            .collect()
    }

    pub(super) fn should_materialize_eagerly(
        &self,
        path: &buck2_core::fs::project_rel_path::ProjectRelativePath,
    ) -> bool {
        self.active
            .get(path)
            .is_some_and(|lease| lease.upgrade().is_some())
    }

    /// For bridged content-based declares, remembers the actual declared path so release can
    /// cancel the in-flight low-priority work later.
    pub(super) fn add_bridged_declare(
        &mut self,
        eager_path: &ProjectRelativePath,
        declared_path: &ProjectRelativePath,
    ) {
        if eager_path == declared_path {
            return;
        }

        self.bridged_declares
            .entry(eager_path.to_owned())
            .or_default()
            .insert(declared_path.to_owned());
    }

    /// Releases a registered eager path. When the last lease goes away, returns all paths whose
    /// low-priority materialization should be cancelled: the registered path itself plus any
    /// bridged declared paths that were triggered by it.
    pub(super) fn release(
        &mut self,
        path: &ProjectRelativePath,
    ) -> Option<Vec<ProjectRelativePathBuf>> {
        match self.active.remove_entry(path) {
            Some((_path, lease)) if lease.upgrade().is_none() => {
                let mut paths_to_cancel = vec![path.to_owned()];
                if let Some(bridged_paths) = self.bridged_declares.remove(path) {
                    paths_to_cancel.extend(bridged_paths);
                }
                Some(paths_to_cancel)
            }
            Some((path, lease)) => {
                self.active.insert(path, lease);
                None
            }
            None => None,
        }
    }
}
