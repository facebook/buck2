/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Represents the forward and backward dependencies of the computation graph

use std::collections::hash_map::Entry;

use allocative::Allocative;
use dupe::Dupe;

use crate::arc::Arc;
use crate::impls::deps::graph::SeriesParallelDeps;
use crate::impls::key::DiceKey;
use crate::versions::VersionNumber;
use crate::HashMap;

#[derive(Allocative, Clone)]
pub(crate) struct VersionedDependencies {
    /// once the deps at a particular version is written, it is final and never modified
    /// We only store the dependencies relevant to the most recent result
    recorded_at: VersionNumber,
    deps: Arc<SeriesParallelDeps>,
}

impl Dupe for VersionedDependencies {
    // triomphe is dupe
}

impl VersionedDependencies {
    pub(crate) fn new(recorded_at: VersionNumber, deps: Arc<SeriesParallelDeps>) -> Self {
        Self { recorded_at, deps }
    }

    pub(crate) fn deps(&self) -> &Arc<SeriesParallelDeps> {
        &self.deps
    }
}

// the set of reverse dependencies of a node
#[derive(Allocative, Clone, Debug)] // TODO(bobyf) remove need to clone
pub(crate) struct VersionedRevDependencies {
    rdeps: HashMap<DiceKey, VersionNumber>,
}

impl VersionedRevDependencies {
    pub(crate) fn new() -> Self {
        Self {
            rdeps: Default::default(),
        }
    }

    pub(crate) fn add_rdep(&mut self, dependent: DiceKey, current_version: VersionNumber) {
        match self.rdeps.entry(dependent) {
            Entry::Occupied(entry) => {
                if *entry.get() < current_version {
                    entry.replace_entry(current_version);
                }
            }
            Entry::Vacant(v) => {
                v.insert(current_version);
            }
        }
    }

    pub(crate) fn rdeps(&self) -> &HashMap<DiceKey, VersionNumber> {
        &self.rdeps
    }
}
