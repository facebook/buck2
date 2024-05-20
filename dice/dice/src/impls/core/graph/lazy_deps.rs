/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;

use crate::impls::key::DiceKey;
use crate::versions::VersionNumber;

/// Provides a set that lazily dedupes entries. This is efficient for common patterns that we see for rdeps tracking.
///
/// For rdeps, in the normal flow (where computations happen at increasing versions without overlap) the sequence of things we'll see is:
///
/// 1. the first version will add some set of unique rdeps
/// 2. subsequent versions will add more rdeps, a dep will only be added once at each version, but can be repeated across versions
/// 3. eventually the node itself will be invalidated and the rdeps will be drained
///
/// This means that the inserts will be a sequence of sets of unique deps. deduping multiple times withing one such block is wasteful, and
/// deduping on every block may also be wasteful (a typical flow adds a lot of rdeps in the first block, and then only a small number in each
/// subsequent block).
///
/// So, we track the version so we can identify the transitions between those blocks. and only do dedupe (1) when starting a new block and (2)
/// if our list is sufficiently large (we know the minimum size is the largest of our previous deduped size or the size of any block we've added).
///
/// One common pattern that is close to our worst case is that a node itself is never invalidated but that all of its rdeps are. In that case
/// at every version we'll be adding the same N rdeps to the list. In that case, we'll do the sort+dedupe on every version. Overall, this
/// ends up being better in practice than using HashSet.
#[derive(Allocative, Debug)]
pub(crate) struct LazyDepsSet {
    data: Vec<DiceKey>,
    state: State,
}

#[derive(Allocative, Debug, Clone, Copy)]
enum State {
    New,
    Growing {
        /// The last version that we've seen. We use this to track the size of "blocks" of unique deps
        latest_version: VersionNumber,
        /// How many deps have been added at `latest_version`
        this_version_count: u32,
        /// The maximum of our previous deduped size and the size of any block of deps we've seen since then
        min_size: u32,
    },
}

impl LazyDepsSet {
    pub(crate) fn new() -> LazyDepsSet {
        Self {
            data: Vec::new(),
            state: State::New,
        }
    }

    pub(crate) fn insert(&mut self, v: VersionNumber, k: DiceKey) {
        match self.state {
            State::New => {
                self.state = State::Growing {
                    latest_version: v,
                    min_size: 0,
                    this_version_count: 1,
                }
            }
            State::Growing {
                latest_version,
                min_size,
                ref mut this_version_count,
            } => {
                if latest_version == v {
                    *this_version_count += 1;
                } else if latest_version < v {
                    // This indicates that we've started a new "block".
                    let mut min_size = std::cmp::max(min_size, *this_version_count);

                    // need to decide if we should clean up duplicates
                    let resize_threshold = std::cmp::max(10, (min_size as usize) * 3 / 2);
                    if self.data.len() > resize_threshold {
                        // stable sort is going to best handle the fact that the initial run is completely sorted.
                        self.data.sort();
                        self.data.dedup();
                        min_size = self.data.len() as u32;
                    }

                    self.state = State::Growing {
                        latest_version: v,
                        min_size,
                        this_version_count: 1,
                    }
                } else {
                    // an older version... just ignore?
                    // TODO(cjhopman): We should revisit this behavior when we more widely allow concurrent computations.
                }
            }
        }
        self.data.push(k);
    }

    /// Clears the set and returns all currently stored deps. The returned iterator might contain duplicates.
    pub(crate) fn drain(&mut self) -> std::vec::Drain<'_, DiceKey> {
        self.state = State::New;
        self.data.drain(..)
    }

    /// Iterates over all currently stored deps. The returned iterator might contain duplicates.
    pub(crate) fn iter(&self) -> impl Iterator<Item = DiceKey> + '_ {
        self.data.iter().copied()
    }
}
