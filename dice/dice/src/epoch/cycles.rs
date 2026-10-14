/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Cycles among the tasks of one version.
//!
//! A computation that, through its dependencies, waits on itself never finishes; neither does
//! anything waiting on it. DICE deals with that by failing the waits that make up the cycle: once
//! a cycle has been found, each task in it is marked with it (`DiceTaskRef::mark_in_cycle`), and
//! from then on a dependent that is itself a key of the cycle gets a cycle error from its
//! `DicePromise` on any of the cycle's tasks instead of a value. The computations involved see
//! that error from `DiceComputations::compute` and finish with whatever they make of it, and
//! their tasks complete like any other, so dependents from outside the cycle are served a value
//! as usual.

use std::collections::VecDeque;
use std::sync::Arc;
use std::sync::OnceLock;

use dice_error::cycles::CycleError;
use dice_error::cycles::RequestedKey;
use dupe::Dupe;
use parking_lot::Mutex;

use crate::DynKey;
use crate::key::DiceKey;
use crate::key::DiceKeyErased;
use crate::key::ParentKey;

/// A dependency cycle found among the pending tasks of a version: a set of keys each of which
/// waits, directly or through the others, on itself.
pub(crate) struct DetectedCycle {
    /// Sorted by key.
    members: Box<[Member]>,
    /// Who waits on whom within the cycle, as indices into `members`.
    edges: Box<[(u32, u32)]>,
}

struct Member {
    key: DiceKey,
    erased: DiceKeyErased,
    /// Whether this member's waits on the other members fail.
    ///
    /// They don't for a projection key: its task only ever waits for its base, on behalf of a
    /// dep check that could do nothing with an error (see the projection arm of
    /// `TransactionData::evaluate`), so it keeps waiting and projects the base's eventual value
    /// instead. That leaves every cycle with a failing wait all the same, since a base is never
    /// a projection key.
    fails: bool,
}

impl DetectedCycle {
    /// `members` gives each key of the cycle with whether its waits fail; `edges` the waits among
    /// them, as `(waiting key, awaited key)`.
    #[allow(dead_code)]
    pub(crate) fn new(
        members: impl IntoIterator<Item = (DiceKey, DiceKeyErased, bool)>,
        edges: impl IntoIterator<Item = (DiceKey, DiceKey)>,
    ) -> Arc<Self> {
        let mut members: Box<[Member]> = members
            .into_iter()
            .map(|(key, erased, fails)| Member { key, erased, fails })
            .collect();
        members.sort_by_key(|m| m.key);
        let index_of = |k: DiceKey| -> u32 {
            members
                .binary_search_by_key(&k, |m| m.key)
                .expect("edges connect members") as u32
        };
        let edges = edges
            .into_iter()
            .map(|(from, to)| (index_of(from), index_of(to)))
            .collect();
        Arc::new(DetectedCycle { members, edges })
    }

    pub(crate) fn keys(&self) -> impl Iterator<Item = DiceKey> + '_ {
        self.members.iter().map(|m| m.key)
    }

    /// The error with which `parent`'s wait on `awaited`, a key of this cycle, fails, or `None`
    /// if that wait is to go on: `parent` is not a key of the cycle, or one whose waits don't
    /// fail.
    pub(crate) fn error_for(&self, parent: ParentKey, awaited: DiceKey) -> Option<CycleError> {
        let ParentKey::Some(parent) = parent else {
            return None;
        };
        let parent = self.index_of(parent)?;
        if !self.members[parent].fails {
            return None;
        }
        let awaited = self
            .index_of(awaited)
            .expect("a task is only marked with cycles it is a key of");
        Some(CycleError {
            keys: self
                .shortest_cycle(parent, awaited)
                .into_iter()
                .map(|i| {
                    Arc::new(DynKey {
                        erased: self.members[i].erased.dupe(),
                    }) as Arc<dyn RequestedKey>
                })
                .collect(),
        })
    }

    fn index_of(&self, k: DiceKey) -> Option<usize> {
        self.members.binary_search_by_key(&k, |m| m.key).ok()
    }

    /// The members along a shortest cycle that starts with `from` waiting on `to`; the last one
    /// waits on `from` again. Every pair of members has one, the cycle being strongly connected.
    fn shortest_cycle(&self, from: usize, to: usize) -> Vec<usize> {
        if from == to {
            return vec![from];
        }
        // A BFS from `to` back to `from`, remembering how each member was reached.
        let mut reached_from = vec![None; self.members.len()];
        let mut queue = VecDeque::from([to]);
        reached_from[to] = Some(to);
        while let Some(cur) = queue.pop_front() {
            if cur == from {
                break;
            }
            for &(u, v) in self.edges.iter() {
                let (u, v) = (u as usize, v as usize);
                if u == cur && reached_from[v].is_none() {
                    reached_from[v] = Some(cur);
                    queue.push_back(v);
                }
            }
        }
        assert!(
            reached_from[from].is_some(),
            "a cycle's members are strongly connected"
        );
        let mut path = vec![from];
        let mut cur = from;
        while cur != to {
            cur = reached_from[cur].expect("walking back along the BFS");
            path.push(cur);
        }
        path.reverse();
        // `path` now runs `to ... from`; the cycle starts with `from` waiting on `to`.
        path.rotate_right(1);
        path
    }
}

/// The cycles a task has been found to be a key of, see `DiceTaskRef::mark_in_cycle`.
///
/// Usually none, and normally at most one; more only for a key whose compute swallows the cycle
/// error, goes on waiting, and ends up in another cycle.
#[derive(Default)]
pub(crate) struct TaskCycles(OnceLock<Box<Mutex<Vec<Arc<DetectedCycle>>>>>);

impl TaskCycles {
    pub(crate) fn add(&self, cycle: &Arc<DetectedCycle>) {
        self.0
            .get_or_init(Default::default)
            .lock()
            .push(cycle.dupe());
    }

    /// The error with which `waiting`'s wait on `awaited`, the task's key, fails, if one of the
    /// cycles fails it; see `DetectedCycle::error_for`.
    pub(crate) fn error_for(&self, waiting: ParentKey, awaited: DiceKey) -> Option<CycleError> {
        self.0
            .get()?
            .lock()
            .iter()
            .find_map(|cycle| cycle.error_for(waiting, awaited))
    }
}
