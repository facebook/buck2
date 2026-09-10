/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Resolution (`incrementality.md` §4.4) and certificate coverage (§4.5).

use smallvec::SmallVec;
use smallvec::smallvec;

use crate::arc::Arc;
use crate::cert::Cert;
use crate::env::Env;
use crate::env::Premise;
use crate::history::Entry;
use crate::ids::BranchId;
use crate::ids::EpsilonToken;
use crate::ids::Key;
use crate::ids::Revision;
use crate::ids::Seq;
use crate::ids::Version;
use crate::slot::Claim;
use crate::slot::Window;
use crate::state::CoreState;

/// What a key resolves to at a version.
pub(crate) enum Resolved<'a, E: Env> {
    /// `Valid` through a claim; `owner` is the branch holding it, which is the queried branch
    /// or an ancestor the query delegated to.
    Claim {
        owner: BranchId,
        claim: &'a Claim<E>,
    },
    /// `Valid` through an assertion history entry, for an asserted key.
    Asserted {
        owner: BranchId,
        entry: &'a Entry<Revision, E::AssertionData>,
    },
    /// `Unknown`, with the certificate a caller may try to re-establish: the nearest claim on the
    /// resolution chain whatever its window, else any claim the key has on another branch.
    Unknown {
        candidate: Option<&'a Arc<Cert<E::Premises>>>,
    },
}

impl<'a, E: Env> Resolved<'a, E> {
    pub(crate) fn revision(&self) -> Option<Revision> {
        match self {
            Resolved::Claim { claim, .. } => Some(claim.revision()),
            Resolved::Asserted { entry, .. } => Some(entry.revision),
            Resolved::Unknown { .. } => None,
        }
    }
}

/// How a key is attached at a branch's head (§4.6).
pub(crate) enum Attached<'a, E: Env> {
    /// Through the branch's own open claim.
    Own(&'a Claim<E>),
    /// Through inheritance from an ancestor's claim, which the branch's own commits have not
    /// diverged from.
    Inherited { claim: &'a Claim<E> },
}

impl<'a, E: Env> Attached<'a, E> {
    pub(crate) fn claim(&self) -> &'a Claim<E> {
        match self {
            Attached::Own(claim) | Attached::Inherited { claim, .. } => claim,
        }
    }
}

/// A half-open range of seqs `[from, until)` of one branch, `until` absent meaning unbounded.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) struct Interval {
    pub(crate) from: Seq,
    pub(crate) until: Option<Seq>,
}

impl Interval {
    fn is_empty(self) -> bool {
        self.until == Some(self.from)
    }

    fn contains(self, seq: Seq) -> bool {
        self.from <= seq && self.until.is_none_or(|until| seq < until)
    }
}

/// Disjoint, nonempty intervals in increasing order.
pub(crate) type Intervals = SmallVec<[Interval; 2]>;

fn push_merging(out: &mut Intervals, interval: Interval) {
    if interval.is_empty() {
        return;
    }
    if let Some(last) = out.last_mut()
        && last.until == Some(interval.from)
    {
        last.until = interval.until;
        return;
    }
    out.push(interval);
}

fn intersect(a: &[Interval], b: &[Interval]) -> Intervals {
    let mut out = Intervals::new();
    let (mut i, mut j) = (0, 0);
    while i < a.len() && j < b.len() {
        let from = a[i].from.max(b[j].from);
        let until = match (a[i].until, b[j].until) {
            (Some(x), Some(y)) => Some(x.min(y)),
            (Some(x), None) | (None, Some(x)) => Some(x),
            (None, None) => None,
        };
        if until.is_none_or(|until| from < until) {
            push_merging(&mut out, Interval { from, until });
        }
        // Advance whichever interval ends first; an unbounded one never does.
        match (a[i].until, b[j].until) {
            (Some(x), Some(y)) if x <= y => i += 1,
            (Some(_), Some(_)) => j += 1,
            (Some(_), None) => i += 1,
            (None, Some(_)) => j += 1,
            (None, None) => break,
        }
    }
    out
}

impl<E: Env> CoreState<E> {
    /// Resolves `key` at `v` (§4.4): the branch's own claim or assertion history if it has one,
    /// else the parent's at the fork point, and so on up to a root.
    pub(crate) fn resolve(&self, key: Key, v: Version) -> Resolved<'_, E> {
        if let Some(histories) = self.assertions.get(&key) {
            let mut cur = v;
            loop {
                if let Some(history) = histories
                    .iter()
                    .find_map(|(b, h)| (*b == cur.branch()).then_some(h))
                    && let Some(entry) = history.at(cur.seq())
                {
                    return Resolved::Asserted {
                        owner: cur.branch(),
                        entry,
                    };
                }
                match self.branch(cur.branch()).parent {
                    Some(parent) => cur = parent,
                    None => return Resolved::Unknown { candidate: None },
                }
            }
        }
        let slots = self.slots.get(&key).map(|s| s.as_slice()).unwrap_or(&[]);
        let mut cur = v;
        loop {
            if let Some(slot) = slots.iter().find(|s| s.branch == cur.branch())
                && let Some(claim) = &slot.claim
            {
                if claim.window.covers(cur.seq()) {
                    return Resolved::Claim {
                        owner: cur.branch(),
                        claim,
                    };
                }
                return Resolved::Unknown {
                    candidate: Some(&claim.cert),
                };
            }
            match self.branch(cur.branch()).parent {
                Some(parent) => cur = parent,
                None => {
                    return Resolved::Unknown {
                        candidate: slots.iter().find_map(|s| s.claim.as_ref().map(|c| &c.cert)),
                    };
                }
            }
        }
    }

    /// The assertion of `key`'s untracked input in force at `v`, or `None` for the root default
    /// [`EpsilonToken::INITIAL`].
    pub(crate) fn resolve_untracked(
        &self,
        key: Key,
        v: Version,
    ) -> Option<(BranchId, &Entry<EpsilonToken, E::AssertionData>)> {
        let slots = self.slots.get(&key)?;
        let mut cur = v;
        loop {
            if let Some(slot) = slots.iter().find(|s| s.branch == cur.branch())
                && let Some(entry) = slot.untracked.at(cur.seq())
            {
                return Some((cur.branch(), entry));
            }
            cur = self.branch(cur.branch()).parent?;
        }
    }

    /// The revision of `key`'s untracked input at `v`.
    pub fn epsilon(&self, key: Key, v: Version) -> EpsilonToken {
        self.resolve_untracked(key, v)
            .map_or(EpsilonToken::INITIAL, |(_, entry)| entry.revision)
    }

    /// Whether, and how, `key` is attached at branch `b` (§4.6): resolves `Valid` at the head
    /// through an own open claim or through inheritance. During a commit, a claim the commit has
    /// already closed at the new seq still covers the old head; it is not attached.
    pub(crate) fn attached(&self, key: Key, b: BranchId) -> Option<Attached<'_, E>> {
        match self.resolve(key, self.head(b)) {
            Resolved::Claim { owner, claim } if owner == b => {
                claim.window.is_open().then_some(Attached::Own(claim))
            }
            Resolved::Claim { claim, .. } => Some(Attached::Inherited { claim }),
            Resolved::Asserted { .. } | Resolved::Unknown { .. } => None,
        }
    }

    /// The seqs of `b` at which an assertion history resolves to `revision`, given the branch's
    /// own entries and whether the parent resolves to `revision` at the fork point (§4.5).
    fn history_intervals<R: Copy + Eq, T>(
        &self,
        b: BranchId,
        own: &[Entry<R, T>],
        parent_matches: bool,
        revision: R,
    ) -> Intervals {
        let mut out = Intervals::new();
        if parent_matches {
            push_merging(
                &mut out,
                Interval {
                    from: self.branch(b).first,
                    until: own.first().map(|e| e.seq),
                },
            );
        }
        for (i, entry) in own.iter().enumerate() {
            if entry.revision == revision {
                push_merging(
                    &mut out,
                    Interval {
                        from: entry.seq,
                        until: own.get(i + 1).map(|e| e.seq),
                    },
                );
            }
        }
        out
    }

    /// The seqs of `b` at which `premise` resolves to its recorded revision.
    fn premise_intervals(&self, b: BranchId, premise: Premise) -> Intervals {
        let revision = premise.revision;
        let parent = self.branch(b).parent;
        if let Some(histories) = self.assertions.get(&premise.key) {
            let own = histories
                .iter()
                .find_map(|(hb, h)| (*hb == b).then_some(h.entries()))
                .unwrap_or(&[]);
            let parent_matches =
                parent.is_some_and(|p| self.resolve(premise.key, p).revision() == Some(revision));
            return self.history_intervals(b, own, parent_matches, revision);
        }
        match self.slot(premise.key, b).and_then(|s| s.claim.as_ref()) {
            Some(claim) => {
                if claim.revision() == revision && !claim.window.is_empty() {
                    smallvec![Interval {
                        from: claim.window.from(),
                        until: claim.window.until(),
                    }]
                } else {
                    Intervals::new()
                }
            }
            None => {
                let inherited = parent
                    .is_some_and(|p| self.resolve(premise.key, p).revision() == Some(revision));
                if inherited {
                    smallvec![Interval {
                        from: self.branch(b).first,
                        until: None,
                    }]
                } else {
                    Intervals::new()
                }
            }
        }
    }

    /// The seqs of `b` at which `key`'s untracked input has revision `epsilon`.
    fn untracked_intervals(&self, b: BranchId, key: Key, epsilon: EpsilonToken) -> Intervals {
        let own = self
            .slot(key, b)
            .map(|s| s.untracked.entries())
            .unwrap_or(&[]);
        let parent_matches = match self.branch(b).parent {
            Some(p) => self.epsilon(key, p) == epsilon,
            None => epsilon == EpsilonToken::INITIAL,
        };
        self.history_intervals(b, own, parent_matches, epsilon)
    }

    /// `cover_b(cert)` (§4.5): the seqs of `b` at which every premise of `cert`, the untracked
    /// input included, resolves to its recorded revision.
    pub(crate) fn cover(&self, b: BranchId, cert: &Cert<E::Premises>) -> Intervals {
        let mut cover: Intervals = smallvec![Interval {
            from: self.branch(b).first,
            until: None,
        }];
        cover = intersect(&cover, &self.untracked_intervals(b, cert.key, cert.epsilon));
        for premise in cert.premises() {
            if cover.is_empty() {
                break;
            }
            cover = intersect(&cover, &self.premise_intervals(b, premise));
        }
        cover
    }

    /// The window a write installs from a nonempty cover (§4.5, §5.2): the interval containing the
    /// head, as an open window, if there is one, otherwise the most recent interval.
    pub(crate) fn pick_window(&self, b: BranchId, cover: &Intervals) -> Option<Window> {
        let last = cover.last()?;
        let head = self.branch(b).head;
        if last.contains(head) {
            debug_assert!(
                last.until.is_none(),
                "an interval reaching the head is unbounded: nothing closes at or after it"
            );
            Some(Window::open(last.from))
        } else {
            let until = last.until.expect("an unbounded interval contains the head");
            debug_assert!(until <= head);
            Some(Window::closed(last.from, until))
        }
    }
}
