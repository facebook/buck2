/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Debug;

use allocative::Allocative;
use dupe::Dupe;
use gazebo::variants::VariantName;
use itertools::Either;
use itertools::Itertools;
use mini_vec::MiniVec;

use crate::arc::Arc;
use crate::core::graph::revision::Revision;
use crate::deps::encoding::SPEncoder;
use crate::deps::iterator::SeriesNodeIterator;
use crate::deps::iterator::SeriesParallelDepsIteratorItem;
use crate::key::DiceKey;

/// One recorded dep edge: the dep's key plus the revision of the value the compute
/// observed for it. `None` means the dep was transient at the time of the compute;
/// a transient value has no interned identity, so an edge to one never revalidates.
#[derive(Copy, Clone, Dupe, Debug, Allocative)]
pub(crate) struct DepEdge {
    pub(crate) key: DiceKey,
    pub(crate) revision: Option<Revision>,
}

impl DepEdge {
    pub(crate) fn new(key: DiceKey, revision: Option<Revision>) -> Self {
        Self { key, revision }
    }
}

/// The DiceComputations compute apis are designed so that in normal usage the graph of
/// inter-dep data dependencies within a compute node form a series-parallel graph.
///
/// The [SeriesParallelDeps] records the deps and the structure of that graph so that
/// when we recompute we can check keys in parallel but avoid requesting a key that
/// would not be requested by calling the compute directly in that state. Each dep is
/// recorded as a [`DepEdge`], i.e. together with the revision the compute observed, so
/// that a dep check can decide "same value as before?" by revision equality.
///
/// For non-trivial graphs, we will encode the graph as a flat list of edges and an
/// encoding of the description of the graph.
///
/// The `SeriesNodeIterator` and `ParallelNodeIterator` provide a fairly readable
/// implementation of decoding and traversing the graph.
///
/// A Series node is a sequence of keys and parallel nodes. It's encoding is a sequence of two items:
///   SPItem::Keys(v): Indicates that the next v keys are part of this series
///   SPItem::Parallel{keys: x, specs: y}: Indicates that the next `x` keys and `y` specs form a parallel node
///
/// If a Series node has remaining keys not covered by its spec, the remaining keys are all part of the series.
///
/// A Parallel node is a set of Series nodes. A parallel node of series nodes S1, S2, and S3 will have:
///
/// ```ignore
/// keys = keys(S1) + keys(S2) + keys(S3)
/// spec = header(S1) + spec(S1) + header(S2) + spec(S2) + header(S3) + spec(S3)
/// ```
///
/// Where `spec(S1)` is the encoding of a series Node as described above and `header(S1)` is a SPSeriesHeader, which
/// is one of two items:
///   SPSeriesHeader::Simple(n): Indicates the series has no nested parallel nodes and is just a series of n keys (and 0 specs).
///   SPSeriesHeader::Complex{keys: x, specs: y}: Indicates a complex series that covers the next x keys and y specs.
///
/// For both SPItem::Parallel and SPSeriesHeader::Complex, the specs value is the size of the encoded specs.
#[derive(Allocative, Debug)]
pub(crate) enum SeriesParallelDeps {
    None,
    /// It's very common for a parallel compute to record only a single dep and so we have an optimized case for that.
    One(DepEdge),
    /// Once a set of deps becomes non-trivial, it's represented by a SPDepsMany.
    Many(Box<SPDepsMany>),
}

impl SeriesParallelDeps {
    pub(crate) fn insert(&mut self, edge: DepEdge) {
        match self {
            SeriesParallelDeps::None => *self = SeriesParallelDeps::One(edge),
            SeriesParallelDeps::One(..) => self.upgrade_to_many().push(edge),
            SeriesParallelDeps::Many(v) => v.push(edge),
        }
    }

    fn upgrade_to_many(&mut self) -> &mut SPDepsMany {
        match self {
            SeriesParallelDeps::None => {
                *self = SeriesParallelDeps::Many(Box::new(SPDepsMany::new()));
                self.unwrap_many_mut()
            }
            SeriesParallelDeps::One(..) => {
                let v =
                    std::mem::replace(self, SeriesParallelDeps::Many(Box::new(SPDepsMany::new())));
                let edge = match v {
                    SeriesParallelDeps::One(edge) => edge,
                    _ => unreachable!(),
                };
                let many = self.unwrap_many_mut();
                many.push(edge);
                many
            }
            SeriesParallelDeps::Many(v) => &mut *v,
        }
    }

    pub(crate) fn header(&self) -> SPSeriesHeader {
        match self {
            SeriesParallelDeps::None => SPSeriesHeader::Simple { key_count: 0 },
            SeriesParallelDeps::One(..) => SPSeriesHeader::Simple { key_count: 1 },
            SeriesParallelDeps::Many(many) => {
                if many.spec.is_empty() {
                    SPSeriesHeader::Simple {
                        key_count: many.deps.len().try_into().unwrap(),
                    }
                } else {
                    SPSeriesHeader::Complex {
                        key_count: many.deps.len().try_into().unwrap(),
                        spec_count: many.spec.len().try_into().unwrap(),
                    }
                }
            }
        }
    }

    fn unwrap_many_mut(&mut self) -> &mut SPDepsMany {
        match self {
            SeriesParallelDeps::Many(v) => &mut *v,
            _ => panic!(),
        }
    }

    /// A serial dep list whose edges record no revision. An edge without a revision
    /// never revalidates, so tests using this must not rely on revalidation succeeding
    /// across these deps.
    #[cfg(test)]
    pub(crate) fn testing_serial_from(vec: Vec<DiceKey>) -> SeriesParallelDeps {
        Self::serial_from_edges(vec.into_iter().map(|k| DepEdge::new(k, None)).collect())
    }

    pub(crate) fn serial_from_edges(mut edges: Vec<DepEdge>) -> SeriesParallelDeps {
        match edges.len() {
            0 => SeriesParallelDeps::None,
            1 => SeriesParallelDeps::One(edges.pop().unwrap()),
            _ => SeriesParallelDeps::Many(Box::new(SPDepsMany::serial_from_edges(edges))),
        }
    }

    pub(crate) fn iter_keys(&self) -> impl Iterator<Item = DiceKey> + '_ {
        self.iter_edges().map(|e| e.key)
    }

    /// Flat iteration over the dep edges, discarding the series-parallel structure.
    pub(crate) fn iter_edges(&self) -> impl Iterator<Item = DepEdge> + '_ {
        match self {
            SeriesParallelDeps::None => Either::Left(None.into_iter()),
            SeriesParallelDeps::One(edge) => Either::Left(Some(*edge).into_iter()),
            SeriesParallelDeps::Many(m) => Either::Right(m.deps.iter().copied()),
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        match self {
            SeriesParallelDeps::None => true,
            SeriesParallelDeps::One(..) => false,
            SeriesParallelDeps::Many(many) => many.deps.is_empty(),
        }
    }

    /// Same series-parallel shape and same keys in the same order, ignoring the per-edge
    /// revisions: whether a recompute read the same deps, whatever their values were.
    pub(crate) fn equal_ignoring_revisions(&self, other: &Self) -> bool {
        match (self, other) {
            (SeriesParallelDeps::None, SeriesParallelDeps::None) => true,
            (SeriesParallelDeps::One(a), SeriesParallelDeps::One(b)) => a.key == b.key,
            (SeriesParallelDeps::Many(a), SeriesParallelDeps::Many(b)) => {
                a.equal_ignoring_revisions(b)
            }
            _ => false,
        }
    }

    /// [`Self::equal_ignoring_revisions`] and every edge records the same revision on both
    /// sides: the two are traces of the same computational circumstances. An edge without a
    /// revision never matches, a transient having no identity to compare.
    pub(crate) fn equal_with_revisions(&self, other: &Self) -> bool {
        self.equal_ignoring_revisions(other)
            && self
                .iter_edges()
                .zip(other.iter_edges())
                .all(|(a, b)| a.revision.is_some() && a.revision == b.revision)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = SeriesParallelDepsIteratorItem<'_>> {
        match self {
            SeriesParallelDeps::None => {
                Either::Left(Option::<SeriesParallelDepsIteratorItem>::None.into_iter())
            }
            SeriesParallelDeps::One(edge) => {
                Either::Left(Some(SeriesParallelDepsIteratorItem::Key(*edge)).into_iter())
            }
            SeriesParallelDeps::Many(v) => Either::Right(v.iter()),
        }
    }

    pub(crate) fn insert_parallel(
        &mut self,
        parallel: impl Iterator<Item = Self>,
        new_keys: u32,
        new_specs: u32,
    ) {
        self.upgrade_to_many()
            .insert_parallel(parallel, new_keys, new_specs);
    }

    pub(crate) fn into_arc(mut self) -> Arc<Self> {
        if let SeriesParallelDeps::Many(many) = &mut self {
            many.deps.shrink_to_fit();
            many.spec.shrink_to_fit();
        }
        Arc::new(self)
    }
}

#[derive(Allocative)]
pub(crate) struct SPDepsMany {
    deps: MiniVec<DepEdge>,
    /// Encoded series-parallel graph structure — tells how to read the deps
    /// list as a series-parallel graph.
    spec: MiniVec<u32>,
    trailing_deps_start: u32,
}

impl Debug for SPDepsMany {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SeriesParallelDeps")
            .field(
                "deps",
                &format!("[{}]", self.deps.iter().map(|e| e.key.index).join(",")),
            )
            .field("spec", &format!("[{:?}]", self.spec))
            .field("trailing_deps_start", &self.trailing_deps_start)
            .finish()
    }
}

impl SPDepsMany {
    /// See [`SeriesParallelDeps::equal_ignoring_revisions`].
    fn equal_ignoring_revisions(&self, other: &Self) -> bool {
        if self.spec != other.spec || self.trailing_deps_start != other.trailing_deps_start {
            return false;
        }
        if self.deps.len() != other.deps.len() {
            return false;
        }
        self.deps
            .iter()
            .zip(other.deps.iter())
            .all(|(a, b)| a.key == b.key)
    }

    fn new() -> SPDepsMany {
        Self {
            deps: MiniVec::new(),
            spec: MiniVec::new(),
            trailing_deps_start: 0,
        }
    }

    fn push(&mut self, edge: DepEdge) {
        self.deps.push(edge);
    }

    pub(crate) fn iter(&self) -> SeriesNodeIterator<'_> {
        SeriesNodeIterator::new(self.deps.iter(), self.spec.iter())
    }

    fn serial_from_edges(edges: Vec<DepEdge>) -> SPDepsMany {
        Self {
            deps: edges.into(),
            spec: MiniVec::new(),
            trailing_deps_start: 0,
        }
    }

    fn insert_parallel(
        &mut self,
        parallel: impl Iterator<Item = SeriesParallelDeps>,
        new_keys: u32,
        new_specs: u32,
    ) {
        let trailing_keys = (self.deps.len() - (self.trailing_deps_start as usize))
            .try_into()
            .unwrap();
        let trailing_keys = match trailing_keys {
            0 => None,
            n => Some(SPItem::Keys { key_count: n }),
        };
        let parallel_item = SPItem::Parallel {
            key_count: new_keys.try_into().unwrap(),
            spec_count: new_specs.try_into().unwrap(),
        };

        let mut total_new_specs = new_specs;
        if let Some(v) = &trailing_keys {
            total_new_specs += v.encoded_len();
        }
        total_new_specs += parallel_item.encoded_len();
        let total_new_specs = total_new_specs as usize;

        self.deps.reserve(new_keys as usize);
        let expected_total_specs = self.spec.len() + total_new_specs;
        self.spec.reserve(total_new_specs);

        if let Some(v) = trailing_keys {
            self.spec.write_item(v)
        }
        self.spec.write_item(parallel_item);

        for dep in parallel {
            self.spec.write_series_header(dep.header());
            match dep {
                SeriesParallelDeps::None => {}
                SeriesParallelDeps::One(edge) => {
                    self.deps.push(edge);
                }
                SeriesParallelDeps::Many(other) => {
                    self.spec.extend(other.spec);
                    self.deps.extend(other.deps);
                }
            }
        }

        assert_eq!(self.spec.len(), expected_total_specs);
        self.trailing_deps_start = self.deps.len().try_into().unwrap();
    }
}

/// SPSeriesHeader describes the total encoded size of a series node.
#[derive(Debug, Copy, Clone, Dupe, VariantName, Eq, PartialEq)]
pub(crate) enum SPSeriesHeader {
    /// A series node with no parallel children.
    Simple { key_count: u32 },
    /// A series node with parallel children. key_count and spec_count here indicate the full size of this encoded series node and all of its (transitive) children.
    Complex { key_count: u32, spec_count: u32 },
}

impl SPSeriesHeader {
    pub(crate) fn keys_len(&self) -> u32 {
        match self {
            SPSeriesHeader::Simple { key_count } => *key_count,
            SPSeriesHeader::Complex { key_count, .. } => *key_count,
        }
    }
}

/// SPItem represents part of a series node. A series node consists of a sequence of keys and parallel nodes, and a "part" is
/// either a set of sequential keys or a single parallel node.
#[derive(Debug, Copy, Clone, Dupe, VariantName, Eq, PartialEq)]
pub(crate) enum SPItem {
    Keys {
        key_count: u32,
    },
    /// The key_count and spec_count here are the full size of the encoded parallel node and all of its (transitive) children.
    Parallel {
        key_count: u32,
        spec_count: u32,
    },
}
