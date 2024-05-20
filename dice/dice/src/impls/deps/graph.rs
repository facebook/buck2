/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use allocative::Allocative;
use dupe::Dupe;
use gazebo::variants::VariantName;
use itertools::Either;
use itertools::Itertools;

use crate::impls::deps::encoding::SPEncoder;
use crate::impls::deps::iterator::SeriesNodeIterator;
use crate::impls::deps::iterator::SeriesParallelDepsIteratorItem;
use crate::impls::key::DiceKey;

/// The DiceComputations compute apis are designed so that in normal usage the graph of
/// inter-dep data dependencies within a compute node form a series-parallel graph.
///
/// The [SeriesParallelDeps] records the deps and the structure of that graph so that
/// when we recompute we can check keys in parallel but avoid requesting a key that
/// would not be requested by calling the compute directly in that state.
///
/// For non-trivial graphs, we will encode the graph as a flat list of keys and an
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
#[derive(Allocative, Debug, Eq, PartialEq)]
pub(crate) enum SeriesParallelDeps {
    None,
    /// It's very common for a parallel compute to record only a single dep and so we have an optimized case for that.
    One(DiceKey),
    /// Once a set of deps becomes non-trivial, it's represented by a SPDepsMany.
    Many(Box<SPDepsMany>),
}

impl SeriesParallelDeps {
    pub(crate) fn insert(&mut self, k: DiceKey) {
        match self {
            SeriesParallelDeps::None => *self = SeriesParallelDeps::One(k),
            SeriesParallelDeps::One(_) => self.upgrade_to_many().push(k),
            SeriesParallelDeps::Many(v) => v.push(k),
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
                let v = match v {
                    SeriesParallelDeps::One(v) => v,
                    _ => unreachable!(),
                };
                let many = self.unwrap_many_mut();
                many.push(v);
                many
            }
            SeriesParallelDeps::Many(v) => &mut *v,
        }
    }

    pub(crate) fn header(&self) -> SPSeriesHeader {
        match self {
            SeriesParallelDeps::None => SPSeriesHeader::Simple { key_count: 0 },
            SeriesParallelDeps::One(_) => SPSeriesHeader::Simple { key_count: 1 },
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

    pub(crate) fn serial_from_vec(mut vec: Vec<DiceKey>) -> SeriesParallelDeps {
        match vec.len() {
            0 => SeriesParallelDeps::None,
            1 => SeriesParallelDeps::One(vec.pop().unwrap()),
            _ => SeriesParallelDeps::Many(Box::new(SPDepsMany::serial_from_vec(vec))),
        }
    }

    pub(crate) fn iter_keys(&self) -> impl Iterator<Item = DiceKey> + '_ {
        match self {
            SeriesParallelDeps::None => Either::Left(Option::<DiceKey>::None.into_iter()),
            SeriesParallelDeps::One(v) => Either::Left(Option::<DiceKey>::Some(*v).into_iter()),
            SeriesParallelDeps::Many(m) => Either::Right(m.deps.iter().copied()),
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        match self {
            SeriesParallelDeps::None => true,
            SeriesParallelDeps::One(_) => false,
            SeriesParallelDeps::Many(many) => many.deps.is_empty(),
        }
    }

    #[allow(unused)] // TODO(cjhopman): delete this once it's used outside tests
    pub(crate) fn iter(&self) -> impl Iterator<Item = SeriesParallelDepsIteratorItem<'_>> {
        match self {
            SeriesParallelDeps::None => {
                Either::Left(Option::<SeriesParallelDepsIteratorItem>::None.into_iter())
            }
            SeriesParallelDeps::One(k) => {
                Either::Left(Some(SeriesParallelDepsIteratorItem::Key(k)).into_iter())
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
}

#[derive(Allocative, Eq, PartialEq)]
pub(crate) struct SPDepsMany {
    deps: Vec<DiceKey>,
    /// This holds the encoded series-parallel graph structure, i.e. it tells how to read the deps list as a series-parallel graph.
    spec: Vec<u32>,
    trailing_deps_start: u32,
}

impl Debug for SPDepsMany {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SeriesParallelDeps")
            .field(
                "deps",
                &format!("[{}]", self.deps.iter().map(|v| v.index).join(",")),
            )
            .field("spec", &format!("[{:?}]", &self.spec))
            .field("trailing_deps_start", &self.trailing_deps_start)
            .finish()
    }
}

impl SPDepsMany {
    fn new() -> SPDepsMany {
        Self {
            deps: Vec::new(),
            spec: Vec::new(),
            trailing_deps_start: 0,
        }
    }

    fn push(&mut self, k: DiceKey) {
        self.deps.push(k);
    }

    pub(crate) fn iter(&self) -> SeriesNodeIterator<'_> {
        SeriesNodeIterator::new(self.deps.iter(), self.spec.iter())
    }

    fn serial_from_vec(vec: Vec<DiceKey>) -> SPDepsMany {
        Self {
            deps: vec,
            spec: Vec::new(),
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
                SeriesParallelDeps::One(v) => {
                    self.deps.push(v);
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
