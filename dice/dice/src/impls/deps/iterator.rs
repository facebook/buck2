/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::impls::deps::encoding::SPDecoder;
use crate::impls::deps::graph::SPItem;
use crate::impls::deps::graph::SPSeriesHeader;
use crate::impls::key::DiceKey;

#[allow(unused)]
pub(crate) enum SeriesParallelDepsIteratorItem<'a> {
    Key(&'a DiceKey),
    Parallel(ParallelNodeIterator<'a>),
}

/// Data and some utility functions used by both a SeriesNodeIterator and a ParallelNodeIterator
struct IteratorData<'a> {
    deps: std::slice::Iter<'a, DiceKey>,
    specs: SPDecoder<'a>,
}

pub(crate) struct SeriesNodeIterator<'a> {
    data: IteratorData<'a>,
    keys_to_next_spec: u32,
}

impl SeriesNodeIterator<'_> {
    pub(crate) fn new<'a>(
        deps: std::slice::Iter<'a, DiceKey>,
        specs: std::slice::Iter<'a, u32>,
    ) -> SeriesNodeIterator<'a> {
        SeriesNodeIterator {
            data: IteratorData {
                deps,
                specs: SPDecoder(specs),
            },
            keys_to_next_spec: 0,
        }
    }
}

impl<'a> IteratorData<'a> {
    fn split_at(&mut self, keys: u32, specs: u32) -> IteratorData<'a> {
        let remaining_keys = self.deps.as_slice();
        let (split_keys, remaining_keys) = remaining_keys.split_at(keys as usize);
        self.deps = remaining_keys.iter();

        let split_specs = self.specs.split_at(specs);
        IteratorData {
            deps: split_keys.iter(),
            specs: split_specs,
        }
    }
}

impl<'a> Iterator for SeriesNodeIterator<'a> {
    type Item = SeriesParallelDepsIteratorItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.keys_to_next_spec == 0 {
            match self.data.specs.read_item().unwrap() {
                Some(SPItem::Keys { key_count }) => {
                    self.keys_to_next_spec = key_count;
                }
                Some(SPItem::Parallel {
                    key_count,
                    spec_count,
                }) => {
                    let parallel_data = self.data.split_at(key_count, spec_count);
                    return Some(SeriesParallelDepsIteratorItem::Parallel(
                        ParallelNodeIterator {
                            data: parallel_data,
                        },
                    ));
                }
                None => {
                    // drain the trailing keys
                    self.keys_to_next_spec = u32::MAX;
                }
            }
        }
        self.keys_to_next_spec -= 1;
        self.data
            .deps
            .next()
            .map(SeriesParallelDepsIteratorItem::Key)
    }
}

pub(crate) struct ParallelNodeIterator<'a> {
    data: IteratorData<'a>,
}

impl<'a> Iterator for ParallelNodeIterator<'a> {
    type Item = SeriesNodeIterator<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (next_series_keys, next_series_specs) =
            match self.data.specs.read_series_header().unwrap() {
                None => {
                    return None;
                }
                Some(SPSeriesHeader::Complex {
                    key_count,
                    spec_count,
                }) => (key_count, spec_count),
                Some(SPSeriesHeader::Simple { key_count }) => (key_count, 0),
            };

        let next_series_data = self.data.split_at(next_series_keys, next_series_specs);
        Some(SeriesNodeIterator {
            data: next_series_data,
            keys_to_next_spec: 0,
        })
    }
}
