/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::{BTreeMap, HashMap};

use minibench::{bench, elapsed};
use sorted_vector_map::SortedVectorMap;

static WORDS: &[&str] = &[
    "cat",
    "goldfish",
    "dog",
    "badger",
    "porcupine",
    "hedgehog",
    "terrapin",
    "bird",
    "squirrel",
    "wombat",
];

#[inline(never)]
fn consume<T>(_t: T) {}

fn build_map_data(start: usize, size: usize) -> Vec<(String, usize)> {
    let mut index = 0;
    let mut data = Vec::with_capacity(size);
    for n in start.. {
        for word1 in WORDS.iter() {
            for word2 in WORDS.iter() {
                data.push((format!("{}.{}.{}", word1, n, word2), index));
                index += 1;
                if index >= size {
                    return data;
                }
            }
        }
    }
    unreachable!()
}

macro_rules! make_map_bench {
    ($name:ident, $map:ident, [ $(,)? ]) => {};
    ($name:ident, $map:ident, [ $(,)? $count:literal $( $counts:tt )* ]) => {
        let mut data = build_map_data(0, $count);
        bench(concat!(stringify!($name), " (", stringify!($count), ") build unordered"), || {
            elapsed(|| { consume(data.iter().cloned().collect::<$map<_, _>>()); })
        });

        data.sort();
        bench(concat!(stringify!($name), " (", stringify!($count), ") build ordered"), || {
            elapsed(|| { consume(data.iter().cloned().collect::<$map<_, _>>()); })
        });

        let map = data.iter().cloned().collect::<$map<_, _>>();
        bench(concat!(stringify!($name), " (", stringify!($count), ") iterate"), || {
            elapsed(|| map.iter().for_each(|item| { consume(item); }))
        });

        let mut get_keys = Vec::with_capacity(1000);
        for n in 0..10 {
            for word1 in WORDS.iter() {
                for word2 in WORDS.iter() {
                    get_keys.push(format!("{}.{}.{}", word1, n, word2));
                }
            }
        }
        bench(concat!(stringify!($name), " (", stringify!($count), ") get-thousand"), || {
            elapsed(|| {
                for key in get_keys.iter() {
                    consume(map.get(key));
                }
            })
        });

        let mut insert_pairs = Vec::with_capacity(1000);
        for n in 0..10 {
            for word1 in WORDS.iter() {
                for word2 in WORDS.iter() {
                    insert_pairs.push((format!("{}.{}.{}.additional", word1, n, word2), n));
                }
            }
        }
        bench(concat!(stringify!($name), " (", stringify!($count), ") insert-thousand"), || {
            let mut map = map.clone();
            elapsed(|| {
                for (key, value) in insert_pairs.iter().cloned() {
                    map.insert(key, value);
                }
            })
        });

        let more_data = build_map_data($count, $count);
        bench(concat!(stringify!($name), " (", stringify!($count), ") extend"), || {
            let mut map = map.clone();
            elapsed(|| {
                map.extend(more_data.iter().cloned());
            })
        });

        make_map_bench!($name, $map, [$( $counts )*]);
    };
}

fn main() {
    make_map_bench!(sorted_vector_map, SortedVectorMap, [1000, 10000, 100000]);
    make_map_bench!(btreemap, BTreeMap, [1000, 10000, 100000]);
    make_map_bench!(hashmap, HashMap, [1000, 10000, 100000]);
}
