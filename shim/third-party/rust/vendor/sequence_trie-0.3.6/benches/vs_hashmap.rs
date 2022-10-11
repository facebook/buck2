#![feature(test)]
extern crate test;
extern crate sequence_trie;

use std::collections::HashMap;
use test::Bencher;
use sequence_trie::SequenceTrie;

macro_rules! u32_benchmark {
    ($map_constructor: expr, $test_id: ident, $num_keys: expr, $key_length: expr) => (
        #[bench]
        fn $test_id(b: &mut Bencher) {
            let mut test_data = Vec::<Vec<u32>>::with_capacity($num_keys);
            let mut map = $map_constructor;
            for i in 0 .. $num_keys {
                let mut key = Vec::<u32>::with_capacity($key_length);
                for j in 0 .. $key_length {
                    key.push(i * j);
                }

                test_data.push(key);
            }

            b.iter(|| {
                for key in &test_data {
                    map.insert(&key[..], 7u32);
                }
            });
        }
    )
}

u32_benchmark! { HashMap::new(), hashmap_k1024_l16, 1024, 16 }
u32_benchmark! { SequenceTrie::new(), trie_k1024_l16, 1024, 16 }

u32_benchmark! { HashMap::new(), hashmap_k64_l128, 64, 128 }
u32_benchmark! { SequenceTrie::new(), trie_k64_l128, 64, 128 }
