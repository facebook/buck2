/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use allocative::Allocative;
use allocative::Key;
use allocative::Visitor;
use sequence_trie::SequenceTrie;

pub(crate) fn visit_sequence_trie<K: Allocative + Eq + Hash, V: Allocative>(
    t: &SequenceTrie<K, V>,
    visitor: &mut Visitor<'_>,
) {
    let mut visitor = visitor.enter_self_sized::<SequenceTrie<K, V>>();
    if let Some(root) = t.get([]) {
        visitor.visit_field(Key::new("root"), root);
    }
    visitor.visit_generic_map_fields(t.iter().filter_map(|(mut k, v)| Some((k.pop()?, v))));
    visitor.exit();
}
