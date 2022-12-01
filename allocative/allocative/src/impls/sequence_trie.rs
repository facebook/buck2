/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(feature = "sequence_trie")]

use std::hash::Hash;

use sequence_trie::SequenceTrie;

use crate::allocative_trait::Allocative;
use crate::impls::common::visit_generic_map;
use crate::key::Key;
use crate::visitor::Visitor;

impl<K: Allocative + Eq + Hash, V: Allocative> Allocative for SequenceTrie<K, V> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        if let Some(root) = self.get([]) {
            visitor.visit_field(Key::new("root"), root);
        }
        visit_generic_map(
            &mut visitor,
            self.iter().filter_map(|(mut k, v)| Some((k.pop()?, v))),
        );
        visitor.exit();
    }
}
