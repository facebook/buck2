/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(feature = "dashmap")]

use std::hash::Hash;
use std::mem;

use dashmap::DashMap;

use crate::allocative_trait::Allocative;
use crate::key::Key;
use crate::visitor::Visitor;

impl<K: Allocative + Eq + Hash, V: Allocative> Allocative for DashMap<K, V> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        let entries = self.iter();
        let mut visitor2 = visitor.enter_unique(Key::new("data"), mem::size_of::<*const ()>());
        for entry in entries {
            visitor2.visit_field(Key::new("key"), entry.key());
            visitor2.visit_field(Key::new("value"), entry.value());
        }
        visitor2.exit();
        visitor.exit();
    }
}
