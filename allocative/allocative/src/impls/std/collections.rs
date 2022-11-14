/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;

use crate::allocative_trait::Allocative;
use crate::impls::common::visit_generic_map;
use crate::impls::common::visit_generic_set;
use crate::visitor::Visitor;

impl<K: Allocative, V: Allocative> Allocative for BTreeMap<K, V> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        visit_generic_map(&mut visitor, self);
        visitor.exit();
    }
}

impl<K: Allocative> Allocative for BTreeSet<K> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        visit_generic_set(&mut visitor, self);
        visitor.exit();
    }
}

impl<K: Allocative, V: Allocative> Allocative for HashMap<K, V> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        // TODO: can do better extra capacity.
        visit_generic_map(&mut visitor, self);
        visitor.exit();
    }
}

impl<K: Allocative> Allocative for HashSet<K> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        // TODO: can do better extra capacity.
        visit_generic_set(&mut visitor, self);
        visitor.exit();
    }
}
