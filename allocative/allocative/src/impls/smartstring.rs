/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(feature = "smartstring")]

use smartstring::SmartString;
use smartstring::SmartStringMode;

use crate::allocative_trait::Allocative;
use crate::impls::common::PTR_NAME;
use crate::impls::common::UNUSED_CAPACITY_NAME;
use crate::key::Key;
use crate::visitor::Visitor;

impl<M: SmartStringMode + 'static> Allocative for SmartString<M> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        if !self.is_inline() {
            let mut visitor = visitor.enter_unique(PTR_NAME, std::mem::size_of::<*const u8>());
            visitor.visit_simple(Key::new("str"), self.len());
            let unused_capacity = self.capacity() - self.len();
            visitor.visit_simple(UNUSED_CAPACITY_NAME, unused_capacity);
        }
        visitor.exit();
    }
}
