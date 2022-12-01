/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;

use crate::impls::common::DATA_NAME;
use crate::Allocative;
use crate::Visitor;

impl<T: Allocative> Allocative for RefCell<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        if let Ok(v) = self.try_borrow() {
            visitor.visit_field(DATA_NAME, &*v);
        }
        visitor.exit();
    }
}
