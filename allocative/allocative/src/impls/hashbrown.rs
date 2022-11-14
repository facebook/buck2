/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(feature = "hashbrown")]

use std::mem;

use hashbrown::raw::RawTable;

use crate::Allocative;
use crate::Key;
use crate::Visitor;

const CAPACITY_NAME: Key = Key::new("capacity");

impl<T: Allocative> Allocative for RawTable<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        use crate::impls::common::DATA_NAME;
        use crate::impls::hashbrown_util;

        let mut visitor = visitor.enter_self_sized::<Self>();
        {
            let mut visitor = visitor.enter_unique(DATA_NAME, mem::size_of::<*const T>());
            {
                let mut visitor = visitor.enter(
                    CAPACITY_NAME,
                    hashbrown_util::raw_table_alloc_size_for_len::<T>(self.capacity()),
                );
                unsafe { visitor.visit_iter(self.iter().map(|e| e.as_ref())) };
                visitor.exit();
            }
            visitor.exit();
        }
        visitor.exit();
    }
}
