/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(feature = "triomphe")]

use std::alloc::Layout;
use std::mem;
use std::sync::atomic::AtomicUsize;

use triomphe::HeaderSlice;
use triomphe::HeaderWithLength;
use triomphe::ThinArc;

use crate::impls::common::PTR_NAME;
use crate::Allocative;
use crate::Key;
use crate::Visitor;

impl<H: Allocative, T: Allocative + ?Sized> Allocative for HeaderSlice<H, T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self(self);
        visitor.visit_field(Key::new("header"), &self.header);
        visitor.visit_field(Key::new("slice"), &self.slice);
        visitor.exit();
    }
}

impl<H: Allocative> Allocative for HeaderWithLength<H> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self(self);
        visitor.visit_field(Key::new("header"), &self.header);
        visitor.visit_field(Key::new("len"), &self.length);
        visitor.exit();
    }
}

impl<H: Allocative, T: Allocative> Allocative for ThinArc<H, T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        {
            if let Some(mut visitor) = visitor.enter_shared(
                PTR_NAME,
                mem::size_of::<*mut u8>(),
                ThinArc::ptr(self) as *const (),
            ) {
                #[repr(C)]
                struct ThinArcInnerRepr<H> {
                    _counter: AtomicUsize,
                    _header: H,
                    _len: usize,
                }
                let size = Layout::new::<ThinArcInnerRepr<H>>()
                    .extend(Layout::array::<T>(self.slice.len()).unwrap())
                    .unwrap()
                    .0
                    .size();
                {
                    let mut visitor =
                        visitor.enter(Key::for_type_name::<ThinArcInnerRepr<H>>(), size);
                    visitor.visit_field::<HeaderSlice<HeaderWithLength<H>, [T]>>(
                        Key::new("data"),
                        self,
                    );
                }
                visitor.exit();
            }
        }
        visitor.exit();
    }
}
