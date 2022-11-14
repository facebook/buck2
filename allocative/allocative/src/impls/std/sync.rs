/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::mem;
use std::rc;
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicI32;
use std::sync::atomic::AtomicI64;
use std::sync::atomic::AtomicIsize;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::RwLock;
use std::sync::Weak;

use crate::allocative_trait::Allocative;
use crate::impls::common::PTR_NAME;
use crate::key::Key;
use crate::visitor::Visitor;

impl<T: Allocative> Allocative for RwLock<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        if let Ok(data) = self.try_read() {
            visitor.visit_field(Key::new("data"), &*data);
        }
        visitor.exit();
    }
}

impl<T: Allocative + ?Sized> Allocative for Arc<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        {
            let visitor = visitor.enter_shared(
                PTR_NAME,
                // TODO(nga): 8 or 16 depending on sized or unsized.
                mem::size_of::<*const ()>(),
                Arc::as_ptr(self) as *const (),
            );
            if let Some(mut visitor) = visitor {
                struct ArcInner(AtomicUsize, AtomicUsize, ());
                {
                    let val: &T = self;
                    let mut visitor = visitor.enter(
                        Key::new("ArcInner"),
                        mem::size_of::<ArcInner>() + mem::size_of_val(val),
                    );
                    val.visit(&mut visitor);
                    visitor.exit();
                }
                visitor.exit();
            }
        }
        visitor.exit();
    }
}

impl<T: Allocative + ?Sized> Allocative for Weak<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        {
            if let Some(arc) = self.upgrade() {
                arc.visit(&mut visitor);
            }
        }
        visitor.exit();
    }
}

impl<T: Allocative> Allocative for Rc<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        {
            let visitor = visitor.enter_shared(
                PTR_NAME,
                // TODO(nga): 8 or 16 depending on sized or unsized.
                mem::size_of::<*const ()>(),
                Rc::as_ptr(self) as *const (),
            );
            if let Some(mut visitor) = visitor {
                struct RcInner(AtomicUsize, AtomicUsize, ());
                {
                    let val: &T = self;
                    let mut visitor = visitor.enter(
                        Key::new("RcInner"),
                        mem::size_of::<RcInner>() + mem::size_of_val(val),
                    );
                    val.visit(&mut visitor);
                    visitor.exit();
                }
                visitor.exit();
            }
        }
        visitor.exit();
    }
}

impl<T: Allocative> Allocative for rc::Weak<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        {
            if let Some(rc) = self.upgrade() {
                rc.visit(&mut visitor);
            }
        }
        visitor.exit();
    }
}

impl Allocative for AtomicU32 {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        visitor.enter_self_sized::<Self>().exit();
    }
}

impl Allocative for AtomicU64 {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        visitor.enter_self_sized::<Self>().exit();
    }
}

impl Allocative for AtomicUsize {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        visitor.enter_self_sized::<Self>().exit();
    }
}

impl Allocative for AtomicI32 {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        visitor.enter_self_sized::<Self>().exit();
    }
}

impl Allocative for AtomicI64 {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        visitor.enter_self_sized::<Self>().exit();
    }
}

impl Allocative for AtomicBool {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        visitor.enter_self_sized::<Self>().exit();
    }
}

impl Allocative for AtomicIsize {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        visitor.enter_self_sized::<Self>().exit();
    }
}

impl<T: Allocative> Allocative for Mutex<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        if let Ok(data) = self.try_lock() {
            visitor.visit_field(Key::new("data"), &*data);
        }
        visitor.exit();
    }
}
