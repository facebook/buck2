/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;

use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use hashbrown::raw::RawTable;

use crate::attrs::coerce::str_hash::str_hash;

pub(crate) struct ArcStrInterner {
    cache: RefCell<RawTable<(u64, ArcStr)>>,
}

impl ArcStrInterner {
    pub(crate) fn new() -> ArcStrInterner {
        ArcStrInterner {
            cache: RefCell::new(RawTable::new()),
        }
    }

    pub(crate) fn intern(&self, s: &str) -> ArcStr {
        if s.is_empty() {
            return ArcStr::default();
        }

        let hash = str_hash(s);
        let mut cache = self.cache.borrow_mut();

        if let Some((_h, v)) = cache.get(hash, |(_h, v)| s == v.as_str()) {
            return v.dupe();
        }

        let value = ArcStr::from(s);
        cache.insert(hash, (hash, value.dupe()), |(h, _v)| *h);
        value
    }
}

#[cfg(test)]
mod tests {
    use std::ptr;

    use crate::attrs::coerce::arc_str_interner::ArcStrInterner;

    #[test]
    fn test_arc_str_interner() {
        let interner = ArcStrInterner::new();
        let foo0 = interner.intern("foo");
        let foo1 = interner.intern("foo");

        assert!(ptr::eq(foo0.as_ptr(), foo1.as_ptr()));

        let bar0 = interner.intern("bar");
        let bar1 = interner.intern("bar");

        assert!(ptr::eq(bar0.as_ptr(), bar1.as_ptr()));
    }
}
