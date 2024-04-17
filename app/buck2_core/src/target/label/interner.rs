/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Formatter;
use std::hash::BuildHasher;

use allocative::Allocative;
use buck2_util::hash::BuckHasherBuilder;
use lock_free_hashtable::sharded::ShardedLockFreeRawTable;

use crate::target::label::label::TargetLabel;

/// Concurrent target label interner.
#[derive(Default, Allocative)]
pub struct ConcurrentTargetLabelInterner {
    table: ShardedLockFreeRawTable<TargetLabel, 64>,
}

impl Debug for ConcurrentTargetLabelInterner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConcurrentTargetLabelInterner")
            .finish_non_exhaustive()
    }
}

impl PartialEq for ConcurrentTargetLabelInterner {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl ConcurrentTargetLabelInterner {
    pub fn intern(&self, target_label: TargetLabel) -> TargetLabel {
        let hash = BuckHasherBuilder.hash_one(&target_label);

        if let Some(r) = self
            .table
            .lookup(hash, |entry_ref| entry_ref == target_label.arc_borrow())
        {
            return r.to_owned();
        }

        let (entry, _) = self.table.insert(
            hash,
            target_label,
            |a, b| a == b,
            |a| BuckHasherBuilder.hash_one(a),
        );
        entry.to_owned()
    }
}

#[cfg(test)]
mod tests {
    use std::ptr;

    use crate::target::label::interner::ConcurrentTargetLabelInterner;
    use crate::target::label::label::TargetLabel;

    #[test]
    fn test_interner() {
        let interner = ConcurrentTargetLabelInterner::default();

        let label1 = interner.intern(TargetLabel::testing_parse("foo//:bar"));
        let label2 = interner.intern(TargetLabel::testing_parse("foo//:bar"));
        assert!(ptr::eq(label1.as_raw(), label2.as_raw()));

        // We would like to check refcount, but there's no public API for that.
    }
}
