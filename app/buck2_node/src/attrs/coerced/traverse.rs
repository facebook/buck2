/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Dependency traversal of coerced values.

use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_core::package::PackageLabel;

use crate::attrs::arena::CoercedAttrRef;
use crate::attrs::arena::schema::View;
use crate::attrs::attr_type::AttrType;
use crate::attrs::coerced::configure::TraverseConfigured;
use crate::attrs::coerced::kinds::ValueVisitor;
use crate::attrs::coerced::kinds::dispatch_value;
use crate::attrs::coerced::semantics::Semantics;
use crate::attrs::traversal::CoercedAttrTraversal;

impl<'a> CoercedAttrRef<'a> {
    /// Type-threaded traversal over the value's deps. Callback borrows come
    /// from the arena's side tables, so they carry the arena's lifetime.
    pub fn traverse(
        &self,
        t: &AttrType,
        pkg: Option<PackageLabel>,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()> {
        let kind = self.arena.kind_of(self.r);
        if kind.is_scalar() {
            return Ok(());
        }
        dispatch_value(
            *self,
            Traverse {
                v: *self,
                t,
                pkg,
                tr: traversal,
            },
        )
    }
}

/// The operands of one value operation, as [`dispatch_value`] hands them to
/// the kind's [`Semantics`]; `v` is the value itself.
pub(crate) struct Traverse<'x, 'a> {
    pub(crate) v: CoercedAttrRef<'a>,
    pub(crate) t: &'x AttrType,
    pub(crate) pkg: Option<PackageLabel>,
    pub(crate) tr: &'x mut dyn CoercedAttrTraversal<'a>,
}

impl<'a> ValueVisitor<'a> for Traverse<'_, 'a> {
    type Out = buck2_error::Result<()>;
    fn visit<K: Semantics + 'static>(self, view: View<'a, K>) -> Self::Out {
        K::traverse(view, self)
    }
}

impl<'a> ValueVisitor<'a> for TraverseConfigured<'_, 'a> {
    type Out = ResultMaybeCompatible<()>;
    fn visit<K: Semantics + 'static>(self, view: View<'a, K>) -> Self::Out {
        K::traverse_configured(view, self)
    }
}
