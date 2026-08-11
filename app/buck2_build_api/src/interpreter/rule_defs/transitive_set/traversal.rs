/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_error::internal_error;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::values::FreezeBranded;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkPagable;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueOfUnchecked;
use starlark::values::starlark_value;

use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetError;

#[derive(
    Debug,
    Clone,
    Dupe,
    Copy,
    Trace,
    FreezeBranded,
    PartialEq,
    Allocative,
    StarlarkPagable
)]
pub enum TransitiveSetOrdering {
    /// Preorder depth-first traversal, visiting parent node first, then children in an unspecified
    /// order that minimizes memory usage during traversal.
    Preorder,
    /// Postorder depth-first traversal, visiting children left-to-right before visiting their
    /// parent node.
    Postorder,
    /// Topological sort order, such that nodes are visited after all nodes that have them as
    /// descendants. This is similar to the pre-order traversal, except that when nodes are shared
    /// with more than one parent it is returned in the order of its last occurrence.
    Topological,
    /// Preorder breadth-first-search (BFS), visits parent node, then eagerly visits all children
    /// left-to-right before traversing to any grandchildren.
    Bfs,
    /// Preorder depth-first-search (DFS). This is similar to the pre-order traversal, except that
    /// children are guaranteed to be visited left-to-right.
    Dfs,
}

impl TransitiveSetOrdering {
    pub fn parse(s: &str) -> buck2_error::Result<TransitiveSetOrdering> {
        // NOTE: If this list is updated, update the OrderingUnexpectedValue error text.
        match s {
            "preorder" => Ok(Self::Preorder),
            "postorder" => Ok(Self::Postorder),
            "topological" => Ok(Self::Topological),
            "bfs" => Ok(Self::Bfs),
            "dfs" => Ok(Self::Dfs),
            _ => Err(TransitiveSetError::OrderingUnexpectedValue {
                ordering: s.to_owned(),
            }
            .into()),
        }
    }
}

#[derive(
    Debug,
    Clone,
    Trace,
    FreezeBranded,
    Display,
    ProvidesStaticType,
    NoSerialize,
    Allocative,
    StarlarkPagable
)]
#[display("Traversal({})", inner)]
#[repr(C)]
pub struct TransitiveSetTraversal<'v> {
    pub(super) inner: Value<'v>,
    pub ordering: TransitiveSetOrdering,
}

starlark_complex_value_branded!(pub TransitiveSetTraversal);

#[starlark_value(type = "TransitiveSetIterator")]
impl<'v> StarlarkValue<'v> for TransitiveSetTraversal<'v> {
    fn iterate_collect(&self, _heap: Heap<'v>) -> starlark::Result<Vec<Value<'v>>> {
        let tset = TransitiveSet::from_value(self.inner.to_value())
            .ok_or_else(|| internal_error!("Invalid inner"))?;
        Ok(tset.iter_values(self.ordering)?.collect())
    }
}

/// The type returned from .traverse() on a tset projection. This is shared by multiple types of
/// projection (as the traversals all just iterate over the projected values).
#[derive(
    Debug,
    Clone,
    Trace,
    FreezeBranded,
    Display,
    ProvidesStaticType,
    NoSerialize,
    Allocative,
    StarlarkPagable
)]
#[display("Traversal({}[\"{}\"])", transitive_set, projection)]
#[repr(C)]
pub struct TransitiveSetProjectionTraversal<'v> {
    pub(super) transitive_set: ValueOfUnchecked<'v, FrozenTransitiveSet>,
    pub projection: usize,
    pub ordering: TransitiveSetOrdering,
}

starlark_complex_value_branded!(pub TransitiveSetProjectionTraversal);

#[starlark_value(type = "TransitiveSetArgsProjectionIterator")]
impl<'v> StarlarkValue<'v> for TransitiveSetProjectionTraversal<'v> {
    fn iterate_collect(&self, _heap: Heap<'v>) -> starlark::Result<Vec<Value<'v>>> {
        let set = TransitiveSet::from_value(self.transitive_set.get().to_value())
            .ok_or_else(|| internal_error!("Invalid inner"))?;
        Ok(set
            .iter_projection_values(self.ordering, self.projection)?
            .collect())
    }
}
