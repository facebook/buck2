/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_error::BuckErrorContext;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::values::starlark_value;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOfUncheckedGeneric;

use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetError;

#[derive(Debug, Clone, Dupe, Copy, Trace, Freeze, PartialEq, Allocative)]
pub enum TransitiveSetOrdering {
    /// Preorder traversal, a good default behavior which traverses depth-first returning the current node, and then its children left-to-right.
    Preorder,
    /// Postorder traversal, which traverses depth-first, first for the children left-to-right, recursively, then returning the node itself.
    Postorder,
    /// Topological sort, such that nodes are listed after all nodes that have them as descendants.
    Topological,
    /// Breadth-first search traversal.
    Bfs,
}

impl TransitiveSetOrdering {
    pub fn parse(s: &str) -> buck2_error::Result<TransitiveSetOrdering> {
        // NOTE: If this list is updated, update the OrderingUnexpectedValue error text.
        match s {
            "preorder" => Ok(Self::Preorder),
            "postorder" => Ok(Self::Postorder),
            "topological" => Ok(Self::Topological),
            "bfs" => Ok(Self::Bfs),
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
    Coerce,
    Freeze,
    Display,
    ProvidesStaticType,
    NoSerialize,
    Allocative
)]
#[display("Traversal({})", inner)]
#[repr(C)]
pub struct TransitiveSetTraversalGen<V: ValueLifetimeless> {
    pub(super) inner: V,
    pub ordering: TransitiveSetOrdering,
}

starlark_complex_value!(pub TransitiveSetTraversal);

#[starlark_value(type = "transitive_set_iterator")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for TransitiveSetTraversalGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn iterate_collect(&self, _heap: &'v Heap) -> starlark::Result<Vec<Value<'v>>> {
        let tset =
            TransitiveSet::from_value(self.inner.to_value()).buck_error_context("Invalid inner")?;
        Ok(tset.iter_values(self.ordering)?.collect())
    }
}

/// The type returned from .traverse() on a tset projection. This is shared by multiple types of
/// projection (as the traversals all just iterate over the projected values).
#[derive(
    Debug,
    Clone,
    Trace,
    Coerce,
    Freeze,
    Display,
    ProvidesStaticType,
    NoSerialize,
    Allocative
)]
#[display("Traversal({}[\"{}\"])", transitive_set, projection)]
#[repr(C)]
pub struct TransitiveSetProjectionTraversalGen<V: ValueLifetimeless> {
    pub(super) transitive_set: ValueOfUncheckedGeneric<V, FrozenTransitiveSet>,
    pub projection: usize,
    pub ordering: TransitiveSetOrdering,
}

starlark_complex_value!(pub TransitiveSetProjectionTraversal);

#[starlark_value(type = "transitive_set_args_projection_iterator")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for TransitiveSetProjectionTraversalGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn iterate_collect(&self, _heap: &'v Heap) -> starlark::Result<Vec<Value<'v>>> {
        let set = TransitiveSet::from_value(self.transitive_set.get().to_value())
            .buck_error_context("Invalid inner")?;
        Ok(set
            .iter_projection_values(self.ordering, self.projection)?
            .collect())
    }
}
