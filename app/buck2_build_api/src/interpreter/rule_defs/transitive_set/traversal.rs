/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use anyhow::Context as _;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;

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
    pub fn parse(s: &str) -> anyhow::Result<TransitiveSetOrdering> {
        // NOTE: If this list is updated, update the OrderingUnexpectedValue error text.
        match s {
            "preorder" => Ok(Self::Preorder),
            "postorder" => Ok(Self::Postorder),
            "topological" => Ok(Self::Topological),
            "bfs" => Ok(Self::Bfs),
            _ => Err(anyhow::anyhow!(
                TransitiveSetError::OrderingUnexpectedValue {
                    ordering: s.to_owned()
                }
            )),
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
#[display(fmt = "Traversal({})", inner)]
#[repr(C)]
pub struct TransitiveSetTraversalGen<V> {
    pub(super) inner: V,
    pub ordering: TransitiveSetOrdering,
}

starlark_complex_value!(pub TransitiveSetTraversal);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for TransitiveSetTraversalGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    starlark_type!("transitive_set_iterator");

    fn iterate_collect(&self, _heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        let tset = TransitiveSet::from_value(self.inner.to_value()).context("Invalid inner")?;
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
#[display(fmt = "Traversal({}[\"{}\"])", transitive_set, projection)]
#[repr(C)]
pub struct TransitiveSetProjectionTraversalGen<V> {
    pub(super) transitive_set: V,
    pub projection: usize,
    pub ordering: TransitiveSetOrdering,
}

starlark_complex_value!(pub TransitiveSetProjectionTraversal);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for TransitiveSetProjectionTraversalGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    starlark_type!("transitive_set_args_projection_iterator");

    fn iterate_collect(&self, _heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        let set =
            TransitiveSet::from_value(self.transitive_set.to_value()).context("Invalid inner")?;
        Ok(set
            .iter_projection_values(self.ordering, self.projection)?
            .collect())
    }
}
