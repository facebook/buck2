/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context as _;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::coerce::Coerce;
use gazebo::dupe::Dupe;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::interpreter::rule_defs::transitive_set::TransitiveSet;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetError;

#[derive(Debug, Clone, Dupe, Copy, Trace, Freeze, PartialEq)]
pub enum TransitiveSetOrdering {
    /// Preorder traversal, a good default behavior which traverses depth-first returning the current node, and then its children left-to-right.
    Preorder,
}

impl TransitiveSetOrdering {
    pub fn parse(s: &str) -> anyhow::Result<TransitiveSetOrdering> {
        // NOTE: If this list is updated, update the OrderingUnexpectedValue error text.
        match s {
            "preorder" => Ok(Self::Preorder),
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
    NoSerialize
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
    Self: ProvidesStaticType,
{
    starlark_type!("transitive_set_iterator");

    fn iterate<'a>(
        &'a self,
        _heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        let tset = TransitiveSet::from_value(self.inner.to_value()).context("Invalid inner")?;
        tset.iter_values(self.ordering)
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
    NoSerialize
)]
#[display(fmt = "Traversal({}[\"{}\"])", transitive_set, projection)]
#[repr(C)]
pub struct TransitiveSetProjectionTraversalGen<V> {
    pub(super) transitive_set: V,
    pub projection: usize,
}

starlark_complex_value!(pub TransitiveSetProjectionTraversal);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for TransitiveSetProjectionTraversalGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("transitive_set_args_projection_iterator");

    fn iterate<'a>(
        &'a self,
        _heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        let set =
            TransitiveSet::from_value(self.transitive_set.to_value()).context("Invalid inner")?;
        set.iter_projection_values(TransitiveSetOrdering::Preorder, self.projection)
    }
}
