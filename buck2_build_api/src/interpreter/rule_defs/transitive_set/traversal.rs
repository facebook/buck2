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
use gazebo::{any::ProvidesStaticType, coerce::Coerce};
use starlark::values::{Freeze, Heap, NoSerialize, StarlarkValue, Trace, Value, ValueLike};

use crate::interpreter::rule_defs::transitive_set::{TransitiveSet, TransitiveSetArgsProjection};

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
        tset.iter_values()
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
pub struct TransitiveSetArgsProjectionTraversalGen<V> {
    pub(super) inner: V,
}

starlark_complex_value!(pub TransitiveSetArgsProjectionTraversal);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for TransitiveSetArgsProjectionTraversalGen<V>
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
        let projection = TransitiveSetArgsProjection::from_value(self.inner.to_value())
            .context("Invalid inner")?;
        projection.iter_values()
    }
}
