/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use derive_more::Display;
use derive_more::From;
use gazebo::any::ProvidesStaticType;
use gazebo::coerce::Coerce;
use starlark::starlark_complex_value;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;

#[derive(
    Debug,
    Clone,
    Coerce,
    Trace,
    Freeze,
    Display,
    ProvidesStaticType,
    NoSerialize
)]
#[display(fmt = "Traversal({})", self.0)]
#[repr(C)]
pub struct StarlarkTargetNodeCoercedAttributesGen<V> {
    pub(super) inner: V,
}

starlark_complex_value!(pub StarlarkTargetNodeCoercedAttributes);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StarlarkTargetNodeCoercedAttributesGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("starlark_attributes");

    fn iterate<'a>(
        &'a self,
        heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        let starlark_target_node = self
            .inner
            .downcast_ref::<StarlarkTargetNode>()
            .context("invalid inner")?;
        let target_node = &starlark_target_node.0;
        Ok(box target_node
            .attrs(AttrInspectOptions::All)
            .map(|(name, value)| heap.alloc((name, StarlarkCoercedAttr::from(value.clone())))))
    }
}

#[derive(Debug, Display, ProvidesStaticType, From)]
#[derive(NoSerialize)] // TODO probably should be serializable the same as how queries serialize
#[display(fmt = "{:?}", self)]
pub struct StarlarkCoercedAttr(pub CoercedAttr);

starlark_simple_value!(StarlarkCoercedAttr);

impl<'v> StarlarkValue<'v> for StarlarkCoercedAttr {
    starlark_type!("coerced_attr");
}
