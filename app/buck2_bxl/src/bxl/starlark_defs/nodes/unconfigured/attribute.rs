/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use anyhow::Context;
use buck2_core::package::PackageLabel;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::display::AttrDisplayWithContext;
use buck2_node::attrs::fmt_context::AttrFmtContext;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::attrs::serialize::AttrSerializeWithContext;
use derive_more::From;
use dupe::Dupe;
use serde::Serialize;
use starlark::__derive_refs::serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
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
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;

#[derive(
    Debug,
    Clone,
    Coerce,
    Trace,
    Freeze,
    ProvidesStaticType,
    NoSerialize,
    Allocative
)]
#[repr(C)]
pub struct StarlarkTargetNodeCoercedAttributesGen<V> {
    pub(super) inner: V,
}

impl<V: Display> Display for StarlarkTargetNodeCoercedAttributesGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Traversal({})", self.inner)
    }
}

starlark_complex_value!(pub StarlarkTargetNodeCoercedAttributes);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StarlarkTargetNodeCoercedAttributesGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("starlark_attributes");

    fn iterate_collect(&self, heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        let starlark_target_node = self
            .inner
            .downcast_ref::<StarlarkTargetNode>()
            .context("invalid inner")?;
        let target_node = &starlark_target_node.0;
        Ok(target_node
            .attrs(AttrInspectOptions::All)
            .map(|a| {
                heap.alloc((
                    a.name,
                    StarlarkCoercedAttr(a.value.clone(), target_node.label().pkg()),
                ))
            })
            .collect())
    }
}

#[derive(Debug, ProvidesStaticType, From, Allocative, StarlarkDocs)]
#[starlark_docs(directory = "bxl")]
pub struct StarlarkCoercedAttr(pub CoercedAttr, pub PackageLabel);

starlark_simple_value!(StarlarkCoercedAttr);

impl Display for StarlarkCoercedAttr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(
            &AttrFmtContext {
                package: Some(self.1.dupe()),
            },
            f,
        )
    }
}

impl Serialize for StarlarkCoercedAttr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.serialize_with_ctx(
            &AttrFmtContext {
                package: Some(self.1.dupe()),
            },
            serializer,
        )
    }
}

/// Coerced attr from an unconfigured target node.
impl<'v> StarlarkValue<'v> for StarlarkCoercedAttr {
    starlark_type!("coerced_attr");
}
