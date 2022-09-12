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
use std::iter;

use anyhow::Context as _;
use gazebo::any::ProvidesStaticType;
use gazebo::coerce::Coerce;
use gazebo::display::display_chain;
use gazebo::display::display_container;
use gazebo::display::display_pair;
use gazebo::prelude::*;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::artifact_groups::TransitiveSetProjectionKey;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetOrdering;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetProjectionTraversal;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;

/// TransitiveSetJsonProjection is the starlark value returned from the starlark method `transitive_set.project_as_json()`
///
/// The projected values are all stored on the TransitiveSet itself and this value will reference back to that. The main
/// point of this object is to provide a distinct value that the write_json implementation understands so that the value
/// can be passed to that.
#[derive(Debug, Clone, Coerce, Trace, Freeze, ProvidesStaticType)]
#[derive(NoSerialize)] // TODO we should probably have a serialization for transitive set
#[repr(C)]
pub struct TransitiveSetJsonProjectionGen<V> {
    pub(super) transitive_set: V,

    /// The index of the projection. Once transitive sets are defined, their projections never
    /// change, so we can afford to just store the index here.
    pub projection: usize,

    /// The ordering to use when traversing the projection.
    pub ordering: TransitiveSetOrdering,
}

impl<'v, V: ValueLike<'v>> Display for TransitiveSetJsonProjectionGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let projection_name = self.projection_name().unwrap_or("<invalid projection>");
        display_container(
            f,
            "TransitiveSetProjection(",
            ")",
            display_chain(
                iter::once(projection_name),
                iter::once(display_pair("transitive_set", "=", &self.transitive_set)),
            ),
        )
    }
}

impl<'v, V: ValueLike<'v>> TransitiveSetJsonProjectionGen<V> {
    fn projection_name(&self) -> anyhow::Result<&'v str> {
        TransitiveSet::from_value(self.transitive_set.to_value())
            .context("Invalid transitive_set")?
            .projection_name(self.projection)
    }

    pub(crate) fn to_projection_key(&self) -> anyhow::Result<TransitiveSetProjectionKey> {
        let set = TransitiveSet::from_value(self.transitive_set.to_value())
            .context("Invalid transitive_set")?;

        Ok(TransitiveSetProjectionKey {
            key: set.key().dupe(),
            projection: self.projection,
        })
    }
}

impl<'v, V: ValueLike<'v>> TransitiveSetJsonProjectionGen<V> {
    pub fn iter_values<'a>(&'a self) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        let set = TransitiveSet::from_value(self.transitive_set.to_value())
            .context("Invalid transitive_set")?;
        set.iter_projection_values(self.ordering, self.projection)
    }
}

starlark_complex_value!(pub TransitiveSetJsonProjection);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for TransitiveSetJsonProjectionGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("transitive_set_json_projection");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(transitive_set_json_projection_methods)
    }
}

#[starlark_module]
fn transitive_set_json_projection_methods(builder: &mut MethodsBuilder) {
    fn traverse<'v>(this: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let projection = TransitiveSetJsonProjection::from_value(this).context("Invalid this")?;
        Ok(heap.alloc(TransitiveSetProjectionTraversal {
            transitive_set: projection.transitive_set,
            projection: projection.projection,
            ordering: projection.ordering,
        }))
    }
}
