/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::fmt::Display;
use std::iter;

use allocative::Allocative;
use buck2_error::BuckErrorOptionContext;
use display_container::display_pair;
use display_container::fmt_container;
use display_container::iter_display_chain;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::values::FreezeBranded;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkPagable;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUnchecked;
use starlark::values::starlark_value;

use crate::artifact_groups::TransitiveSetProjectionKey;
use crate::artifact_groups::TransitiveSetProjectionWrapper;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetOrdering;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetProjectionTraversal;

/// TransitiveSetJsonProjection is the starlark value returned from the starlark method `transitive_set.project_as_json()`
///
/// The projected values are all stored on the TransitiveSet itself and this value will reference back to that. The main
/// point of this object is to provide a distinct value that the write_json implementation understands so that the value
/// can be passed to that.
#[derive(
    Debug,
    Clone,
    Trace,
    FreezeBranded,
    ProvidesStaticType,
    Allocative,
    StarlarkPagable
)]
#[derive(NoSerialize)] // TODO we should probably have a serialization for transitive set
#[repr(C)]
pub struct TransitiveSetJsonProjection<'v> {
    pub(super) transitive_set: ValueOfUnchecked<'v, FrozenTransitiveSet>,

    /// The index of the projection. Once transitive sets are defined, their projections never
    /// change, so we can afford to just store the index here.
    pub projection: usize,

    /// The ordering to use when traversing the projection.
    #[freeze_branded(identity)]
    pub ordering: TransitiveSetOrdering,
}

impl<'v> Display for TransitiveSetJsonProjection<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let projection_name = self.projection_name().unwrap_or("<invalid projection>");
        fmt_container(
            f,
            "TransitiveSetProjection(",
            ")",
            iter_display_chain(
                iter::once(projection_name),
                iter::once(display_pair("TransitiveSet", "=", &self.transitive_set)),
            ),
        )
    }
}

impl<'v> TransitiveSetJsonProjection<'v> {
    fn projection_name(&self) -> buck2_error::Result<&'v str> {
        TransitiveSet::from_value(self.transitive_set.get().to_value())
            .internal_error("Invalid transitive_set")?
            .projection_name(self.projection)
    }

    pub(crate) fn to_projection_key_wrapper(
        &self,
    ) -> buck2_error::Result<TransitiveSetProjectionWrapper> {
        let set = TransitiveSet::from_value(self.transitive_set.get().to_value())
            .internal_error("Invalid transitive_set")?;

        Ok(TransitiveSetProjectionWrapper::new(
            TransitiveSetProjectionKey {
                key: set.key().dupe(),
                projection: self.projection,
            },
            set.projection_path_resolution_may_require_artifact_value
                .get(self.projection)?,
            set.projection_is_eligible_for_dedupe.get(self.projection)?,
        ))
    }
}

impl<'v> TransitiveSetJsonProjection<'v> {
    pub fn iter_values<'a>(
        &'a self,
    ) -> buck2_error::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        let set = TransitiveSet::from_value(self.transitive_set.get().to_value())
            .internal_error("Invalid transitive_set")?;
        set.iter_projection_values(self.ordering, self.projection)
    }
}

starlark_complex_value_branded!(pub TransitiveSetJsonProjection);

starlark::methods_static!(
    TRANSITIVE_SET_JSON_PROJECTION_METHODS = transitive_set_json_projection_methods
);

#[starlark_value(type = "TransitiveSetJsonProjection")]
impl<'v> StarlarkValue<'v> for TransitiveSetJsonProjection<'v> {
    fn get_methods() -> Option<&'static Methods> {
        Some(TRANSITIVE_SET_JSON_PROJECTION_METHODS.methods())
    }
}

#[starlark_module]
fn transitive_set_json_projection_methods(builder: &mut MethodsBuilder) {
    fn traverse<'v>(
        this: ValueOf<'v, &'v TransitiveSetJsonProjection<'v>>,
        heap: Heap<'v>,
    ) -> starlark::Result<Value<'v>> {
        Ok(heap.alloc(TransitiveSetProjectionTraversal {
            transitive_set: this.typed.transitive_set,
            projection: this.typed.projection,
            ordering: this.typed.ordering,
        }))
    }

    #[starlark(attribute)]
    fn projection_name<'v>(
        this: ValueOf<'v, &'v TransitiveSetJsonProjection<'v>>,
        heap: Heap<'v>,
    ) -> starlark::Result<StringValue<'v>> {
        Ok(heap.alloc_str(this.typed.projection_name()?))
    }

    #[starlark(attribute)]
    fn transitive_set<'v>(
        this: ValueOf<'v, &'v TransitiveSetJsonProjection<'v>>,
    ) -> starlark::Result<ValueOfUnchecked<'v, FrozenTransitiveSet>> {
        Ok(this.typed.transitive_set)
    }
}
