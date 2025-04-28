/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter;

use buck2_node::attrs::attr_type::tuple::TupleAttrType;
use buck2_node::attrs::attr_type::tuple::TupleLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use dupe::IterDupedExt;
use gazebo::prelude::SliceExt;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::list::ListRef;
use starlark::values::tuple::TupleRef;

use crate::attrs::coerce::AttrTypeCoerce;
use crate::attrs::coerce::attr_type::AttrTypeExt;
use crate::attrs::coerce::attr_type::ty_maybe_select::TyMaybeSelect;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum TupleError {
    #[error("Expected tuple of at most {0} elements")]
    TooManyElements(usize),
}

impl AttrTypeCoerce for TupleAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> buck2_error::Result<CoercedAttr> {
        let coerce = |items: &[Value]| {
            // Use comparison rather than equality below. If the tuple is too short,
            // it is implicitly extended using None.
            if items.len() <= self.xs.len() {
                let mut res = Vec::with_capacity(self.xs.len());
                for (c, v) in self
                    .xs
                    .iter()
                    .zip(items.iter().duped().chain(iter::repeat(Value::new_none())))
                {
                    res.push(c.coerce(configurable, ctx, v)?);
                }
                Ok(CoercedAttr::Tuple(TupleLiteral(ctx.intern_list(res))))
            } else {
                Err(TupleError::TooManyElements(self.xs.len()).into())
            }
        };
        if let Some(list) = ListRef::from_value(value) {
            coerce(list.content())
        } else {
            coerce(<&TupleRef>::unpack_value_err(value)?.content())
        }
    }

    fn starlark_type(&self) -> TyMaybeSelect {
        TyMaybeSelect::Tuple(self.xs.map(|x| x.starlark_type()))
    }
}
