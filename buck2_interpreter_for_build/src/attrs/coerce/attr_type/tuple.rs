/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter;

use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::tuple::TupleAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use gazebo::prelude::*;
use itertools::Itertools;
use starlark::values::list::List;
use starlark::values::tuple::Tuple;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::AttrTypeExt;
use crate::attrs::coerce::error::CoercionError;
use crate::attrs::coerce::AttrTypeCoerce;

impl AttrTypeCoerce for TupleAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let coerce = |value, items: &[Value]| {
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
                Ok(AttrLiteral::Tuple(res.into_boxed_slice()))
            } else {
                Err(anyhow::anyhow!(CoercionError::type_error(
                    &format!("Tuple of at most length {}", self.xs.len()),
                    value
                )))
            }
        };
        if let Some(list) = Tuple::from_value(value) {
            coerce(value, list.content())
        } else if let Some(list) = List::from_value(value) {
            coerce(value, list.content())
        } else {
            Err(anyhow::anyhow!(CoercionError::type_error(
                Tuple::TYPE,
                value,
            )))
        }
    }

    fn starlark_type(&self) -> String {
        format!("({})", self.xs.iter().map(|x| x.starlark_type()).join(", "))
    }
}
