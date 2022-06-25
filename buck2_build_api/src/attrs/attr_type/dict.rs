/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Ordering;

use anyhow::anyhow;
use buck2_node::attrs::attr_type::dict::DictAttrType;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::values::dict::Dict;
use starlark::values::Value;

use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::attr_type::AttrTypeExt;
use crate::attrs::AttrCoercionContext;
use crate::attrs::AttrLiteral;
use crate::attrs::CoercedAttr;

impl AttrTypeCoerce for DictAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        if let Some(dict) = Dict::from_value(value) {
            let mut res = Vec::with_capacity(dict.len());
            if self.sorted {
                // First sort the values
                let mut items = dict.iter().collect::<Vec<_>>();
                // If two things are incompatible, just return Eq. The resulting order is undefined, but safely undefined.
                items.sort_by(|a, b| a.0.compare(b.0).unwrap_or(Ordering::Equal));

                for (k, v) in items {
                    res.push((
                        self.key.coerce(configurable, ctx, k)?,
                        self.value.coerce(configurable, ctx, v)?,
                    ));
                }
            } else {
                for (k, v) in dict.iter() {
                    res.push((
                        self.key.coerce(configurable, ctx, k)?,
                        self.value.coerce(configurable, ctx, v)?,
                    ));
                }
            }
            Ok(AttrLiteral::Dict(res))
        } else {
            Err(anyhow!(CoercionError::type_error(Dict::TYPE, value,)))
        }
    }

    fn starlark_type(&self) -> String {
        format!(
            "{{{}: {}}}",
            self.key.starlark_type(),
            self.value.starlark_type()
        )
    }
}
