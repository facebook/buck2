/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Ordering;

use buck2_node::attrs::attr_type::dict::DictAttrType;
use buck2_node::attrs::attr_type::dict::DictLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::dict::DictRef;

use crate::attrs::coerce::AttrTypeCoerce;
use crate::attrs::coerce::attr_type::AttrTypeExt;
use crate::attrs::coerce::attr_type::ty_maybe_select::TyMaybeSelect;

impl AttrTypeCoerce for DictAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> buck2_error::Result<CoercedAttr> {
        let dict = DictRef::unpack_value_err(value)?;
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
        Ok(CoercedAttr::Dict(DictLiteral(ctx.intern_dict(res))))
    }

    fn starlark_type(&self) -> TyMaybeSelect {
        TyMaybeSelect::Dict(
            Box::new(self.key.starlark_type()),
            Box::new(self.value.starlark_type()),
        )
    }
}
