/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::list::ListAttrType;
use buck2_node::attrs::attr_type::list::ListLiteral;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use gazebo::prelude::*;
use starlark::values::list::ListRef;
use starlark::values::tuple::TupleRef;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::AttrTypeExt;
use crate::attrs::coerce::error::CoercionError;
use crate::attrs::coerce::AttrTypeCoerce;

impl AttrTypeCoerce for ListAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral> {
        if let Some(list) = ListRef::from_value(value) {
            Ok(AttrLiteral::List(ListLiteral(
                ctx.intern_list(
                    list.content()
                        .try_map(|v| (self.inner).coerce(configurable, ctx, *v))?,
                ),
            )))
        } else if let Some(list) = TupleRef::from_value(value) {
            Ok(AttrLiteral::List(ListLiteral(
                ctx.intern_list(
                    list.content()
                        .try_map(|v| (self.inner).coerce(configurable, ctx, *v))?,
                ),
            )))
        } else {
            Err(anyhow::anyhow!(CoercionError::type_error(
                ListRef::TYPE,
                value,
            )))
        }
    }

    fn starlark_type(&self) -> String {
        format!("[{}]", self.inner.starlark_type())
    }
}
