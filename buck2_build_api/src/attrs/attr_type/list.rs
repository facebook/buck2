/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::anyhow;
use buck2_node::attrs::attr_type::list::ListAttrType;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use gazebo::prelude::*;
use starlark::values::list::List;
use starlark::values::tuple::Tuple;
use starlark::values::Value;

use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::attr_type::AttrTypeExt;
use crate::attrs::AttrLiteral;
use crate::attrs::CoercedAttr;

impl AttrTypeCoerce for ListAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        if let Some(list) = List::from_value(value) {
            Ok(AttrLiteral::List(
                list.content()
                    .try_map(|v| (self.inner).coerce(configurable, ctx, *v))?
                    .into_boxed_slice(),
                self.inner.dupe(),
            ))
        } else if let Some(list) = Tuple::from_value(value) {
            Ok(AttrLiteral::List(
                list.content()
                    .try_map(|v| (self.inner).coerce(configurable, ctx, *v))?
                    .into_boxed_slice(),
                self.inner.dupe(),
            ))
        } else {
            Err(anyhow!(CoercionError::type_error(List::TYPE, value,)))
        }
    }

    fn starlark_type(&self) -> String {
        format!("[{}]", self.inner.starlark_type())
    }
}
