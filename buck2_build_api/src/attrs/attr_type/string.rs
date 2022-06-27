/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::anyhow;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::string::StringAttrType;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;

use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::CoercedAttr;

impl AttrTypeCoerce for StringAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        _ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        match value.unpack_str() {
            Some(s) => Ok(AttrLiteral::String(s.to_owned())),
            None => Err(anyhow!(CoercionError::type_error(STRING_TYPE, value))),
        }
    }

    fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}
