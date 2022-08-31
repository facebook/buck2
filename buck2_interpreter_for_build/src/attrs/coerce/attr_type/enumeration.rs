/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::enumeration::EnumAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;

use crate::attrs::coerce::error::CoercionError;
use crate::attrs::coerce::AttrTypeCoerce;

impl AttrTypeCoerce for EnumAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        _ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        match value.unpack_str() {
            Some(s) => {
                // Enum names in Buck can be specified upper or lower case,
                // so we normalise them to lowercase to make rule implementations easier
                let s = s.to_lowercase();
                if self.variants.contains(&s) {
                    Ok(AttrLiteral::EnumVariant(s))
                } else {
                    let wanted = self.variants.iter().cloned().collect();
                    Err(CoercionError::InvalidEnumVariant(s, wanted).into())
                }
            }
            None => Err(CoercionError::type_error(STRING_TYPE, value).into()),
        }
    }

    fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}
