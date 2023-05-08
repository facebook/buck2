/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_node::attrs::attr::Attribute;
use buck2_node::attrs::attr::CoercedValue;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::docs::DocString;
use starlark::docs::DocStringKind;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::AttrTypeExt;
use crate::attrs::coerce::error::CoercionError;

pub mod attribute_as_starlark_value;
pub mod attrs_global;
pub mod coerce;

#[derive(Debug, thiserror::Error)]
enum AttrCoerceError {
    #[error("Parameter `{0}` had no value provided, but it is mandatory")]
    MissingMandatoryParameter(String),
}

pub trait AttributeCoerceExt {
    fn coerce<'v>(
        &self,
        param_name: &str,
        configurable: AttrIsConfigurable,
        coercer_ctx: &dyn AttrCoercionContext,
        value: Value<'v>,
    ) -> anyhow::Result<CoercedValue>;

    fn docstring(&self) -> Option<DocString>;

    fn starlark_type(&self) -> String;
}

impl AttributeCoerceExt for Attribute {
    /// Attempt to coerce a value. If the value provided is `None`, and a default value is available,
    /// that default value is returned.
    fn coerce<'v>(
        &self,
        param_name: &str,
        configurable: AttrIsConfigurable,
        coercer_ctx: &dyn AttrCoercionContext,
        value: Value<'v>,
    ) -> anyhow::Result<CoercedValue> {
        if self.is_default_only() {
            if value.is_none() {
                return Ok(CoercedValue::Default);
            } else {
                return Err(CoercionError::DefaultOnly(value.to_string()).into());
            }
        }

        match self.default() {
            default if !value.is_none() => self
                .coercer()
                .coerce_with_default(configurable, coercer_ctx, value, default.map(|x| &**x))
                .map(CoercedValue::Custom)
                .with_context(|| {
                    format!(
                        "Error coercing attribute `{}` of type `{}`",
                        param_name, self
                    )
                }),
            Some(_) => Ok(CoercedValue::Default),
            None => Err(AttrCoerceError::MissingMandatoryParameter(param_name.to_owned()).into()),
        }
    }

    fn docstring(&self) -> Option<DocString> {
        DocString::from_docstring(DocStringKind::Starlark, self.doc())
    }

    fn starlark_type(&self) -> String {
        self.coercer().starlark_type()
    }
}
