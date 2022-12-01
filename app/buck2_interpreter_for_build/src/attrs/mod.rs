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
use starlark::values::docs::DocString;
use starlark::values::docs::DocStringKind;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::AttrTypeExt;

pub mod attribute_as_starlark_value;
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
        value: Option<Value<'v>>,
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
        value: Option<Value<'v>>,
    ) -> anyhow::Result<CoercedValue> {
        match (&self.default, value) {
            (default, Some(value)) if !value.is_none() => self
                .coercer
                .coerce_with_default(
                    configurable,
                    coercer_ctx,
                    value,
                    default.as_ref().map(|x| &**x),
                )
                .map(CoercedValue::Custom)
                .with_context(|| format!("when coercing attribute `{}`", param_name)),
            (Some(_), _) => Ok(CoercedValue::Default),
            (None, _) => {
                Err(AttrCoerceError::MissingMandatoryParameter(param_name.to_owned()).into())
            }
        }
    }

    fn docstring(&self) -> Option<DocString> {
        DocString::from_docstring(DocStringKind::Starlark, &self.doc)
    }

    fn starlark_type(&self) -> String {
        self.coercer.starlark_type()
    }
}
