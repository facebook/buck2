/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::source::SourceAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::typing::Ty;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::ty_maybe_select::TyMaybeSelect;
use crate::attrs::coerce::error::CoercionError;
use crate::attrs::coerce::AttrTypeCoerce;

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum SourceLabelCoercionError {
    #[error(
        "Couldn't coerce `{0}` as a source.\n  Error when treated as a target: {1:#}\n  Error when treated as a path: {2:#}"
    )]
    CoercionFailed(String, buck2_error::Error, buck2_error::Error),
}

/// Try cleaning up irrelevant details users often type
fn cleanup_path(value: &str) -> &str {
    let value = value.strip_prefix("./").unwrap_or(value);
    let value = value.strip_suffix("/").unwrap_or(value);
    if value == "." { "" } else { value }
}

impl AttrTypeCoerce for SourceAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> buck2_error::Result<CoercedAttr> {
        let source_label = value
            .unpack_str()
            .ok_or_else(|| CoercionError::type_error(STRING_TYPE, value))?;

        let label_err = if source_label.contains(':') {
            match ctx.coerce_providers_label(source_label) {
                Ok(l) => return Ok(CoercedAttr::SourceLabel(l)),
                Err(e) => Some(e),
            }
        } else {
            // As an optimization, we skip trying to parse as a label in this case
            None
        };

        let path_err = match ctx.coerce_path(cleanup_path(source_label), self.allow_directory) {
            Ok(path) => return Ok(CoercedAttr::SourceFile(path)),
            Err(path_err) => path_err,
        };

        if let Some(label_err) = label_err {
            Err(
                SourceLabelCoercionError::CoercionFailed(value.to_str(), label_err, path_err)
                    .into(),
            )
        } else {
            Err(path_err
                .context(format!("Coercing `{}` as a source", value.to_str()))
                .into())
        }
    }

    fn starlark_type(&self) -> TyMaybeSelect {
        TyMaybeSelect::Basic(Ty::string())
    }
}
