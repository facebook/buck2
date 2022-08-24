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
use buck2_node::attrs::attr_type::source::SourceAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use gazebo::prelude::*;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;

use crate::attrs::coerce::error::CoercionError;
use crate::attrs::coerce::AttrTypeCoerce;

#[derive(Debug, thiserror::Error)]
enum SourceLabelCoercionError {
    #[error(
        "Couldn't coerce `{0}` as a source.\n  Error when treated as a target: {1:#}\n  Error when treated as a path: {2:#}"
    )]
    CoercionFailed(String, anyhow::Error, anyhow::Error),
}

/// Try cleaning up irrelevant details users often type
fn cleanup_path(value: &str) -> &str {
    let value = value.trim_start_match("./");
    let value = value.trim_end_match("/");
    if value == "." { "" } else { value }
}

impl AttrTypeCoerce for SourceAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let source_label = value
            .unpack_str()
            .ok_or_else(|| anyhow!(CoercionError::type_error(STRING_TYPE, value)))?;
        match ctx.coerce_label(source_label) {
            Ok(label) => Ok(AttrLiteral::SourceLabel(box label)),
            Err(label_err) => {
                match ctx.coerce_path(cleanup_path(source_label), self.allow_directory) {
                    Ok(path) => Ok(AttrLiteral::SourceFile(box path)),
                    Err(path_err) => Err(SourceLabelCoercionError::CoercionFailed(
                        value.to_str(),
                        label_err,
                        path_err,
                    )
                    .into()),
                }
            }
        }
    }

    fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}
