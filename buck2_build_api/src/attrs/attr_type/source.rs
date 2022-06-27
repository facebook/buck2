/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::anyhow;
use buck2_core::buck_path::BuckPath;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::source::SourceAttrType;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use gazebo::prelude::*;
use starlark::values::list::FrozenList;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;
use thiserror::Error;

use crate::actions::artifact::SourceArtifact;
use crate::attrs::analysis::AttrResolutionContext;
use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::CoercedAttr;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;

#[derive(Error, Debug)]
enum SourceLabelResolutionError {
    #[error("Expected a single artifact from {0}, but it returned {1} artifacts")]
    ExpectedSingleValue(String, usize),
    #[error(
        "Couldn't coerce `{0}` as a source.\n  Error when treated as a target: {1:#}\n  Error when treated as a path: {2:#}"
    )]
    CoercionFailed(String, anyhow::Error, anyhow::Error),
}

/// Try cleaning up irrelvant details users often type
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
                    Err(path_err) => Err(SourceLabelResolutionError::CoercionFailed(
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

pub(crate) trait SourceAttrTypeExt {
    fn resolve_single_file<'v>(ctx: &'v dyn AttrResolutionContext, path: &BuckPath) -> Value<'v> {
        ctx.heap().alloc(StarlarkArtifact::new(
            SourceArtifact::new(path.clone()).into(),
        ))
    }

    fn resolve_label<'v>(
        ctx: &'v dyn AttrResolutionContext,
        label: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<Vec<Value<'v>>> {
        let dep = ctx.get_dep(label)?;
        let default_outputs = dep
            .provider_collection()
            .default_info()
            .default_outputs_raw();
        let res = FrozenList::from_frozen_value(&default_outputs)
            .unwrap()
            .iter()
            .collect();
        Ok(res)
    }

    fn resolve_single_label<'v>(
        ctx: &'v dyn AttrResolutionContext,
        value: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<Value<'v>> {
        let mut resolved = Self::resolve_label(ctx, value)?;
        if resolved.len() == 1 {
            Ok(resolved.pop().unwrap())
        } else {
            Err(
                SourceLabelResolutionError::ExpectedSingleValue(value.to_string(), resolved.len())
                    .into(),
            )
        }
    }
}

impl SourceAttrTypeExt for SourceAttrType {}
