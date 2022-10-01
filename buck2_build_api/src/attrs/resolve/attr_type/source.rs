/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::buck_path::BuckPath;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_execute::artifact::source_artifact::SourceArtifact;
use buck2_node::attrs::attr_type::source::SourceAttrType;
use starlark::values::list::FrozenList;
use starlark::values::Value;

use crate::attrs::resolve::ctx::AttrResolutionContext;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;

#[derive(thiserror::Error, Debug)]
enum SourceLabelResolutionError {
    #[error("Expected a single artifact from {0}, but it returned {1} artifacts")]
    ExpectedSingleValue(String, usize),
}

pub(crate) trait SourceAttrTypeExt {
    fn resolve_single_file<'v>(ctx: &dyn AttrResolutionContext<'v>, path: &BuckPath) -> Value<'v> {
        ctx.heap().alloc(StarlarkArtifact::new(
            SourceArtifact::new(path.clone()).into(),
        ))
    }

    fn resolve_label<'v>(
        ctx: &dyn AttrResolutionContext<'v>,
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
        ctx: &dyn AttrResolutionContext<'v>,
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
