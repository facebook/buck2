/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::target_aliases::TargetAliasResolver;
use buck2_core::{
    cells::{paths::CellRelativePath, CellInstance},
    package::Package,
    target::TargetLabel,
};
use buck2_interpreter::{
    pattern::{ParsedPattern, TargetPattern},
    types::target_label::StarlarkTargetLabel,
};
use gazebo::dupe::Dupe;
use starlark::values::{Value, ValueLike};

#[derive(Debug, thiserror::Error)]
enum ValueAsTargetLabelError {
    #[error("Expected a single target like ite, but was `{0}`")]
    NotATarget(String),
}

pub trait ValueAsStarlarkTargetLabel {
    fn parse_target_platforms(
        self,
        target_alias_resolver: &TargetAliasResolver,
        cell: &CellInstance,
    ) -> anyhow::Result<Option<TargetLabel>>;
}

impl<'v> ValueAsStarlarkTargetLabel for Value<'v> {
    fn parse_target_platforms(
        self,
        target_alias_resolver: &TargetAliasResolver,
        cell: &CellInstance,
    ) -> anyhow::Result<Option<TargetLabel>> {
        let target_platform = if self.is_none() {
            None
        } else if let Some(s) = self.unpack_str() {
            Some(
                ParsedPattern::<TargetPattern>::parse_relaxed(
                    target_alias_resolver,
                    cell.cell_alias_resolver(),
                    &Package::new(cell.name(), CellRelativePath::unchecked_new("")),
                    s,
                )?
                .as_target_label(s)?,
            )
        } else if let Some(target) = self.downcast_ref::<StarlarkTargetLabel>() {
            Some(target.label().dupe())
        } else {
            return Err(anyhow::anyhow!(ValueAsTargetLabelError::NotATarget(
                self.to_repr()
            )));
        };

        Ok(target_platform)
    }
}
