/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::path::Path;

use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_execute::artifact::source_artifact::SourceArtifact;
use derive_more::Display;
use either::Either;
use gazebo::dupe::Dupe;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::file_set::StarlarkFileNode;

/// FileExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be files. It will accept either a
/// literal (like `//path/to/some/file.txt`) or a `SourceArtifact` via `StarlarkArtifact`
/// or a `StarlarkFileNode`
#[derive(Debug, Display, Clone)]
pub enum FileExpr<'a> {
    Literal(&'a str),
    SourceArtifact(SourceArtifact),
    StarlarkFileNode(StarlarkFileNode),
}

impl<'a> FileExpr<'a> {
    pub fn get(self, dice: &BxlSafeDiceComputations<'_>) -> anyhow::Result<CellPath> {
        match self {
            FileExpr::Literal(val) => {
                let fs = dice.0.global_data().get_io_provider().project_root().dupe();
                let rel = if Path::new(val).is_absolute() {
                    fs.relativize(AbsPath::new(val)?)?
                } else {
                    Cow::Borrowed(<&ProjectRelativePath>::try_from(val)?)
                };
                dice.via_dice(async move |ctx| ctx.get_cell_resolver().await?.get_cell_path(&rel))
            }
            FileExpr::SourceArtifact(val) => Ok(val.get_path().to_cell_path()),
            FileExpr::StarlarkFileNode(val) => Ok(val.0),
        }
    }
}

impl<'v> UnpackValue<'v> for FileExpr<'v> {
    fn expected() -> String {
        "literal, or source artifact, or file node".to_owned()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if let Some(s) = value.unpack_str() {
            Some(FileExpr::Literal(s))
        } else if let Some(v) = value.downcast_ref::<StarlarkFileNode>() {
            Some(FileExpr::StarlarkFileNode(v.to_owned()))
        } else {
            value
                .as_artifact()?
                .get_bound_artifact()
                .map(|a| a.get_source())
                .ok()?
                .map(FileExpr::SourceArtifact)
        }
    }
}

impl<'v> StarlarkTypeRepr for FileExpr<'v> {
    fn starlark_type_repr() -> String {
        Either::<String, StarlarkArtifact>::starlark_type_repr()
    }
}
