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

use buck2_build_api::actions::artifact::source_artifact::SourceArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellInstance;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::pattern::maybe_split_cell_alias_and_relative_path;
use derive_more::Display;
use dupe::Dupe;
use either::Either;
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

fn parse_cell_path_as_file_expr_literal(
    val: &str,
    cell_alias_resolver: &CellAliasResolver,
) -> anyhow::Result<Option<CellPath>> {
    Ok(match maybe_split_cell_alias_and_relative_path(val)? {
        Some((alias, path)) => {
            let cell_name = cell_alias_resolver.resolve(alias.as_str())?;

            let cell_relative_path = CellRelativePath::new(path);

            Some(CellPath::new(cell_name, cell_relative_path.to_buf()))
        }
        None => None,
    })
}

impl<'a> FileExpr<'a> {
    pub fn get(
        self,
        dice: &BxlSafeDiceComputations<'_>,
        cell_instance: &CellInstance,
    ) -> anyhow::Result<CellPath> {
        match self {
            FileExpr::Literal(val) => {
                match parse_cell_path_as_file_expr_literal(
                    val,
                    cell_instance.cell_alias_resolver(),
                )? {
                    Some(cell_path) => Ok(cell_path),
                    None => {
                        let fs = dice.0.global_data().get_io_provider().project_root().dupe();
                        let path = Path::new(val);
                        let rel = if path.is_absolute() {
                            Cow::Owned(fs.relativize_any(AbsPath::new(path)?)?)
                        } else {
                            Cow::Borrowed(<&ProjectRelativePath>::try_from(val)?)
                        };
                        dice.via_dice(async move |ctx| {
                            ctx.get_cell_resolver().await?.get_cell_path(&rel)
                        })
                    }
                }
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

#[cfg(test)]
mod tests {

    use buck2_core::cells::alias::NonEmptyCellAlias;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::CellAliasResolver;
    use maplit::hashmap;

    use super::*;

    #[test]
    fn test_parse_cell_path_as_file_expr_literal() -> anyhow::Result<()> {
        let cell1 = CellName::testing_new("cell1");

        let map = hashmap![
            NonEmptyCellAlias::new("cell1".to_owned()).unwrap() => CellName::testing_new("cell1"),
            NonEmptyCellAlias::new("cell2".to_owned()).unwrap() => CellName::testing_new("cell2"),
            NonEmptyCellAlias::new("cell3".to_owned()).unwrap() => CellName::testing_new("cell3"),
        ];

        let cell_alias_resolver = CellAliasResolver::new(cell1, map)?;

        let actual_cell_path =
            parse_cell_path_as_file_expr_literal("//foo/bar", &cell_alias_resolver)?.unwrap();
        let expected_cell_path = CellPath::testing_new("cell1", "foo/bar");
        assert_eq!(actual_cell_path, expected_cell_path);

        let actual_cell_path =
            parse_cell_path_as_file_expr_literal("@//foo/bar", &cell_alias_resolver)?.unwrap();
        let expected_cell_path = CellPath::testing_new("cell1", "foo/bar");
        assert_eq!(actual_cell_path, expected_cell_path);

        let actual_cell_path =
            parse_cell_path_as_file_expr_literal("@cell1//foo/bar", &cell_alias_resolver)?.unwrap();
        let expected_cell_path = CellPath::testing_new("cell1", "foo/bar");
        assert_eq!(actual_cell_path, expected_cell_path);

        let actual_cell_path =
            parse_cell_path_as_file_expr_literal("cell2//foo/bar", &cell_alias_resolver)?.unwrap();
        let expected_cell_path = CellPath::testing_new("cell2", "foo/bar");
        assert_eq!(actual_cell_path, expected_cell_path);

        let actual_cell_path =
            parse_cell_path_as_file_expr_literal("@cell3//foo/bar", &cell_alias_resolver)?.unwrap();
        let expected_cell_path = CellPath::testing_new("cell3", "foo/bar");
        assert_eq!(actual_cell_path, expected_cell_path);

        assert!(
            parse_cell_path_as_file_expr_literal("/abs/path/foo/bar", &cell_alias_resolver)?
                .is_none()
        );
        assert!(
            parse_cell_path_as_file_expr_literal("rel/path/foo/bar", &cell_alias_resolver)?
                .is_none()
        );

        Ok(())
    }
}
