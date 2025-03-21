/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_error::BuckErrorContext;

use crate::cells::paths::CellRelativePath;
use crate::cells::CellAliasResolver;
use crate::fs::paths::forward_rel_path::ForwardRelativePath;
use crate::package::PackageLabel;

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum ParsePackageError {
    #[error("Package should contain `//`: `{0}`")]
    NoSlashSlash(String),
}

/// Parse package without `@` in cell name.
pub fn parse_package(
    package: &str,
    cell_alias_resolver: &CellAliasResolver,
) -> buck2_error::Result<PackageLabel> {
    // There's no ready to use parser for package, so create simple one here.
    let (cell, cell_relative) = package
        .split_once("//")
        .ok_or_else(|| ParsePackageError::NoSlashSlash(package.to_owned()))?;

    let cell = cell_alias_resolver.resolve(cell)?;
    let cell_relative =
        ForwardRelativePath::new(cell_relative).buck_error_context("Parsing package argument")?;
    let cell_relative = CellRelativePath::new(cell_relative);

    Ok(PackageLabel::new(cell, cell_relative))
}

#[cfg(test)]
mod tests {

    use crate::cells::alias::NonEmptyCellAlias;
    use crate::cells::name::CellName;
    use crate::cells::CellAliasResolver;
    use crate::pattern::parse_package::parse_package;

    #[test]
    fn test_parse_package() {
        let package = parse_package(
            "foo//bar/baz",
            &CellAliasResolver::new(
                CellName::testing_new("oof"),
                [(
                    NonEmptyCellAlias::new("foo".to_owned()).unwrap(),
                    CellName::testing_new("oof"),
                )]
                .into_iter()
                .collect(),
            )
            .unwrap(),
        )
        .unwrap();
        assert_eq!("oof", package.cell_name().as_str());
        assert_eq!("bar/baz", package.cell_relative_path().as_str());
    }
}
