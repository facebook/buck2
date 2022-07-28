/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;

use crate::cells::paths::CellRelativePath;
use crate::cells::CellAliasResolver;
use crate::fs::paths::ForwardRelativePath;
use crate::package::Package;

#[derive(Debug, thiserror::Error)]
enum ParsePackageError {
    #[error("Package should contain `//`: `{0}`")]
    NoSlashSlash(String),
}

/// Parse package without `@` in cell name.
pub fn parse_package(
    package: &str,
    cell_alias_resolver: &CellAliasResolver,
) -> anyhow::Result<Package> {
    // There's no ready to use parser for package, so create simple one here.
    let (cell, cell_relative) = package
        .split_once("//")
        .ok_or_else(|| ParsePackageError::NoSlashSlash(package.to_owned()))?;

    let cell = cell_alias_resolver.resolve(cell).with_context(|| format!(
        "when trying to resolve cell alias `{}` for path {}",
        cell, package
    ))?;
    let cell_relative =
        ForwardRelativePath::new(cell_relative).context("Parsing package argument")?;
    let cell_relative = CellRelativePath::new(cell_relative);

    Ok(Package::new(cell, cell_relative))
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::cells::CellAlias;
    use crate::cells::CellAliasResolver;
    use crate::cells::CellName;
    use crate::pattern::parse_package::parse_package;

    #[test]
    fn test_parse_package() {
        let package = parse_package(
            "foo//bar/baz",
            &CellAliasResolver::new(Arc::new(
                [
                    (
                        CellAlias::new("foo".to_owned()),
                        CellName::unchecked_new("oof".to_owned()),
                    ),
                    (
                        CellAlias::new("".to_owned()),
                        CellName::unchecked_new("oof".to_owned()),
                    ),
                ]
                .into_iter()
                .collect(),
            ))
            .unwrap(),
        )
        .unwrap();
        assert_eq!("oof", package.cell_name().as_str());
        assert_eq!("bar/baz", package.cell_relative_path().as_str());
    }
}
