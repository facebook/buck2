/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellAliasResolver;
use buck2_core::package::Package;
use thiserror::Error;

use crate::parse_import::parse_import;

#[derive(Error, Debug)]
enum PackageImportsError {
    #[error("Expected value to contain `=>`. Got `{0}`.")]
    MissingArrow(String),
    #[error("Expected value to contain `::`. Got `{0}`.")]
    MissingColons(String),
}

#[derive(Debug, Eq, PartialEq)]
pub struct ImplicitImport {
    import: ImportPath,
    // Oddly buckv1 allows renaming symbols for these imports.
    symbols: HashMap<String, String>,
}

impl ImplicitImport {
    pub fn import(&self) -> &ImportPath {
        &self.import
    }

    pub fn lookup_alias<'a>(&'a self, name: &'a str) -> &'a str {
        match self.symbols.get(name) {
            Some(v) => v,
            None => name,
        }
    }
}

/// Supports parsing and resolution of package implicit imports.
#[derive(PartialEq, Debug)]
pub struct PackageImplicitImports {
    /// It would probably be a little nicer if this were a sequence_trie, but
    /// that doesn't support Borrow in the same way normal maps do, so in it's
    /// current state it's unclear if it would be better.
    mappings: HashMap<CellRelativePathBuf, Arc<ImplicitImport>>,
}

impl PackageImplicitImports {
    /// Mappings are encoded roughly like so:
    ///
    /// full_mappings: `package_mapping`,`package_mapping`,...
    /// package_mapping:
    /// `package_path`=>`import_path`::`symbol_spec`::`symbol_spec`::
    /// `symbol_spec`::... symbol_spec: `alias`=`symbol` | `symbol`
    pub fn new(
        cell_name: &BuildFileCell,
        cell_alias_resolver: CellAliasResolver,
        encoded_mappings: Option<&str>,
    ) -> anyhow::Result<Self> {
        let mut mappings = HashMap::new();
        if let Some(value) = encoded_mappings {
            let root_path = CellPath::new(
                cell_name.name().clone(),
                CellRelativePathBuf::unchecked_new("".to_owned()),
            );
            for item in value.split(',') {
                let (dir, import_spec) = item.trim().split_once("=>").ok_or_else(|| {
                    anyhow::anyhow!(PackageImportsError::MissingArrow(item.to_owned()))
                })?;
                let (import, symbol_specs) = import_spec.split_once("::").ok_or_else(|| {
                    anyhow::anyhow!(PackageImportsError::MissingColons(import_spec.to_owned()))
                })?;
                let import_path = parse_import(&cell_alias_resolver, &root_path, import)?;
                // Package implicit imports are only going to be used for a top-level module in
                // the same cell, so we can set that early.
                let import_path = ImportPath::new(import_path, cell_name.clone())?;
                let mut symbols = HashMap::new();
                for spec in symbol_specs.split("::") {
                    let (alias, symbol) = match spec.split_once('=') {
                        Some(v) => v,
                        None => (spec, spec),
                    };
                    symbols.insert(alias.to_owned(), symbol.to_owned());
                }

                mappings.insert(
                    CellRelativePathBuf::try_from(dir.to_owned())?,
                    Arc::new(ImplicitImport {
                        import: import_path,
                        symbols,
                    }),
                );
            }
        }
        Ok(Self { mappings })
    }

    pub fn get(&self, package: &Package) -> Option<&Arc<ImplicitImport>> {
        let package_dir = package.cell_relative_path();
        let mut package_dir: Option<&CellRelativePath> = Some(package_dir);

        while let Some(d) = package_dir {
            if let Some(v) = self.mappings.get(d) {
                return Some(v);
            }
            package_dir = d.parent();
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_core::cells::CellAlias;
    use buck2_core::cells::CellAliasResolver;
    use buck2_core::cells::CellName;
    use buck2_core::package::testing::PackageExt;

    use super::*;

    #[test]
    fn test() -> anyhow::Result<()> {
        let cell_alias_resolver = CellAliasResolver::new(Arc::new(
            vec![("", "root"), ("root", "root"), ("cell1", "cell1")]
                .into_iter()
                .map(|(alias, name)| {
                    (
                        CellAlias::new(alias.to_owned()),
                        CellName::unchecked_new(name.to_owned()),
                    )
                })
                .collect(),
        ))?;

        let root_name = CellName::unchecked_new("root".to_owned());
        let imports = PackageImplicitImports::new(
            &BuildFileCell::new(root_name),
            cell_alias_resolver,
            Some(
                "src=>//:src.bzl::symbols,src/bin=>//:bin.bzl::symbols , other=>@cell1//:other.bzl::alias1=symbol1::alias2=symbol2::symbol3",
            ),
        )?;

        assert_eq!(None, imports.get(&Package::testing_new("root", "java/src")));

        let expect_import = |cell, path| {
            let package = Package::testing_new(cell, path);
            match imports.get(&package) {
                None => panic!("Should've had implicit import for {}", package),
                Some(v) => v,
            }
        };

        let import = expect_import("root", "src/java/com");
        assert_eq!("root//src.bzl", import.import().id().as_str());
        assert_eq!("symbols", import.lookup_alias("symbols"));
        assert_eq!("other", import.lookup_alias("other"));

        let import = expect_import("root", "src");
        assert_eq!("root//src.bzl", import.import().id().as_str());

        let import = expect_import("root", "src/bin");
        assert_eq!("root//bin.bzl", import.import().id().as_str());

        let import = expect_import("root", "other/bin");
        assert_eq!("cell1//other.bzl@root", import.import().id().as_str());
        assert_eq!("symbol1", import.lookup_alias("alias1"));
        assert_eq!("symbol2", import.lookup_alias("alias2"));
        assert_eq!("symbol3", import.lookup_alias("symbol3"));
        assert_eq!("alias3", import.lookup_alias("alias3"));
        assert_eq!("symbol1", import.lookup_alias("symbol1"));

        assert_eq!(None, imports.get(&Package::testing_new("root", "")));
        assert_eq!(
            None,
            imports.get(&Package::testing_new("root", "third_party/src"))
        );

        Ok(())
    }
}
