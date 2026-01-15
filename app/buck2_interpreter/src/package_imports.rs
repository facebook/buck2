/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path_with_allowed_relative_dir::CellPathWithAllowedRelativeDir;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::package::PackageLabel;
use pagable::Pagable;
use starlark_map::ordered_map::OrderedMap;

use crate::parse_import::RelativeImports;
use crate::parse_import::parse_import;

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
enum PackageImportsError {
    #[error("Expected value to contain `=>`. Got `{0}`.")]
    MissingArrow(String),
    #[error("Expected value to contain `::`. Got `{0}`.")]
    MissingColons(String),
}

#[derive(Debug, Eq, PartialEq, Allocative, Pagable)]
pub struct ImplicitImport {
    import: ImportPath,
    // Oddly buckv1 allows renaming symbols for these imports.
    symbols: OrderedMap<String, String>,
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
#[derive(PartialEq, Debug, Allocative, Pagable)]
pub struct PackageImplicitImports {
    /// It would probably be a little nicer if this were a sequence_trie, but
    /// that doesn't support Borrow in the same way normal maps do, so in it's
    /// current state it's unclear if it would be better.
    mappings: OrderedMap<CellRelativePathBuf, Arc<ImplicitImport>>,
}

impl PackageImplicitImports {
    /// Mappings are encoded roughly like so:
    ///
    /// full_mappings: `package_mapping`,`package_mapping`,...
    /// package_mapping:
    /// `package_path`=>`import_path`::`symbol_spec`::`symbol_spec`::
    /// `symbol_spec`::... symbol_spec: `alias`=`symbol` | `symbol`
    pub fn new(
        cell_name: BuildFileCell,
        cell_alias_resolver: CellAliasResolver,
        encoded_mappings: Option<&str>,
    ) -> buck2_error::Result<Self> {
        let mut mappings = OrderedMap::new();
        if let Some(value) = encoded_mappings {
            let root_path = CellPath::new(
                cell_name.name(),
                CellRelativePathBuf::unchecked_new("".to_owned()),
            );
            for item in value.split(',') {
                let (dir, import_spec) = item
                    .trim()
                    .split_once("=>")
                    .ok_or_else(|| PackageImportsError::MissingArrow(item.to_owned()))?;
                let (import, symbol_specs) = import_spec
                    .split_once("::")
                    .ok_or_else(|| PackageImportsError::MissingColons(import_spec.to_owned()))?;
                let relative_import_option = RelativeImports::Allow {
                    current_dir_with_allowed_relative: &CellPathWithAllowedRelativeDir::new(
                        root_path.clone(),
                        None,
                    ),
                };
                let import_path =
                    parse_import(&cell_alias_resolver, relative_import_option, import)?;
                // Package implicit imports are only going to be used for a top-level module in
                // the same cell, so we can set that early.
                let import_path = ImportPath::new_with_build_file_cells(import_path, cell_name)?;
                let mut symbols = OrderedMap::new();
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

    pub fn get(&self, package: PackageLabel) -> Option<&Arc<ImplicitImport>> {
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

    use buck2_core::cells::alias::NonEmptyCellAlias;
    use buck2_core::cells::name::CellName;
    use dupe::Dupe;

    use super::*;

    #[test]
    fn test() -> buck2_error::Result<()> {
        let cell_alias_resolver = CellAliasResolver::new(
            CellName::testing_new("root"),
            vec![("root", "root"), ("cell1", "cell1")]
                .into_iter()
                .map(|(alias, name)| {
                    (
                        NonEmptyCellAlias::new(alias.to_owned()).unwrap(),
                        CellName::testing_new(name),
                    )
                })
                .collect(),
        )?;

        let root_name = CellName::testing_new("root");
        let imports = PackageImplicitImports::new(
            BuildFileCell::new(root_name),
            cell_alias_resolver,
            Some(
                "src=>//:src.bzl::symbols,src/bin=>//:bin.bzl::symbols , other=>@cell1//:other.bzl::alias1=symbol1::alias2=symbol2::symbol3",
            ),
        )?;

        assert_eq!(
            None,
            imports.get(PackageLabel::testing_parse("root//java/src"))
        );

        let expect_import = |cell, path| {
            let package = PackageLabel::testing_new(cell, path);
            match imports.get(package.dupe()) {
                None => panic!("Should've had implicit import for {package}"),
                Some(v) => v,
            }
        };

        let import = expect_import("root", "src/java/com");
        assert_eq!("root//src.bzl", import.import().to_string());
        assert_eq!("symbols", import.lookup_alias("symbols"));
        assert_eq!("other", import.lookup_alias("other"));

        let import = expect_import("root", "src");
        assert_eq!("root//src.bzl", import.import().to_string());

        let import = expect_import("root", "src/bin");
        assert_eq!("root//bin.bzl", import.import().to_string());

        let import = expect_import("root", "other/bin");
        assert_eq!("cell1//other.bzl@root", import.import().to_string());
        assert_eq!("symbol1", import.lookup_alias("alias1"));
        assert_eq!("symbol2", import.lookup_alias("alias2"));
        assert_eq!("symbol3", import.lookup_alias("symbol3"));
        assert_eq!("alias3", import.lookup_alias("alias3"));
        assert_eq!("symbol1", import.lookup_alias("symbol1"));

        assert_eq!(None, imports.get(PackageLabel::testing_parse("root//")));
        assert_eq!(
            None,
            imports.get(PackageLabel::testing_parse("root//third_party/src"))
        );

        Ok(())
    }
}
