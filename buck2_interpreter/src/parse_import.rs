/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Parses imports for load_file() calls in build files.

use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellAliasResolver;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use thiserror::Error;

#[derive(Error, Debug)]
enum ImportParseError {
    #[error(
        "Unable to parse import spec. Expected format `(@<cell>)//package/name:filename.bzl` or `:filename.bzl`. Got `{0}`"
    )]
    MatchFailed(String),
    #[error(
        "Unable to parse import spec. Expected format `(@<cell>)//package/name:filename.bzl` or `:filename.bzl`, but got an empty filename. Got `{0}`"
    )]
    EmptyFileName(String),
    #[error("Unexpected relative import spec. Got `{0}`")]
    ProhibitedRelativeImport(String),
    #[error(
        "Invalid path `{0}` for file relative import path. Should be a forward relative path."
    )]
    InvalidCurrentPathWhenFileRelativeImport(String),
    #[error(
        "Unable to parse import spec. Expected format `(@<cell>)//package/name:filename.bzl` or `:filename.bzl`, but got a path. Got `{0}`"
    )]
    NotAFileName(String),
}

/// Extra options for parsing a load() or load-like path into a `BuckPath`
pub struct ParseImportOptions {
    /// Whether '@' is required at the beginning of the import.
    pub allow_missing_at_symbol: bool,
    /// Whether relative imports (':bar.bzl') are allowed.
    pub allow_relative_imports: bool,
}

// Parses a string of the form `(@<cell>)//dir/name` to the corresponding
// alias and cell relative path.
fn parse_import_cell_path_parts(path: &str, allow_missing_at_symbol: bool) -> Option<(&str, &str)> {
    let (alias, cell_rel_path) = path.split_once("//")?;
    let alias = if alias.is_empty() {
        alias
    } else if !alias.starts_with('@') {
        if !allow_missing_at_symbol {
            return None;
        }
        alias
    } else {
        &alias[1..]
    };
    Some((alias, cell_rel_path))
}

pub fn parse_import(
    cell_resolver: &CellAliasResolver,
    current_path: &CellPath,
    import: &str,
) -> anyhow::Result<CellPath> {
    const OPTS: ParseImportOptions = ParseImportOptions {
        allow_missing_at_symbol: false,
        allow_relative_imports: true,
    };
    parse_import_with_config(cell_resolver, current_path, import, &OPTS)
}

/// Parse import string into a BuckPath, but potentially be more or less flexible with what is
/// accepted.
///
/// Common use case is e.g. allowing "fbsource//foo:bar.bzl" to be passed on the command line
/// and letting that be turned into an ImportPath eventually, or disallowing relative imports
/// from command line arguments.
///
/// Strings for the `load()` statement in starlark files should use [`parse_import`]
pub fn parse_import_with_config(
    cell_resolver: &CellAliasResolver,
    current_dir: &CellPath,
    import: &str,
    opts: &ParseImportOptions,
) -> anyhow::Result<CellPath> {
    match import.split_once(':') {
        None => {
            // import without `:`, so just try to parse the cell and cell relative paths

            match parse_import_cell_path_parts(import, opts.allow_missing_at_symbol) {
                None => {
                    if opts.allow_relative_imports {
                        let rel_path = ForwardRelativePath::new(import).map_err(|_e| {
                            ImportParseError::InvalidCurrentPathWhenFileRelativeImport(
                                import.to_owned(),
                            )
                        })?;
                        Ok(current_dir.join(rel_path))
                    } else {
                        Err(anyhow::anyhow!(ImportParseError::ProhibitedRelativeImport(
                            import.to_owned()
                        )))
                    }
                }
                Some((alias, cell_relative_path)) => {
                    let cell = cell_resolver.resolve(alias)?;
                    Ok(CellPath::new(
                        cell.clone(),
                        CellRelativePathBuf::try_from(cell_relative_path.to_owned())?,
                    ))
                }
            }
        }
        Some((path, filename)) => {
            if filename.is_empty() {
                return Err(anyhow::anyhow!(ImportParseError::EmptyFileName(
                    import.to_owned()
                )));
            }

            let filename = FileName::new(filename)
                .map_err(|_| ImportParseError::NotAFileName(import.to_owned()))?;

            if path.is_empty() {
                if opts.allow_relative_imports {
                    Ok(current_dir.join(filename))
                } else {
                    Err(anyhow::anyhow!(ImportParseError::ProhibitedRelativeImport(
                        import.to_owned()
                    )))
                }
            } else {
                let (alias, cell_relative_path) =
                    parse_import_cell_path_parts(path, opts.allow_missing_at_symbol)
                        .ok_or_else(|| ImportParseError::MatchFailed(import.to_owned()))?;
                let cell = cell_resolver.resolve(alias)?;
                Ok(CellPath::new(
                    cell.clone(),
                    <&CellRelativePath>::try_from(cell_relative_path)?.join(filename),
                ))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::sync::Arc;

    use buck2_core::cells::CellAlias;
    use buck2_core::cells::CellAliasResolver;
    use buck2_core::cells::CellName;

    use super::*;

    fn resolver() -> CellAliasResolver {
        let mut m = HashMap::new();
        m.insert(
            CellAlias::new("".to_owned()),
            CellName::unchecked_new("root".to_owned()),
        );
        m.insert(
            CellAlias::new("cell1".to_owned()),
            CellName::unchecked_new("cell1".to_owned()),
        );
        m.insert(
            CellAlias::new("alias2".to_owned()),
            CellName::unchecked_new("cell2".to_owned()),
        );
        CellAliasResolver::new(Arc::new(m)).expect("valid resolver")
    }

    fn dir(cell: &str, path: &str) -> CellPath {
        CellPath::testing_new(cell, path)
    }

    fn path(cell: &str, dir: &str, filename: &str) -> CellPath {
        CellPath::new(
            CellName::unchecked_new(cell.to_owned()),
            CellRelativePath::unchecked_new(dir).join(FileName::unchecked_new(filename)),
        )
    }

    #[test]
    fn root_package() -> anyhow::Result<()> {
        assert_eq!(
            path("root", "package/path", "import.bzl"),
            parse_import(&resolver(), &dir("", ""), "//package/path:import.bzl")?
        );
        Ok(())
    }

    #[test]
    fn cell_package() -> anyhow::Result<()> {
        assert_eq!(
            path("cell1", "package/path", "import.bzl"),
            parse_import(&resolver(), &dir("", ""), "@cell1//package/path:import.bzl")?
        );
        Ok(())
    }

    #[test]
    fn package_relative() -> anyhow::Result<()> {
        assert_eq!(
            path("cell1", "package/path", "import.bzl"),
            parse_import(&resolver(), &dir("cell1", "package/path"), ":import.bzl")?
        );
        Ok(())
    }

    #[test]
    fn missing_colon() -> anyhow::Result<()> {
        let import = "//package/path/import.bzl".to_owned();
        assert_eq!(
            parse_import(&resolver(), &dir("", ""), &import)?,
            path("root", "package/path", "import.bzl")
        );
        Ok(())
    }

    #[test]
    fn empty_filename() -> anyhow::Result<()> {
        let path = "//package/path:".to_owned();
        match parse_import(&resolver(), &dir("", ""), &path) {
            Ok(import) => panic!("Expected parse failure for {}, got result {}", path, import),
            Err(e) => {
                assert_eq!(
                    format!("{:#}", e),
                    ImportParseError::EmptyFileName(path.to_owned()).to_string()
                );
            }
        }
        Ok(())
    }

    #[test]
    fn bad_alias() -> anyhow::Result<()> {
        let path = "bad_alias//package/path:".to_owned();
        match parse_import(&resolver(), &dir("", ""), &path) {
            Ok(import) => panic!("Expected parse failure for {}, got result {}", path, import),
            Err(_) => {
                // TODO: should we verify the contents of the error?
            }
        }
        Ok(())
    }

    #[test]
    fn file_relative_import_given_relative_paths_allowed() -> anyhow::Result<()> {
        assert_eq!(
            path("cell1", "package/path", "bar.bzl"),
            parse_import(&resolver(), &dir("cell1", "package/path"), "bar.bzl",)?
        );
        assert_eq!(
            path("cell1", "package/path", "foo/bar.bzl"),
            parse_import(&resolver(), &dir("cell1", "package/path"), "foo/bar.bzl",)?
        );
        Ok(())
    }

    #[test]
    fn cell_relative_import_given_relative_paths_allowed() -> anyhow::Result<()> {
        let importer = dir("cell1", "package/path");
        let importee = "foo/bar.bzl";

        assert_eq!(
            parse_import(&resolver(), &importer, importee)?,
            path("cell1", "package/path/foo", "bar.bzl")
        );
        Ok(())
    }

    #[test]
    fn regular_import_given_relative_paths_allowed() -> anyhow::Result<()> {
        assert_eq!(
            path("cell1", "package/path", "import.bzl"),
            parse_import(
                &resolver(),
                &dir("", "foo/bar"),
                "@cell1//package/path:import.bzl",
            )?
        );
        Ok(())
    }

    #[test]
    fn allows_non_at_symbols() -> anyhow::Result<()> {
        assert_eq!(
            path("cell1", "package/path", "import.bzl"),
            parse_import_with_config(
                &resolver(),
                &dir("", ""),
                "cell1//package/path:import.bzl",
                &ParseImportOptions {
                    allow_missing_at_symbol: true,
                    allow_relative_imports: true
                }
            )?,
        );
        Ok(())
    }

    #[test]
    fn fails_relative_import_if_disallowed() -> anyhow::Result<()> {
        let imported_file = ":bar.bzl";
        let res = parse_import_with_config(
            &resolver(),
            &dir("", ""),
            imported_file,
            &ParseImportOptions {
                allow_missing_at_symbol: false,
                allow_relative_imports: false,
            },
        );
        match res {
            Ok(res) => panic!(
                "Expected parse failure for {}, got result {}",
                imported_file, res
            ),
            Err(e) => {
                assert_eq!(
                    format!("{:#}", e),
                    ImportParseError::ProhibitedRelativeImport(imported_file.to_owned())
                        .to_string()
                );
            }
        };
        Ok(())
    }
}
