/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Parses imports for load_file() calls in build files.

use buck2_core::bzl::ImportPath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path_with_allowed_relative_dir::CellPathWithAllowedRelativeDir;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::fs::paths::RelativePath;
use buck2_core::fs::paths::file_name::FileName;

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
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
        "Unable to parse import spec. Expected format `(@<cell>)//package/name:filename.bzl` or `:filename.bzl`, but got a path. Got `{0}`"
    )]
    NotAFileName(String),
}

pub enum RelativeImports<'a> {
    Allow {
        current_dir_with_allowed_relative: &'a CellPathWithAllowedRelativeDir,
    },
    Disallow,
}

/// Extra options for parsing a load() or load-like path into a `BuckPath`
pub struct ParseImportOptions<'a> {
    /// Whether '@' is required at the beginning of the import.
    pub allow_missing_at_symbol: bool,
    /// Whether relative imports (':bar.bzl') are allowed.
    pub relative_import_option: RelativeImports<'a>,
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
    relative_import_option: RelativeImports,
    import: &str,
) -> buck2_error::Result<CellPath> {
    let opts: ParseImportOptions = ParseImportOptions {
        allow_missing_at_symbol: false,
        relative_import_option,
    };
    parse_import_with_config(cell_resolver, import, &opts)
}

/// Parse import string into a BuckPath, but potentially be more or less flexible with what is
/// accepted.
///
/// Common use case is e.g. allowing "cell//foo:bar.bzl" to be passed on the command line
/// and letting that be turned into an ImportPath eventually, or disallowing relative imports
/// from command line arguments.
///
/// Strings for the `load()` statement in starlark files should use [`parse_import`]
pub fn parse_import_with_config(
    cell_resolver: &CellAliasResolver,
    import: &str,
    opts: &ParseImportOptions,
) -> buck2_error::Result<CellPath> {
    match import.split_once(':') {
        None => {
            // import without `:`, so just try to parse the cell and cell relative paths

            match parse_import_cell_path_parts(import, opts.allow_missing_at_symbol) {
                None => {
                    if let RelativeImports::Allow {
                        current_dir_with_allowed_relative,
                    } = opts.relative_import_option
                    {
                        current_dir_with_allowed_relative
                            .join_normalized(RelativePath::from_path(import)?)
                    } else {
                        Err(ImportParseError::ProhibitedRelativeImport(import.to_owned()).into())
                    }
                }
                Some((alias, cell_relative_path)) => {
                    let cell = cell_resolver.resolve(alias)?;
                    Ok(CellPath::new(
                        cell,
                        CellRelativePathBuf::try_from(cell_relative_path.to_owned())?,
                    ))
                }
            }
        }
        Some((path, filename)) => {
            if filename.is_empty() {
                return Err(ImportParseError::EmptyFileName(import.to_owned()).into());
            }

            let filename = FileName::new(filename)
                .map_err(|_| ImportParseError::NotAFileName(import.to_owned()))?;

            if path.is_empty() {
                if let RelativeImports::Allow {
                    current_dir_with_allowed_relative,
                } = opts.relative_import_option
                {
                    Ok(current_dir_with_allowed_relative.join(filename))
                } else {
                    Err(ImportParseError::ProhibitedRelativeImport(import.to_owned()).into())
                }
            } else {
                let (alias, cell_relative_path) =
                    parse_import_cell_path_parts(path, opts.allow_missing_at_symbol)
                        .ok_or_else(|| ImportParseError::MatchFailed(import.to_owned()))?;
                let cell = cell_resolver.resolve(alias)?;
                Ok(CellPath::new(
                    cell,
                    <&CellRelativePath>::try_from(cell_relative_path)?.join(filename),
                ))
            }
        }
    }
}

pub fn parse_bzl_path_with_config(
    cell_resolver: &CellAliasResolver,
    import: &str,
    opts: &ParseImportOptions,
    build_cell_path: BuildFileCell,
) -> buck2_error::Result<ImportPath> {
    let path = parse_import_with_config(cell_resolver, import, opts)?;
    ImportPath::new_with_build_file_cells(path, build_cell_path)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use buck2_core::cells::alias::NonEmptyCellAlias;
    use buck2_core::cells::name::CellName;

    use super::*;

    fn resolver() -> CellAliasResolver {
        let mut m = HashMap::new();
        m.insert(
            NonEmptyCellAlias::new("cell1".to_owned()).unwrap(),
            CellName::testing_new("cell1"),
        );
        m.insert(
            NonEmptyCellAlias::new("alias2".to_owned()).unwrap(),
            CellName::testing_new("cell2"),
        );
        CellAliasResolver::new(CellName::testing_new("root"), m).expect("valid resolver")
    }

    fn path(cell: &str, dir: &str, filename: &str) -> CellPath {
        CellPath::new(
            CellName::testing_new(cell),
            CellRelativePath::unchecked_new(dir).join(FileName::unchecked_new(filename)),
        )
    }

    #[test]
    fn root_package() -> buck2_error::Result<()> {
        assert_eq!(
            path("root", "package/path", "import.bzl"),
            parse_import(
                &resolver(),
                RelativeImports::Allow {
                    current_dir_with_allowed_relative: &CellPathWithAllowedRelativeDir::new(
                        CellPath::testing_new("passport//"),
                        None,
                    ),
                },
                "//package/path:import.bzl"
            )?
        );
        Ok(())
    }

    #[test]
    fn cell_package() -> buck2_error::Result<()> {
        assert_eq!(
            path("cell1", "package/path", "import.bzl"),
            parse_import(
                &resolver(),
                RelativeImports::Allow {
                    current_dir_with_allowed_relative: &CellPathWithAllowedRelativeDir::new(
                        CellPath::testing_new("root//"),
                        None,
                    ),
                },
                "@cell1//package/path:import.bzl"
            )?
        );
        Ok(())
    }

    #[test]
    fn package_relative() -> buck2_error::Result<()> {
        assert_eq!(
            path("cell1", "package/path", "import.bzl"),
            parse_import(
                &resolver(),
                RelativeImports::Allow {
                    current_dir_with_allowed_relative: &CellPathWithAllowedRelativeDir::new(
                        CellPath::testing_new("cell1//package/path"),
                        None,
                    ),
                },
                ":import.bzl"
            )?
        );
        Ok(())
    }

    #[test]
    fn missing_colon() -> buck2_error::Result<()> {
        let import = "//package/path/import.bzl".to_owned();
        assert_eq!(
            parse_import(
                &resolver(),
                RelativeImports::Allow {
                    current_dir_with_allowed_relative: &CellPathWithAllowedRelativeDir::new(
                        CellPath::testing_new("lighter//"),
                        None,
                    ),
                },
                &import
            )?,
            path("root", "package/path", "import.bzl")
        );
        Ok(())
    }

    #[test]
    fn empty_filename() -> buck2_error::Result<()> {
        let path = "//package/path:".to_owned();
        match parse_import(
            &resolver(),
            RelativeImports::Allow {
                current_dir_with_allowed_relative: &CellPathWithAllowedRelativeDir::new(
                    CellPath::testing_new("root//"),
                    None,
                ),
            },
            &path,
        ) {
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
    fn bad_alias() -> buck2_error::Result<()> {
        let path = "bad_alias//package/path:".to_owned();
        match parse_import(
            &resolver(),
            RelativeImports::Allow {
                current_dir_with_allowed_relative: &CellPathWithAllowedRelativeDir::new(
                    CellPath::testing_new("root//"),
                    None,
                ),
            },
            &path,
        ) {
            Ok(import) => panic!("Expected parse failure for {}, got result {}", path, import),
            Err(_) => {
                // TODO: should we verify the contents of the error?
            }
        }
        Ok(())
    }

    #[test]
    fn file_relative_import_given_relative_paths_allowed() -> buck2_error::Result<()> {
        assert_eq!(
            path("cell1", "package/path", "bar.bzl"),
            parse_import(
                &resolver(),
                RelativeImports::Allow {
                    current_dir_with_allowed_relative: &CellPathWithAllowedRelativeDir::new(
                        CellPath::testing_new("cell1//package/path"),
                        None,
                    ),
                },
                "bar.bzl",
            )?
        );
        assert_eq!(
            path("cell1", "package/path", "foo/bar.bzl"),
            parse_import(
                &resolver(),
                RelativeImports::Allow {
                    current_dir_with_allowed_relative: &CellPathWithAllowedRelativeDir::new(
                        CellPath::testing_new("cell1//package/path"),
                        None,
                    ),
                },
                "foo/bar.bzl",
            )?
        );
        Ok(())
    }

    #[test]
    fn cell_relative_import_given_relative_paths_allowed() -> buck2_error::Result<()> {
        let importer = CellPath::testing_new("cell1//package/path");
        let importee = "foo/bar.bzl";

        assert_eq!(
            parse_import(
                &resolver(),
                RelativeImports::Allow {
                    current_dir_with_allowed_relative: &CellPathWithAllowedRelativeDir::new(
                        importer, None,
                    ),
                },
                importee
            )?,
            path("cell1", "package/path/foo", "bar.bzl")
        );
        Ok(())
    }

    #[test]
    fn regular_import_given_relative_paths_allowed() -> buck2_error::Result<()> {
        assert_eq!(
            path("cell1", "package/path", "import.bzl"),
            parse_import(
                &resolver(),
                RelativeImports::Allow {
                    current_dir_with_allowed_relative: &CellPathWithAllowedRelativeDir::new(
                        CellPath::testing_new("root//foo/bar"),
                        None,
                    ),
                },
                "@cell1//package/path:import.bzl",
            )?
        );
        Ok(())
    }

    #[test]
    fn allows_non_at_symbols() -> buck2_error::Result<()> {
        assert_eq!(
            path("cell1", "package/path", "import.bzl"),
            parse_import_with_config(
                &resolver(),
                "cell1//package/path:import.bzl",
                &ParseImportOptions {
                    allow_missing_at_symbol: true,
                    relative_import_option: RelativeImports::Allow {
                        current_dir_with_allowed_relative: &CellPathWithAllowedRelativeDir::new(
                            CellPath::testing_new("root//"),
                            None,
                        ),
                    },
                }
            )?,
        );
        Ok(())
    }

    #[test]
    fn fails_relative_import_if_disallowed() -> buck2_error::Result<()> {
        let imported_file = ":bar.bzl";
        let res = parse_import_with_config(
            &resolver(),
            imported_file,
            &ParseImportOptions {
                allow_missing_at_symbol: false,
                relative_import_option: RelativeImports::Disallow,
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
