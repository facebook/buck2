/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use buck2_fs::paths::file_name::FileName;
use dupe::Dupe;
use pagable::Pagable;
use strong_hash::StrongHash;

use crate::cells::build_file_cell::BuildFileCell;
use crate::cells::cell_path::CellPath;
use crate::cells::cell_path::CellPathRef;
use crate::cells::name::CellName;
use crate::cells::paths::CellRelativePath;

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum ImportPathError {
    #[error("Invalid import path `{0}`")]
    Invalid(CellPath),
    #[error(
        "Import path must have suffix `.bzl`, `.json`, or `.toml`, or specify `?format=`: `{0}`"
    )]
    Suffix(CellPath),
    #[error("Unknown load format `{0}`, expected one of `bzl`, `json`, or `toml`")]
    UnknownFormat(String),
}

/// How the contents of a loaded file are parsed.
#[derive(
    Clone,
    Copy,
    Dupe,
    Hash,
    StrongHash,
    Eq,
    PartialEq,
    Debug,
    Allocative,
    Pagable
)]
pub enum LoadFormat {
    Bzl,
    Json,
    Toml,
}

impl LoadFormat {
    pub const fn as_str(self) -> &'static str {
        match self {
            LoadFormat::Bzl => "bzl",
            LoadFormat::Json => "json",
            LoadFormat::Toml => "toml",
        }
    }

    pub fn parse(format: &str) -> buck2_error::Result<Self> {
        match format {
            "bzl" => Ok(LoadFormat::Bzl),
            "json" => Ok(LoadFormat::Json),
            "toml" => Ok(LoadFormat::Toml),
            _ => Err(ImportPathError::UnknownFormat(format.to_owned()).into()),
        }
    }

    fn from_extension(extension: Option<&str>) -> Self {
        match extension {
            Some("json") => LoadFormat::Json,
            Some("toml") => LoadFormat::Toml,
            _ => LoadFormat::Bzl,
        }
    }
}

impl Display for LoadFormat {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Path of a `.bzl` file.
#[derive(Clone, Hash, StrongHash, Eq, PartialEq, Debug, Allocative, Pagable)]
pub struct ImportPath {
    /// The path to the import as a 'CellPath', which contains the cell
    /// information and the cell relative path to the bzl file itself, including the bzl suffix
    path: CellPath,
    /// The cell of the top-level build module that this is being loaded
    /// (perhaps transitively) into.
    build_file_cell: BuildFileCell,
    /// Explicit `?format=` from the `load()` string, or `None` to infer from the extension.
    /// This participates in equality and hashing: the same file loaded under two formats is
    /// two distinct modules, and both must remain separately cached and invalidated.
    format: Option<LoadFormat>,
}

impl ImportPath {
    /// We evaluate `bzl` files multiple times: for each cell we evaluate `bzl` file again.
    /// We want to stop doing that.
    /// This function is for call sites where we don't care about the build file cell.
    pub fn new_same_cell(path: CellPath) -> buck2_error::Result<Self> {
        let build_file_cell = BuildFileCell::new(path.cell());
        Self::new_with_build_file_cells(path, build_file_cell)
    }

    pub fn new_with_build_file_cells(
        path: CellPath,
        build_file_cell: BuildFileCell,
    ) -> buck2_error::Result<Self> {
        Self::new_with_format(path, build_file_cell, None)
    }

    /// Like [`ImportPath::new_with_build_file_cells`], but with an explicit `?format=` from the
    /// `load()` string. An explicit format lifts the extension requirement, so that e.g.
    /// `Cargo.lock` can be loaded as TOML.
    pub fn new_with_format(
        path: CellPath,
        build_file_cell: BuildFileCell,
        format: Option<LoadFormat>,
    ) -> buck2_error::Result<Self> {
        if path.parent().is_none() {
            return Err(ImportPathError::Invalid(path).into());
        }

        if path.path().as_str().contains('?') {
            return Err(ImportPathError::Invalid(path).into());
        }

        let extension_is_known = matches!(path.path().extension(), Some("bzl" | "json" | "toml"));
        if format.is_none() && !extension_is_known {
            return Err(ImportPathError::Suffix(path).into());
        }

        // A format that merely restates the extension must collapse to `None`, so that
        // `:a.bzl?format=bzl` and `:a.bzl` are one module rather than two. Evaluating the same
        // `.bzl` twice would hand out two copies of its providers and transitive sets, and
        // anything doing pointer equality on those would then silently go wrong.
        let format = match format {
            Some(format)
                if extension_is_known
                    && format == LoadFormat::from_extension(path.path().extension()) =>
            {
                None
            }
            format => format,
        };

        Ok(Self {
            path,
            build_file_cell,
            format,
        })
    }

    /// LSP creates imports for non-bzl files.
    pub fn new_hack_for_lsp(
        path: CellPath,
        build_file_cell: BuildFileCell,
    ) -> buck2_error::Result<Self> {
        if path.parent().is_none() {
            return Err(ImportPathError::Invalid(path).into());
        }

        if path.path().as_str().contains('?') {
            return Err(ImportPathError::Invalid(path).into());
        }

        Ok(Self {
            path,
            build_file_cell,
            format: None,
        })
    }

    pub fn testing_new(path: &str) -> Self {
        let (cell, rem) = path.split_once("//").unwrap();
        let (cell_relative_path, filename) = rem.rsplit_once(':').unwrap();
        Self::testing_new_cross_cell(cell, cell_relative_path, filename, cell)
    }

    pub fn testing_new_cross_cell(
        cell: &str,
        cell_relative_path: &str,
        filename: &str,
        build_file_cell: &str,
    ) -> Self {
        let cell_path = CellPath::new(
            CellName::testing_new(cell),
            CellRelativePath::unchecked_new(cell_relative_path)
                .join(FileName::unchecked_new(filename)),
        );
        Self::new_with_build_file_cells(
            cell_path,
            BuildFileCell::new(CellName::testing_new(build_file_cell)),
        )
        .unwrap()
    }

    pub fn cell(&self) -> CellName {
        self.path.cell()
    }

    pub fn build_file_cell(&self) -> BuildFileCell {
        self.build_file_cell
    }

    pub fn path(&self) -> &CellPath {
        &self.path
    }

    /// How this file should be parsed: the explicit `?format=`, else inferred from the extension.
    pub fn load_format(&self) -> LoadFormat {
        self.format
            .unwrap_or_else(|| LoadFormat::from_extension(self.path.path().extension()))
    }

    /// Whether [`ImportPath::load_format`] came from an explicit `?format=` rather than the
    /// extension. Only useful for diagnostics: the extension is otherwise the reader's only
    /// clue as to how the file was parsed.
    pub fn has_explicit_format(&self) -> bool {
        self.format.is_some()
    }

    /// Parent directory of the import path.
    pub fn path_parent(&self) -> CellPathRef<'_> {
        self.path
            .parent()
            .expect("constructor verified path has parent")
    }
}

impl Display for ImportPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.build_file_cell.name() == self.path.cell() {
            write!(f, "{}", self.path)?;
        } else {
            write!(f, "{}@{}", self.path, self.build_file_cell.name())?;
        }
        if let Some(format) = self.format {
            write!(f, "?format={format}")?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn import(filename: &str, format: Option<LoadFormat>) -> buck2_error::Result<ImportPath> {
        let path = CellPath::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("pkg").join(FileName::unchecked_new(filename)),
        );
        ImportPath::new_with_format(
            path,
            BuildFileCell::new(CellName::testing_new("root")),
            format,
        )
    }

    #[test]
    fn format_is_inferred_from_extension() {
        assert_eq!(LoadFormat::Bzl, import("a.bzl", None).unwrap().load_format());
        assert_eq!(
            LoadFormat::Json,
            import("a.json", None).unwrap().load_format()
        );
        assert_eq!(
            LoadFormat::Toml,
            import("a.toml", None).unwrap().load_format()
        );
    }

    #[test]
    fn unknown_extension_requires_an_explicit_format() {
        for filename in ["Cargo.lock", "Makefile", "rules.bzl.in"] {
            assert!(import(filename, None).is_err(), "{filename}");

            let explicit = import(filename, Some(LoadFormat::Toml)).unwrap();
            assert_eq!(LoadFormat::Toml, explicit.load_format(), "{filename}");
            assert!(explicit.has_explicit_format(), "{filename}");
        }
    }

    #[test]
    fn an_extensionless_file_keeps_a_redundant_looking_format() {
        // `from_extension` defaults to `Bzl`, but an extensionless file has no extension for the
        // format to be redundant with, so it must stay explicit or the path would not construct.
        let explicit = import("Makefile", Some(LoadFormat::Bzl)).unwrap();
        assert_eq!(LoadFormat::Bzl, explicit.load_format());
        assert!(explicit.has_explicit_format());
    }

    #[test]
    fn explicit_format_overrides_the_extension() {
        let as_toml = import("a.json", Some(LoadFormat::Toml)).unwrap();
        assert_eq!(LoadFormat::Toml, as_toml.load_format());
        // The same file under two formats must stay two distinct modules.
        assert_ne!(import("a.json", None).unwrap(), as_toml);
    }

    #[test]
    fn redundant_format_collapses_so_the_module_is_not_duplicated() {
        for (filename, format) in [
            ("a.bzl", LoadFormat::Bzl),
            ("a.json", LoadFormat::Json),
            ("a.toml", LoadFormat::Toml),
        ] {
            let explicit = import(filename, Some(format)).unwrap();
            assert_eq!(import(filename, None).unwrap(), explicit);
            assert!(!explicit.has_explicit_format());
        }
    }

    #[test]
    fn display_round_trips_an_explicit_format() {
        assert_eq!("root//pkg/a.bzl", import("a.bzl", None).unwrap().to_string());
        assert_eq!(
            "root//pkg/Cargo.lock?format=toml",
            import("Cargo.lock", Some(LoadFormat::Toml))
                .unwrap()
                .to_string()
        );
    }

    #[test]
    fn a_path_containing_a_question_mark_is_still_rejected() {
        assert!(import("a.bzl?format=toml", Some(LoadFormat::Toml)).is_err());
    }
}
