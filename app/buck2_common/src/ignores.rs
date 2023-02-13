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

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::cell_root_path::CellRootPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use dice::DiceComputations;
use globset::Candidate;
use globset::GlobSetBuilder;
use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::Regex;

use crate::dice::cells::HasCellResolver;
use crate::legacy_configs::dice::HasLegacyConfigs;

#[derive(Debug, thiserror::Error)]
enum FileOpsError {
    #[error("Tried to read ignored dir `{0}` (reason: {1}).")]
    ReadIgnoredDir(String, String),
}

pub enum FileIgnoreResult {
    Ok,
    IgnoredByPattern(String, String),
    IgnoredByCell(String, String),
}

impl FileIgnoreResult {
    /// Converts the FileIgnoreResult to a Result<()> where any ignored case is converted to an Err
    /// with appropriate message. This should be used when it would be an error to interact with an
    /// ignored file.
    pub fn into_result(self) -> anyhow::Result<()> {
        match self {
            FileIgnoreResult::Ok => Ok(()),
            FileIgnoreResult::IgnoredByPattern(path, pattern) => {
                Err(anyhow::anyhow!(FileOpsError::ReadIgnoredDir(
                    path,
                    format!("file is matched by pattern `{}`", pattern)
                )))
            }
            FileIgnoreResult::IgnoredByCell(path, cell) => Err(anyhow::anyhow!(
                FileOpsError::ReadIgnoredDir(path, format!("file is part of cell `{}`", cell))
            )),
        }
    }

    /// Returns true if the file is ignored, false otherwise.
    pub fn is_ignored(&self) -> bool {
        match self {
            FileIgnoreResult::Ok => false,
            _ => true,
        }
    }
}

#[derive(Debug, Allocative)]
pub struct IgnoreSet {
    #[allocative(skip)]
    globset: globset::GlobSet,
    // We keep patterns so that error messages can refer to the specific pattern that was matched.
    // This should be in the same order as the strings were added to the GlobSet to match the indices returned from it.
    patterns: Vec<String>,
}

impl PartialEq for IgnoreSet {
    fn eq(&self, other: &Self) -> bool {
        // Only compare patterns because globset is derived from patterns.
        self.patterns == other.patterns
    }
}

impl Eq for IgnoreSet {}

impl IgnoreSet {
    /// Creates an IgnoreSet from an "ignore spec".
    ///
    /// This is modeled after buck1's parsing of project.ignores.
    ///
    /// An ignore spec is a comma-separated list of ignore patterns. If an ignore pattern
    /// contains a glob character, then it uses java.nio.file.FileSystem.getPathMatcher,
    /// otherwise it creates a com.facebook.buck.io.filesystem.RecursivePathMatcher
    ///
    /// Java's path matcher does not allow  '*' to cross directory boundaries. We get
    /// the RecursivePathMatcher behavior by identifying non-globby things and appending
    /// a '/**'.
    pub fn from_ignore_spec(spec: &str) -> anyhow::Result<Self> {
        // TODO(cjhopman): There's opportunity to greatly improve the performance of IgnoreSet by
        // constructing special cases for a couple of common patterns we see in ignore specs. We
        // know that these can get large wins in some places where we've done this same ignore (watchman, buck1's ignores).
        // `**/filename`: a filename filter. These can all be merged into one hashset lookup.
        // `**/*.ext`: an extension filter. These can all be merged into one hashset lookup.
        // `**/*x*x*`: just some general glob on the filename alone, can merge these into one GlobSet that just needs to check against the filename.
        // `some/prefix/**`: a directory prefix. These can all be merged into one trie lookup.
        let mut patterns_builder = GlobSetBuilder::new();
        let mut patterns = Vec::new();
        for val in spec.split(',') {
            let val = val.trim();
            if val.is_empty() {
                continue;
            }

            let val = val.trim_end_matches('/');

            static GLOB_CHARS: Lazy<Regex> = Lazy::new(|| Regex::new(r"[*?{\[]").unwrap());

            if GLOB_CHARS.is_match(val) {
                patterns_builder.add(
                    globset::GlobBuilder::new(val)
                        .literal_separator(true)
                        .build()?,
                );
            } else {
                patterns_builder.add(globset::Glob::new(&format!("{{{},{}/**}}", val, val))?);
            }
            patterns.push(val.to_owned());
        }

        Ok(Self {
            globset: patterns_builder.build()?,
            patterns,
        })
    }

    /// Constructs an IgnoreSet that will ignore anything contained in a deeper cell.
    ///
    /// Ex. if this cell's path is `some/cell` and other cells are at `.`, `other`,
    /// `some/cell/deeper`, this would construct an IgnoreSet to ignore `deeper/**`
    /// (note that these ignores are expected to receive cell-relative paths.)
    fn from_cell_roots(
        all_cells: &[(CellName, &CellRootPath)],
        this_cell: &CellRootPath,
    ) -> anyhow::Result<Self> {
        let mut cells_builder = GlobSetBuilder::new();
        let mut cell_names = Vec::new();
        for (name, path) in all_cells {
            if *path == this_cell {
                continue;
            }

            if !path.starts_with(this_cell) {
                continue;
            }

            let relative = path.strip_prefix(this_cell).unwrap();
            cells_builder.add(globset::Glob::new(&format!(
                "{{{},{}/**}}",
                relative, relative
            ))?);
            cell_names.push(name.as_str().to_owned());
        }

        Ok(Self {
            globset: cells_builder.build()?,
            patterns: cell_names,
        })
    }

    /// Returns a pattern that matches the candidate if there is one.
    fn matches_candidate(&self, candidate: &Candidate) -> Option<&str> {
        match self.globset.matches_candidate(candidate).as_slice() {
            [] => None,
            [v, ..] => Some(&self.patterns[*v]),
        }
    }

    /// Returns whether any pattern matches.
    pub fn is_match(&self, path: &CellRelativePath) -> bool {
        self.globset.is_match(path.as_str())
    }
}

/// Ignores files based on configured ignore patterns and cell paths.
#[derive(PartialEq, Eq, Allocative, Debug)]
pub struct FileIgnores {
    ignores: IgnoreSet,
    cell_ignores: IgnoreSet,
}

/// This is `CellRelativePath` which may contain incorrect path elements.
#[repr(transparent)]
pub(crate) struct MaybeIgnoredCellRelativePath(str);

impl MaybeIgnoredCellRelativePath {
    #[inline]
    pub(crate) fn new(path: &CellRelativePath) -> &MaybeIgnoredCellRelativePath {
        Self::unchecked_new(path.as_str())
    }

    #[inline]
    pub(crate) fn unchecked_new(path: &str) -> &MaybeIgnoredCellRelativePath {
        unsafe {
            // SAFETY: `repr(transparent)`.
            &*(path as *const str as *const MaybeIgnoredCellRelativePath)
        }
    }
}

impl FileIgnores {
    /// Creates a new FileIgnores intended for use by the interpreter.
    ///
    /// This will ignore files/dirs in the ignore spec and those in other cells.
    pub fn new_for_interpreter(
        ignore_spec: &str,
        all_cells: &[(CellName, &CellRootPath)],
        this_cell: &CellRootPath,
    ) -> anyhow::Result<FileIgnores> {
        Ok(FileIgnores {
            ignores: IgnoreSet::from_ignore_spec(ignore_spec)?,
            cell_ignores: IgnoreSet::from_cell_roots(all_cells, this_cell)?,
        })
    }

    pub(crate) fn check(&self, path: &MaybeIgnoredCellRelativePath) -> FileIgnoreResult {
        let candidate = globset::Candidate::new(&path.0);

        if let Some(pattern) = self.ignores.matches_candidate(&candidate) {
            return FileIgnoreResult::IgnoredByPattern(path.0.to_owned(), pattern.to_owned());
        }

        if let Some(pattern) = self.cell_ignores.matches_candidate(&candidate) {
            return FileIgnoreResult::IgnoredByCell(path.0.to_owned(), pattern.to_owned());
        }

        FileIgnoreResult::Ok
    }
}

/// Ignored path configurations for all cells.
#[derive(Allocative, Debug, Eq, PartialEq)]
pub(crate) struct AllCellIgnores {
    ignores: HashMap<CellName, FileIgnores>,
}

impl AllCellIgnores {
    pub(crate) fn check_ignored(
        &self,
        cell: CellName,
        path: &MaybeIgnoredCellRelativePath,
    ) -> anyhow::Result<FileIgnoreResult> {
        Ok(self
            .ignores
            .get(&cell)
            .unwrap_or_else(|| {
                panic!(
                    "Should've had an ignore spec for `{}`. Had `{}`",
                    cell,
                    self.ignores.keys().join(", ")
                )
            })
            .check(path))
    }
}

#[async_trait]
pub(crate) trait HasAllCellIgnores {
    async fn new_all_cell_ignores(&self) -> anyhow::Result<Arc<AllCellIgnores>>;
}

#[async_trait]
impl HasAllCellIgnores for DiceComputations {
    async fn new_all_cell_ignores(&self) -> anyhow::Result<Arc<AllCellIgnores>> {
        let cells = self.get_cell_resolver().await?;
        let configs = self.get_legacy_configs_on_dice().await?;

        let cell_paths: Vec<_> = cells.cells().map(|e| (e.1.name(), e.1.path())).collect();
        let mut ignores = HashMap::new();

        for (cell_name, instance) in cells.cells() {
            let this_path = instance.path();
            let config = configs.get(cell_name).unwrap();
            let ignore_spec = config.get("project", "ignore")?;
            let ignore_spec = ignore_spec.as_ref().map_or("", |s| &**s);

            let cell_ignores =
                FileIgnores::new_for_interpreter(ignore_spec, &cell_paths, this_path)?;
            ignores.insert(cell_name, cell_ignores);
        }

        Ok(Arc::new(AllCellIgnores { ignores }))
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::cells::cell_root_path::CellRootPath;
    use buck2_core::cells::name::CellName;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;

    use crate::ignores::FileIgnores;
    use crate::ignores::MaybeIgnoredCellRelativePath;

    #[test]
    fn file_ignores() -> anyhow::Result<()> {
        let cells = &[
            (
                CellName::testing_new("root"),
                CellRootPath::new(ProjectRelativePath::unchecked_new("root")),
            ),
            (
                CellName::testing_new("other"),
                CellRootPath::new(ProjectRelativePath::unchecked_new("root/other_cell")),
            ),
            (
                CellName::testing_new("third"),
                CellRootPath::new(ProjectRelativePath::unchecked_new("third")),
            ),
        ];
        let ignores = FileIgnores::new_for_interpreter(
            "**/*.java , some/dir/**, one/*, \n    recursive, trailing_slash/",
            cells,
            CellRootPath::new(ProjectRelativePath::unchecked_new("root")),
        )?;

        assert_eq!(
            true,
            ignores
                .check(MaybeIgnoredCellRelativePath::unchecked_new(
                    "some/long/path/Class.java"
                ))
                .is_ignored()
        );

        assert_eq!(
            true,
            ignores
                .check(MaybeIgnoredCellRelativePath::unchecked_new("other_cell"))
                .is_ignored()
        );

        assert_eq!(
            true,
            ignores
                .check(MaybeIgnoredCellRelativePath::unchecked_new(
                    "other_cell/some/lib"
                ))
                .is_ignored()
        );

        assert_eq!(
            false,
            ignores
                .check(MaybeIgnoredCellRelativePath::unchecked_new("third"))
                .is_ignored()
        );

        assert_eq!(
            false,
            ignores
                .check(MaybeIgnoredCellRelativePath::unchecked_new("one/two/three"))
                .is_ignored()
        );

        assert_eq!(
            true,
            ignores
                .check(MaybeIgnoredCellRelativePath::unchecked_new(
                    "recursive/two/three"
                ))
                .is_ignored()
        );

        assert_eq!(
            true,
            ignores
                .check(MaybeIgnoredCellRelativePath::unchecked_new(
                    "trailing_slash/BUCK"
                ))
                .is_ignored()
        );

        Ok(())
    }
}
